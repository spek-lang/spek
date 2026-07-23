import * as vscode from 'vscode';
import { ChildProcess, spawn } from 'child_process';
import { locateSocketDirOnDisk } from './socketLocator';
import { NdjsonLineBuffer, ObserveSample, parseErrorLine, parseSampleLine } from './model';

/**
 * One attached `spekc observe <pid> --json` child process. Owns the
 * process, decodes its NDJSON stdout into samples, and reports exit.
 * Disposing the session kills the child — closing the panel detaches.
 */
export class ObserveSession implements vscode.Disposable {
  private readonly _onSample = new vscode.EventEmitter<ObserveSample>();
  private readonly _onExit = new vscode.EventEmitter<{ code: number | null; stderr: string }>();

  /** Fires once per decoded sample line. */
  readonly onSample = this._onSample.event;
  /** Fires when the child exits (or fails to spawn). */
  readonly onExit = this._onExit.event;

  private child: ChildProcess | undefined;
  private stderrTail = '';
  private malformedLines = 0;
  private cliError: string | undefined;
  private sawSample = false;
  private disposed = false;

  /** The most recent sample, for late subscribers (panel opened after data). */
  latest: ObserveSample | undefined;

  constructor(
    readonly spekcPath: string,
    readonly pid: number,
  ) {}

  start(): void {
    const lineBuffer = new NdjsonLineBuffer();

    let child: ChildProcess;
    try {
      // The socket FILE is ground truth: find the temp dir actually holding
      // this pid's diagnostics socket (covers /tmp for TMPDIR-unset targets
      // and macOS per-session temps) and point spekc's TMPDIR there. When no
      // socket is found the environment is inherited unchanged — the
      // original behavior. NOTE: if the target was launched by the C#
      // debugger with diagnostics disabled, no socket exists anywhere and
      // the attach legitimately cannot work (see controller's guidance).
      const socketDir = locateSocketDirOnDisk(this.pid);
      const env = socketDir ? { ...process.env, TMPDIR: socketDir } : process.env;
      child = spawn(this.spekcPath, ['observe', String(this.pid), '--json'], {
        stdio: ['ignore', 'pipe', 'pipe'],
        env,
      });
    } catch (err) {
      this._onExit.fire({ code: null, stderr: String(err) });
      return;
    }
    this.child = child;

    child.on('error', (err) => {
      // Typically ENOENT: spekc not on PATH.
      this._onExit.fire({ code: null, stderr: `${err.message}` });
    });

    child.stdout?.setEncoding('utf8');
    child.stdout?.on('data', (chunk: string) => {
      for (const line of lineBuffer.feed(chunk)) this.handleLine(line);
    });

    child.stderr?.setEncoding('utf8');
    child.stderr?.on('data', (chunk: string) => {
      this.stderrTail = (this.stderrTail + chunk).slice(-4000);
    });

    child.on('exit', (code) => {
      const rest = lineBuffer.flush();
      if (rest) this.handleLine(rest);
      if (!this.disposed) {
        let stderr = this.stderrTail.trim();
        if (this.cliError) {
          stderr = this.cliError;   // spekc's own words beat any inference
        } else if (!this.sawSample && this.malformedLines > 0 && stderr.length === 0) {
          stderr =
            'the observe stream produced no JSON samples — this spekc build may not support --json yet';
        }
        this._onExit.fire({ code, stderr });
      }
    });
  }

  private handleLine(line: string): void {
    const sample = parseSampleLine(line);
    if (sample) {
      this.sawSample = true;
      this.latest = sample;
      this._onSample.fire(sample);
      return;
    }
    // spekc reports its own failures as {"error": "..."} on stdout —
    // that message IS the diagnosis; never bury it under a guess.
    const err = parseErrorLine(line);
    if (err) {
      this.cliError = err;
    } else if (line.trim().length > 0) {
      this.malformedLines++;
    }
  }

  dispose(): void {
    if (this.disposed) return;
    this.disposed = true;
    try {
      this.child?.kill();
    } catch {
      // Already gone.
    }
    this.child = undefined;
    this._onSample.dispose();
    this._onExit.dispose();
  }
}
