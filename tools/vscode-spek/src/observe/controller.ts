import * as vscode from 'vscode';
import { ObserveSample } from './model';
import { ObserveSession } from './session';
import { ActorPanel } from './panel';
import { LiveActorCodeLensProvider, openActorDeclaration } from './codelens';
import { pickDotnetProcess } from './processPicker';
import { isSampleStalled } from '../run/debugDecision';
import { resolveSpekcPath } from './spekcLocator';

const ATTACHED_CONTEXT = 'spek.observe.attached';

/** How an attach came about; debug attaches get gentler end-of-life UX. */
export interface AttachOptions {
  /**
   * The attach was made automatically for a debug session. Consequences:
   * the observe child ending shows a banner in the panel instead of a
   * warning toast (the debuggee stopping is the normal way these sessions
   * end), and a sample stall — the debuggee's sampler can't run while the
   * debugger holds it at a breakpoint — shows a calm "frozen" notice.
   */
  viaDebug?: boolean;
}

/**
 * Owns one attach lifecycle: the observe child process, the actor
 * panel, the status bar item, and the (session-scoped) CodeLens
 * provider. Closing the panel kills the child; the child dying keeps
 * the panel open (banner + last tree) but tears the session down.
 */
export class ObserveController implements vscode.Disposable {
  private session: ObserveSession | undefined;
  private panel: ActorPanel | undefined;
  private statusBar: vscode.StatusBarItem | undefined;
  private codeLensProvider: LiveActorCodeLensProvider | undefined;
  // Disposables tied to the live child process (sample/exit listeners,
  // CodeLens registration) — dropped as soon as the session ends.
  private sessionDisposables: vscode.Disposable[] = [];
  // Disposables tied to the panel's lifetime (close hook, click hook) —
  // they outlive the session so a closed panel still cleans everything up.
  private panelDisposables: vscode.Disposable[] = [];

  // Sample-driven UI refresh is throttled to ~1/s: a sample that arrives
  // inside the window is remembered and flushed when the window closes,
  // so the UI always converges on the latest data.
  private static readonly REFRESH_MS = 1000;
  private lastRefresh = 0;
  private pendingSample: ObserveSample | undefined;
  private pendingTimer: NodeJS.Timeout | undefined;

  // Debug-attach state: while a debugger holds the debuggee at a breakpoint
  // its sampler freezes, so stalled samples are expected — a watchdog swaps
  // the panel's data-flow silence for an explanatory notice.
  private static readonly STALL_MS = 3000;
  private viaDebug = false;
  private lastSampleAt: number | undefined;
  private stallTimer: NodeJS.Timeout | undefined;
  private stallNoticeShowing = false;

  constructor(
    /** Panel tab icon — the same mark as the .spek file icon. */
    private readonly panelIcon?: { light: vscode.Uri; dark: vscode.Uri },
  ) {}

  get attached(): boolean {
    return this.session !== undefined;
  }

  async attachCommand(): Promise<void> {
    if (this.attached) {
      const choice = await vscode.window.showWarningMessage(
        'Already attached to a Spek process. Detach first?',
        'Detach and Attach',
        'Cancel',
      );
      if (choice !== 'Detach and Attach') return;
    }

    const pid = await pickDotnetProcess();
    if (pid === undefined) return;
    this.attach(pid);
  }

  attach(pid: number, options?: AttachOptions): void {
    this.detach(); // clear any previous session, panel, or status bar

    this.viaDebug = options?.viaDebug === true;
    this.lastSampleAt = undefined;
    this.stallNoticeShowing = false;

    const config = vscode.workspace.getConfiguration('spek');
    const spekcPath = resolveSpekcPath(
      config.get<string>('spekc.path'),
      (vscode.workspace.workspaceFolders ?? []).map(f => f.uri.fsPath),
      process.env.PATH,
    ) ?? 'spekc';   // let spawn fail with the existing guidance message

    const session = new ObserveSession(spekcPath, pid);
    this.session = session;

    const panel = new ActorPanel(pid, this.panelIcon);
    this.panel = panel;
    this.panelDisposables.push(
      panel.onDidDispose(() => this.detach()),
      panel.onOpenActor((actorType) => void openActorDeclaration(actorType)),
    );

    const statusBar = vscode.window.createStatusBarItem(vscode.StatusBarAlignment.Left, 90);
    statusBar.text = `$(pulse) Spek: attaching to ${pid}…`;
    statusBar.tooltip = 'Spek live introspection — click to show the actor panel';
    statusBar.command = 'spek.showActorPanel';
    statusBar.show();
    this.statusBar = statusBar;

    const codeLensProvider = new LiveActorCodeLensProvider();
    this.codeLensProvider = codeLensProvider;

    this.sessionDisposables.push(
      vscode.languages.registerCodeLensProvider({ language: 'spek' }, codeLensProvider),
      session.onSample((sample) => this.scheduleRefresh(sample)),
      session.onExit(({ code, stderr }) => this.handleExit(pid, spekcPath, code, stderr)),
    );

    void vscode.commands.executeCommand('setContext', ATTACHED_CONTEXT, true);
    if (this.viaDebug) {
      this.stallTimer = setInterval(() => this.checkStall(), 1000);
    }
    session.start();
  }

  /** While debug-attached: stalled samples mean "paused", not "broken". */
  private checkStall(): void {
    if (!this.session || this.stallNoticeShowing) return;
    if (isSampleStalled(Date.now(), this.lastSampleAt, ObserveController.STALL_MS)) {
      this.stallNoticeShowing = true;
      this.panel?.showNotice('paused with debugger — tree frozen at last sample');
    }
  }

  private scheduleRefresh(sample: ObserveSample): void {
    this.lastSampleAt = Date.now();
    if (this.stallNoticeShowing) {
      this.stallNoticeShowing = false;
      this.panel?.clearNotice();
    }
    this.pendingSample = sample;
    const now = Date.now();
    const due = this.lastRefresh + ObserveController.REFRESH_MS;
    if (now >= due) {
      this.flushRefresh();
    } else if (this.pendingTimer === undefined) {
      this.pendingTimer = setTimeout(() => this.flushRefresh(), due - now);
    }
  }

  private flushRefresh(): void {
    if (this.pendingTimer !== undefined) {
      clearTimeout(this.pendingTimer);
      this.pendingTimer = undefined;
    }
    const sample = this.pendingSample;
    this.pendingSample = undefined;
    if (!sample || !this.session) return;
    this.lastRefresh = Date.now();

    const warnDepth = vscode.workspace
      .getConfiguration('spek')
      .get<number>('observe.mailboxWarningDepth', 10);

    this.panel?.update(sample, warnDepth);
    this.codeLensProvider?.updateFromSample(sample);
    if (this.statusBar) {
      const live = sample.actors.filter((a) => !a.isStopped).length;
      this.statusBar.text = `$(pulse) Spek: ${sample.system} (${live} live)`;
      this.statusBar.tooltip =
        `Spek live introspection — ${sample.system}, pid ${this.session.pid}, ` +
        `${sample.actors.length} actors sampled. Click to show the actor panel.`;
    }
  }

  private handleExit(pid: number, spekcPath: string, code: number | null, stderr: string): void {
    if (this.viaDebug && /Unable to connect to Process/i.test(stderr)) {
      stderr +=
        '\n\nLikely cause: this process was launched by a debugger that disables the ' +
        '.NET diagnostics port (COMPlus_EnableDiagnostics=0), so no socket exists to ' +
        'attach to. Launch with "Spek: Debug Project of This File" / F5 on a .spek file ' +
        '(which re-enables it), or add COMPlus_EnableDiagnostics=1 to the "env" of your ' +
        'launch.json configuration.';
    }
    if (!this.session) return; // already detached deliberately
    const detail = stderr.length > 0 ? ` — ${stderr}` : '';
    const message =
      code === null && stderr.includes('ENOENT')
        ? `Couldn't run "${spekcPath}". Install the Spek CLI or set "spek.spekc.path".`
        : `spekc observe (pid ${pid}) ended${code !== null ? ` with exit code ${code}` : ''}${detail}`;
    this.panel?.showEnded(message);
    if (this.statusBar) this.statusBar.text = `$(debug-disconnect) Spek: detached`;
    // A debug-driven attach ends whenever the debuggee stops — that's normal
    // life-cycle, so the panel banner alone carries the news; only manual
    // attaches escalate to a toast.
    if (!this.viaDebug) void vscode.window.showWarningMessage(message);
    // Keep the panel open with its banner and last tree; end the session.
    // Closing the panel later still runs detach() via panelDisposables.
    this.teardownSession();
  }

  /**
   * A debug session this controller was auto-attached to has terminated:
   * end the observe session but keep the panel showing its last tree, with
   * a note explaining why it's no longer live. No-op when already detached.
   */
  endDebugSession(note: string): void {
    if (this.session) {
      this.teardownSession();
      this.panel?.showEnded(note);
      if (this.statusBar) this.statusBar.text = `$(debug-disconnect) Spek: debug ended`;
    } else if (this.panel && this.viaDebug) {
      // The observe child already exited (its banner is up); refresh the note.
      this.panel.showEnded(note);
    }
  }

  showPanel(): void {
    if (this.panel) {
      this.panel.reveal();
    } else {
      void vscode.window.showInformationMessage(
        'No Spek process attached. Run "Spek: Attach to Process" first.',
      );
    }
  }

  /** Ends the child process + live UI wiring; leaves panel and status bar. */
  private teardownSession(): void {
    if (this.stallTimer !== undefined) {
      clearInterval(this.stallTimer);
      this.stallTimer = undefined;
    }
    this.stallNoticeShowing = false;
    if (this.pendingTimer !== undefined) {
      clearTimeout(this.pendingTimer);
      this.pendingTimer = undefined;
    }
    this.pendingSample = undefined;
    for (const d of this.sessionDisposables) d.dispose();
    this.sessionDisposables = [];
    this.session?.dispose();
    this.session = undefined;
    this.codeLensProvider?.dispose(); // registration itself was just disposed above
    this.codeLensProvider = undefined;
    void vscode.commands.executeCommand('setContext', ATTACHED_CONTEXT, false);
  }

  detach(): void {
    this.teardownSession();
    for (const d of this.panelDisposables) d.dispose();
    this.panelDisposables = [];
    this.statusBar?.dispose();
    this.statusBar = undefined;
    const panel = this.panel;
    this.panel = undefined;
    panel?.dispose();
  }

  dispose(): void {
    this.detach();
  }
}
