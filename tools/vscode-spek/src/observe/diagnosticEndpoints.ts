// Pure pid extraction from .NET diagnostics IPC endpoint names.
// No 'vscode' imports — unit-testable.

/**
 * Extracts attachable .NET pids from a directory listing of diagnostics
 * IPC endpoints. The runtime creates `dotnet-diagnostic-<pid>-<time>-socket`
 * in the temp dir (Unix domain sockets) or `dotnet-diagnostic-<pid>` named
 * pipes on Windows — the same discovery `dotnet-counters ps` uses.
 */
export function pidsFromDiagnosticEndpoints(entries: string[]): number[] {
  const pids = new Set<number>();
  for (const entry of entries) {
    const m = /^dotnet-diagnostic-(\d+)(?:-|$)/.exec(entry);
    if (m) {
      const pid = Number(m[1]);
      if (Number.isInteger(pid) && pid > 0) pids.add(pid);
    }
  }
  return [...pids].sort((a, b) => a - b);
}

/**
 * The temp directories a .NET process might have written its diagnostics
 * socket into. macOS gives each launch context (Dock, terminal, a debugger)
 * its own per-session TMPDIR under /var/folders, so a process started by the
 * VS Code debugger can land its socket somewhere the extension host's own
 * os.tmpdir() isn't — the source of "verify TMPDIR is set to the same
 * directory" attach failures. Callers pass os.tmpdir() and process.env.TMPDIR;
 * we add /tmp and dedupe. Unix only; Windows named pipes are global.
 */
export function candidateDiagnosticDirs(osTmp: string, envTmp: string | undefined): string[] {
  const dirs: string[] = [];
  for (const d of [osTmp, envTmp, '/tmp']) {
    if (d) {
      const trimmed = d.replace(/\/+$/, '');
      if (!dirs.includes(trimmed)) dirs.push(trimmed);
    }
  }
  return dirs;
}

/**
 * Finds the directory holding a specific pid's diagnostics socket, so spekc
 * can be spawned with TMPDIR pointed at it. Returns the first candidate dir
 * whose listing contains a `dotnet-diagnostic-<pid>-...` entry, or null.
 * `listDir` returns a directory's entries (or [] if unreadable).
 */
export function diagnosticDirForPid(
  pid: number,
  candidateDirs: string[],
  listDir: (dir: string) => string[],
): string | null {
  for (const dir of candidateDirs) {
    if (pidsFromDiagnosticEndpoints(listDir(dir)).includes(pid)) return dir;
  }
  return null;
}
