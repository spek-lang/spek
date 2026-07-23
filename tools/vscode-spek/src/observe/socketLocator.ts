import * as fs from 'fs';
import * as os from 'os';
import * as path from 'path';

/**
 * Finds the directory that actually holds a pid's .NET diagnostics socket,
 * so spekc can be spawned with a matching TMPDIR.
 *
 * The socket file's location is ground truth: a .NET process writes
 * `dotnet-diagnostic-<pid>-<time>-socket` into its TMPDIR, and TMPDIR varies
 * by launch context. A process started with TMPDIR unset (how the VS Code
 * debugger often launches a target) lands its socket in `/tmp`; one launched
 * inside a macOS per-session temp lands it under `/var/folders/<a>/<b>/T`.
 * Neither is necessarily the extension host's own os.tmpdir(), which is why
 * inheriting the environment fails with ".NET verify TMPDIR is set to the
 * same directory".
 *
 * Everything is `fs.readdirSync` (which follows the `/tmp` → `/private/tmp`
 * symlink; shell `find` does not, and silently returns nothing). Returns the
 * directory, or null when no socket for the pid is found — in which case the
 * caller inherits the environment unchanged, exactly as before this existed.
 *
 * `deps` is injectable for tests; production uses the real fs.
 */
export interface SocketLocatorDeps {
  readonly listDir: (dir: string) => string[];
  readonly osTmp: string;
  readonly envTmp: string | undefined;
  /** macOS per-session temp roots to scan (/var/folders/<a>/<b>/T). */
  readonly sessionTempDirs: () => string[];
}

export function locateSocketDir(pid: number, deps: SocketLocatorDeps): string | null {
  const prefix = `dotnet-diagnostic-${pid}-`;
  const has = (dir: string): boolean => deps.listDir(dir).some((e) => e.startsWith(prefix));

  const primary = [deps.osTmp, deps.envTmp, '/tmp', '/private/tmp']
    .filter((d): d is string => !!d)
    .map((d) => d.replace(/\/+$/, ''));
  const seen = new Set<string>();
  for (const dir of [...primary, ...deps.sessionTempDirs()]) {
    if (seen.has(dir)) continue;
    seen.add(dir);
    if (has(dir)) return dir;
  }
  return null;
}

/** Enumerates `/var/folders/<a>/<b>/T` — macOS per-launch-session temp dirs. */
export function macSessionTempDirs(listDir: (dir: string) => string[]): string[] {
  if (process.platform !== 'darwin') return [];
  const dirs: string[] = [];
  for (const a of listDir('/var/folders')) {
    for (const b of listDir(path.join('/var/folders', a))) {
      dirs.push(path.join('/var/folders', a, b, 'T'));
    }
  }
  return dirs;
}

/** Production locator over the real filesystem. Best effort — null on miss. */
export function locateSocketDirOnDisk(pid: number): string | null {
  if (process.platform === 'win32') return null;
  const listDir = (dir: string): string[] => {
    try {
      return fs.readdirSync(dir);
    } catch {
      return [];
    }
  };
  return locateSocketDir(pid, {
    listDir,
    osTmp: os.tmpdir(),
    envTmp: process.env.TMPDIR,
    sessionTempDirs: () => macSessionTempDirs(listDir),
  });
}
