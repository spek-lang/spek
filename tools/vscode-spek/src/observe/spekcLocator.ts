import * as fs from 'fs';
import * as path from 'path';

/**
 * Resolves the `spekc` executable to spawn, in trust order:
 *
 *  1. The explicit `spek.spekc.path` setting (verbatim — the user's word).
 *  2. `spekc` on PATH (the post-install experience: `dotnet tool install`).
 *  3. A workspace-built CLI: any `Spek.Cli/bin/{Release or Debug}/<tfm>/spekc`
 *     apphost under a workspace folder — the dogfooding experience inside
 *     the Spek repo itself, where nothing is installed but the CLI builds
 *     to a well-known SDK path. Release preferred over Debug, then newest
 *     mtime; the search is shallow and targeted (no workspace-wide glob).
 *
 * Returns the path to spawn, or null when nothing resolves (the caller
 * keeps its existing "install or set spek.spekc.path" error).
 */
export function resolveSpekcPath(
  configuredPath: string | undefined,
  workspaceFolders: readonly string[],
  pathEnv: string | undefined,
  fsx: Pick<typeof fs, 'existsSync' | 'statSync'> = fs,
): string | null {
  if (configuredPath && configuredPath !== 'spekc') return configuredPath;

  // PATH probe: cheap existence check across PATH entries for the apphost.
  const exe = process.platform === 'win32' ? 'spekc.exe' : 'spekc';
  for (const dir of (pathEnv ?? '').split(path.delimiter)) {
    if (dir && fsx.existsSync(path.join(dir, exe))) return path.join(dir, exe);
  }

  // Workspace-built CLI. Well-known layout only: <ws>/**(1-2 levels)/Spek.Cli/bin/...
  const candidates: { p: string; release: boolean; mtime: number }[] = [];
  for (const ws of workspaceFolders) {
    for (const cliDir of [path.join(ws, 'src', 'Spek.Cli'), path.join(ws, 'Spek.Cli')]) {
      for (const cfg of ['Release', 'Debug'] as const) {
        const bin = path.join(cliDir, 'bin', cfg);
        if (!fsx.existsSync(bin)) continue;
        let tfms: string[] = [];
        try { tfms = fs.readdirSync(bin); } catch { continue; }
        for (const tfm of tfms) {
          const p = path.join(bin, tfm, exe);
          if (fsx.existsSync(p)) {
            let mtime = 0;
            try { mtime = fsx.statSync(p).mtimeMs; } catch { /* keep 0 */ }
            candidates.push({ p, release: cfg === 'Release', mtime });
          }
        }
      }
    }
  }
  candidates.sort((a, b) => (Number(b.release) - Number(a.release)) || (b.mtime - a.mtime));
  return candidates[0]?.p ?? null;
}
