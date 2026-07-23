// Pure logic behind "auto-open the actor tree while debugging": DAP message
// inspection, the auto-attach decision matrix, Spek-project evidence, and
// the frozen-sampler stall check. No 'vscode' imports — unit-testable.

import * as path from 'path';
import { ProjectFs, parseCsproj } from './projectResolver';

/**
 * Extract the debuggee's OS process id from one DAP protocol message.
 * The debug adapter announces it in the `process` event:
 *
 *   { "type": "event", "event": "process",
 *     "body": { "name": ..., "systemProcessId": 12345, ... } }
 *
 * Returns null for every other message (requests, responses, other events,
 * malformed bodies, pid 0/negative placeholders).
 */
export function extractDapProcessId(message: unknown): number | null {
  if (typeof message !== 'object' || message === null) return null;
  const msg = message as Record<string, unknown>;
  if (msg.type !== 'event' || msg.event !== 'process') return null;
  const body = msg.body;
  if (typeof body !== 'object' || body === null) return null;
  const pid = (body as Record<string, unknown>).systemProcessId;
  if (typeof pid !== 'number' || !Number.isInteger(pid) || pid <= 0) return null;
  return pid;
}

export type AutoAttachMode = 'always' | 'spekProjectsOnly' | 'never';

/**
 * The auto-attach decision matrix for a coreclr debug session that just
 * announced its process:
 *
 *   mode              | spek-launched | spek evidence | attach?
 *   ------------------|---------------|---------------|--------
 *   never             |       *       |       *       |   no
 *   always            |       *       |       *       |   yes
 *   spekProjectsOnly  |      yes      |       *       |   yes
 *   spekProjectsOnly  |      no       |      yes      |   yes
 *   spekProjectsOnly  |      no       |      no       |   no
 *
 * "spek-launched" means the session was started by this extension's own
 * run/debug commands (the launch config carries a marker). "evidence" is
 * the on-disk check in {@link hasSpekEvidence} for sessions launched from a
 * user's own launch.json.
 */
export function decideAutoAttach(
  mode: AutoAttachMode,
  spekLaunched: boolean,
  hasEvidence: () => boolean,
): boolean {
  switch (mode) {
    case 'never':
      return false;
    case 'always':
      return true;
    case 'spekProjectsOnly':
      return spekLaunched || hasEvidence();
    default:
      // An unknown setting value behaves like the default mode's safe side.
      return false;
  }
}

/** Does this directory's subtree (bounded) contain a .spek file? */
function subtreeHasSpek(dir: string, fs: ProjectFs, depth: number): boolean {
  if (fs.listDir(dir).some((n) => n.toLowerCase().endsWith('.spek'))) return true;
  if (depth <= 0) return false;
  for (const name of fs.listDir(dir)) {
    if (name.startsWith('.') || name === 'bin' || name === 'obj' || name === 'node_modules') continue;
    const child = path.join(dir, name);
    if (fs.isDirectory(child) && subtreeHasSpek(child, fs, depth - 1)) return true;
  }
  return false;
}

/**
 * Is the debugged program part of a Spek project? Walks up from the
 * program's directory (typically bin/Debug/<tfm>) to the nearest directory
 * holding a csproj, then answers yes when either
 *   - that project's subtree contains a .spek file, or
 *   - the csproj ProjectReferences (one hop) a project whose directory
 *     subtree contains a .spek file — the harness pattern, where the
 *     executable itself is pure C# and the Spek sources live in the
 *     referenced library (demos/elevators' Elevators.Harness).
 */
export function hasSpekEvidence(programOrCwd: string, fs: ProjectFs): boolean {
  let dir = fs.isDirectory(programOrCwd) ? programOrCwd : path.dirname(programOrCwd);
  let projectDir: string | null = null;
  for (let i = 0; i < 12 && dir !== path.dirname(dir); i++) {
    if (fs.listDir(dir).some((n) => n.toLowerCase().endsWith('.csproj'))) {
      projectDir = dir;
      break;
    }
    dir = path.dirname(dir);
  }
  if (!projectDir) return false;

  if (subtreeHasSpek(projectDir, fs, 3)) return true;

  for (const name of fs.listDir(projectDir)) {
    if (!name.toLowerCase().endsWith('.csproj')) continue;
    const content = fs.readFile(path.join(projectDir, name));
    if (!content) continue;
    const meta = parseCsproj(content, path.join(projectDir, name));
    for (const ref of meta.projectReferences) {
      if (subtreeHasSpek(path.dirname(ref), fs, 3)) return true;
    }
  }
  return false;
}

/**
 * The frozen-sampler check: while the debugger holds the debuggee at a
 * breakpoint, `spekc observe` stops receiving samples. A stall during an
 * active debug session is expected — the panel shows a calm banner, not an
 * error. Default threshold is a few missed ~1/s samples.
 */
export function isSampleStalled(
  nowMs: number,
  lastSampleAtMs: number | undefined,
  thresholdMs = 3000,
): boolean {
  if (lastSampleAtMs === undefined) return false; // nothing sampled yet
  return nowMs - lastSampleAtMs >= thresholdMs;
}

/**
 * Merges the diagnostics-port re-enable into a coreclr launch config's env.
 * The C# debugger launches targets with COMPlus_EnableDiagnostics=0, which
 * silently blinds every diagnostics-port client (spekc observe, the actor
 * panel, dotnet-counters). In Spek workspaces we re-enable it on EVERY
 * coreclr launch — ours, C# Dev Kit's dynamic config, or the user's own
 * launch.json — so attaching never depends on which door the user came
 * through. Explicit user-provided values always win (defaults first,
 * user's env spread last).
 */
export function withDiagnosticsEnabled(
  env: Record<string, unknown> | undefined,
): Record<string, unknown> {
  return {
    COMPlus_EnableDiagnostics: '1',
    DOTNET_EnableDiagnostics: '1',
    ...(env ?? {}),
  };
}
