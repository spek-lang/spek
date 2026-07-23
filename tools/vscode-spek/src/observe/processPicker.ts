import * as vscode from 'vscode';
import * as fs from 'fs';
import * as os from 'os';
import { execFile } from 'child_process';
import { candidateDiagnosticDirs, pidsFromDiagnosticEndpoints } from './diagnosticEndpoints';

function listDiagnosticPids(): number[] {
  if (process.platform === 'win32') {
    try { return pidsFromDiagnosticEndpoints(fs.readdirSync('\\\\.\\pipe\\')); }
    catch { return []; }
  }
  // Union across every candidate temp dir — a debugger-launched target may
  // have written its socket outside the extension host's own os.tmpdir().
  const pids = new Set<number>();
  for (const dir of candidateDiagnosticDirs(os.tmpdir(), process.env.TMPDIR)) {
    try { for (const p of pidsFromDiagnosticEndpoints(fs.readdirSync(dir))) pids.add(p); }
    catch { /* unreadable dir — skip */ }
  }
  return [...pids].sort((a, b) => a - b);
}

/** Resolves each pid's command line (best effort — missing pids drop out). */
async function describePids(pids: number[]): Promise<Map<number, string>> {
  const result = new Map<number, string>();
  if (pids.length === 0 || process.platform === 'win32') return result;
  const stdout = await new Promise<string>((resolve) => {
    execFile(
      'ps',
      ['-o', 'pid=,args=', '-p', pids.join(',')],
      { timeout: 3000 },
      (_err, out) => resolve(out ?? ''),
    );
  });
  for (const line of stdout.split('\n')) {
    const m = /^\s*(\d+)\s+(.*)$/.exec(line);
    if (m) result.set(Number(m[1]), m[2].trim());
  }
  return result;
}

/**
 * Quickpick over attachable dotnet processes (diagnostics-endpoint
 * enumeration), with a manual-entry escape hatch. Returns the chosen
 * pid, or undefined when cancelled. The current process is excluded —
 * attaching the editor to itself is never what anyone meant.
 */
export async function pickDotnetProcess(): Promise<number | undefined> {
  const pids = listDiagnosticPids().filter((p) => p !== process.pid);
  const descriptions = await describePids(pids);

  type PidItem = vscode.QuickPickItem & { pid?: number };
  const items: PidItem[] = pids.map((pid) => {
    const args = descriptions.get(pid);
    const label = args ? args.split(/[\\/\s]/).find((s) => s.length > 0) ?? 'dotnet' : 'dotnet process';
    return {
      pid,
      label: `$(server-process) ${pid}`,
      description: label,
      detail: args && args.length > 100 ? args.slice(0, 100) + '…' : args,
    };
  });
  items.push({ label: '$(edit) Enter PID manually…' });

  const picked = await vscode.window.showQuickPick(items, {
    title: 'Spek: Attach to Process',
    placeHolder:
      pids.length > 0
        ? 'Pick a .NET process to observe'
        : 'No attachable .NET processes found — enter a PID manually',
    matchOnDescription: true,
    matchOnDetail: true,
  });
  if (!picked) return undefined;
  if (picked.pid !== undefined) return picked.pid;

  const input = await vscode.window.showInputBox({
    title: 'Spek: Attach to Process',
    prompt: 'Process id of the running Spek application',
    validateInput: (v) => (/^\d+$/.test(v.trim()) ? undefined : 'Enter a numeric process id'),
  });
  if (!input) return undefined;
  return Number(input.trim());
}
