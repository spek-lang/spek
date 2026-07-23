// Auto-open the live actor tree while debugging.
//
// Mechanism: a DebugAdapterTrackerFactory on `coreclr` sessions reads the
// DAP message stream from the adapter. The `process` event carries the
// debuggee's OS pid (body.systemProcessId); on seeing it, the
// `spek.observe.autoAttachOnDebug` setting decides whether to start the
// existing observe session on that pid — which opens the live actor panel
// beside the editor. When the debug session terminates, the observe session
// detaches but the panel keeps its last tree with an explanatory note.
//
// The decision logic itself (decideAutoAttach + hasSpekEvidence +
// extractDapProcessId) is pure and lives in ../run/debugDecision.ts.

import * as vscode from 'vscode';
import { ObserveController } from './controller';
import { AutoAttachMode, decideAutoAttach, extractDapProcessId, hasSpekEvidence } from '../run/debugDecision';
import { SPEK_LAUNCH_MARKER } from '../run/runDebug';
import { nodeFs } from '../run/nodeFs';

export function registerDebugAutoAttach(
  context: vscode.ExtensionContext,
  observe: ObserveController,
): void {
  // The one debug session we auto-attached for (if any). Only its
  // termination winds the observe session down.
  let trackedSessionId: string | undefined;

  context.subscriptions.push(
    vscode.debug.registerDebugAdapterTrackerFactory('coreclr', {
      createDebugAdapterTracker(session): vscode.DebugAdapterTracker {
        return {
          onDidSendMessage(message: unknown): void {
            const pid = extractDapProcessId(message);
            if (pid !== null) maybeAttach(session, pid);
          },
        };
      },
    }),
    vscode.debug.onDidTerminateDebugSession((session) => {
      if (session.id === trackedSessionId) {
        trackedSessionId = undefined;
        observe.endDebugSession(
          'debug session ended — the actor tree shows its last sample',
        );
      }
    }),
  );

  function maybeAttach(session: vscode.DebugSession, pid: number): void {
    const mode = vscode.workspace
      .getConfiguration('spek')
      .get<AutoAttachMode>('observe.autoAttachOnDebug', 'spekProjectsOnly');

    // Sessions launched by this extension's own run/debug commands carry a
    // marker; anything else (a user's launch.json) is probed on disk.
    const spekLaunched = session.configuration[SPEK_LAUNCH_MARKER] === true;
    const evidence = (): boolean => {
      const cfg = session.configuration;
      const target =
        (typeof cfg.program === 'string' && cfg.program) ||
        (typeof cfg.cwd === 'string' && cfg.cwd) ||
        undefined;
      return target !== undefined && hasSpekEvidence(target, nodeFs);
    };

    if (!decideAutoAttach(mode, spekLaunched, evidence)) return;
    if (observe.attached) return; // never clobber an attach the user made

    trackedSessionId = session.id;
    observe.attach(pid, { viaDebug: true }); // opens the panel beside the editor
  }
}
