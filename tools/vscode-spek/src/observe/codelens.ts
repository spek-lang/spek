import * as vscode from 'vscode';
import { ObserveSample, statsByType, TypeStats } from './model';
import { findActorDeclarations, lensTitle } from './actorDecl';

/**
 * Live CodeLens above `actor X` declarations while attached. The
 * provider is only registered for the duration of an observe session
 * (see ObserveController), so when nothing is attached there is no
 * provider at all — zero ambient cost. Refreshes ride the throttled
 * sample event the controller drives (~1/s).
 */
export class LiveActorCodeLensProvider implements vscode.CodeLensProvider {
  private readonly _onDidChangeCodeLenses = new vscode.EventEmitter<void>();
  readonly onDidChangeCodeLenses = this._onDidChangeCodeLenses.event;

  private stats: Map<string, TypeStats> = new Map();

  /** Called by the controller on each (throttled) sample. */
  updateFromSample(sample: ObserveSample): void {
    this.stats = statsByType(sample.actors);
    this._onDidChangeCodeLenses.fire();
  }

  provideCodeLenses(document: vscode.TextDocument): vscode.CodeLens[] {
    if (this.stats.size === 0) return [];
    const lenses: vscode.CodeLens[] = [];
    const text = document.getText();
    for (const decl of findActorDeclarations(text)) {
      const stats = this.stats.get(decl.name);
      if (!stats) continue;
      const position = document.positionAt(decl.index);
      const range = new vscode.Range(position, position);
      lenses.push(
        new vscode.CodeLens(range, {
          title: lensTitle(stats),
          command: 'spek.showActorPanel',
          tooltip: 'Live data from the attached Spek process — click to open the actor panel',
        }),
      );
    }
    return lenses;
  }

  dispose(): void {
    this._onDidChangeCodeLenses.dispose();
  }
}

/**
 * Best-effort jump from a live actor to its declaration: scans workspace
 * .spek files for `actor <TypeName>` and opens the first match. Build
 * output and dependency folders are excluded from the scan.
 */
export async function openActorDeclaration(actorType: string): Promise<void> {
  const bare = actorType.replace(/`\d+$/, '');
  const files = await vscode.workspace.findFiles(
    '**/*.spek',
    '**/{node_modules,bin,obj,out}/**',
    200,
  );
  for (const file of files) {
    let doc: vscode.TextDocument;
    try {
      doc = await vscode.workspace.openTextDocument(file);
    } catch {
      continue;
    }
    const re = new RegExp(
      `^[ \\t]*(?:(?:public|internal|protected|private)\\s+)?(?:abstract\\s+)?actor\\s+(${escapeRegExp(bare)})\\b`,
      'm',
    );
    const m = re.exec(doc.getText());
    if (m && m[1]) {
      const namePos = doc.positionAt(m.index + m[0].length - m[1].length);
      const editor = await vscode.window.showTextDocument(doc, { preview: true });
      editor.selection = new vscode.Selection(namePos, namePos.translate(0, m[1].length));
      editor.revealRange(
        new vscode.Range(namePos, namePos),
        vscode.TextEditorRevealType.InCenterIfOutsideViewport,
      );
      return;
    }
  }
  void vscode.window.showInformationMessage(
    `No declaration "actor ${bare}" found in the workspace's .spek files.`,
  );
}

function escapeRegExp(s: string): string {
  return s.replace(/[.*+?^${}()|[\]\\]/g, '\\$&');
}
