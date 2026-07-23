import * as vscode from 'vscode';
import * as fs from 'fs';
import { ErrorCatalog, ErrorEntry } from '../tools/catalogParser';
import { ceCodesForDiagnostic } from './ceCode';

const SCHEME = 'spek-explain';

/**
 * CE-code explanations, rustc --explain style. The full teaching text
 * for every code ships inside the extension (errors/catalog.json,
 * generated at build time from the error-codes reference), so this
 * works offline and needs neither the compiler nor the docs site.
 *
 * Three surfaces:
 *  - hovering a Spek diagnostic adds an "Explain CE####" link,
 *  - the "Spek: Explain Error Code" command quick-picks over all codes,
 *  - both render through the built-in markdown preview via a virtual
 *    document (spek-explain:/CE####.md).
 */
export function registerCeExplain(context: vscode.ExtensionContext): void {
  const catalog = new LazyCatalog(context.asAbsolutePath('errors/catalog.json'));

  const contentProvider: vscode.TextDocumentContentProvider = {
    provideTextDocumentContent(uri: vscode.Uri): string {
      const code = uri.path.replace(/^\//, '').replace(/\.md$/, '');
      return renderEntry(code, catalog.get(code));
    },
  };
  context.subscriptions.push(
    vscode.workspace.registerTextDocumentContentProvider(SCHEME, contentProvider),
  );

  context.subscriptions.push(
    vscode.commands.registerCommand('spek.explainErrorCode', async (code?: string) => {
      let target = typeof code === 'string' ? code.trim().toUpperCase() : undefined;
      if (!target || !/^CE\d{4}$/.test(target)) {
        target = await pickCode(catalog);
        if (!target) return;
      }
      await showExplanation(target);
    }),
  );

  context.subscriptions.push(
    vscode.languages.registerHoverProvider({ language: 'spek' }, {
      provideHover(document, position): vscode.Hover | undefined {
        return explainHover(document, position, catalog);
      },
    }),
  );
}

/** Loads the bundled catalog on first use; a missing/corrupt file degrades to empty. */
class LazyCatalog {
  private loaded: ErrorCatalog | undefined;

  constructor(private readonly filePath: string) {}

  private load(): ErrorCatalog {
    if (!this.loaded) {
      try {
        const raw = JSON.parse(fs.readFileSync(this.filePath, 'utf8'));
        this.loaded =
          raw && typeof raw === 'object' && raw.codes && typeof raw.codes === 'object'
            ? { codes: raw.codes }
            : { codes: {} };
      } catch {
        this.loaded = { codes: {} };
      }
    }
    return this.loaded;
  }

  get(code: string): ErrorEntry | undefined {
    return this.load().codes[code];
  }

  all(): ErrorEntry[] {
    return Object.values(this.load().codes).sort((a, b) => a.code.localeCompare(b.code));
  }
}

async function pickCode(catalog: LazyCatalog): Promise<string | undefined> {
  const entries = catalog.all();
  if (entries.length === 0) {
    void vscode.window.showWarningMessage(
      'The bundled Spek error-code catalog is missing or empty — reinstall the extension.',
    );
    return undefined;
  }
  const picked = await vscode.window.showQuickPick(
    entries.map((e) => ({
      label: e.code,
      description: e.status ?? undefined,
      detail: e.summary ?? undefined,
    })),
    {
      title: 'Spek: Explain Error Code',
      placeHolder: 'Pick a CE code to read its full explanation',
      matchOnDetail: true,
    },
  );
  return picked?.label;
}

async function showExplanation(code: string): Promise<void> {
  const uri = vscode.Uri.parse(`${SCHEME}:/${code}.md`);
  await vscode.commands.executeCommand('markdown.showPreviewToSide', uri);
}

function renderEntry(code: string, entry: ErrorEntry | undefined): string {
  if (!entry) {
    return (
      `# ${code}\n\n` +
      `No explanation for \`${code}\` is bundled with this version of the extension. ` +
      `It may be newer than the extension's error-code catalog — check the Spek ` +
      `documentation's error reference, or update the extension.\n`
    );
  }
  const title = entry.summary ? `# ${entry.code} — ${entry.summary}` : `# ${entry.code}`;
  const status = entry.status ? `\n\n**Status:** ${entry.status}` : '';
  const body = entry.body.length > 0
    ? `\n\n${entry.body}\n`
    : `\n\nThis code is ${entry.status ?? 'reserved'} and has no expanded explanation yet.\n`;
  return `${title}${status}${body}`;
}

function explainHover(
  document: vscode.TextDocument,
  position: vscode.Position,
  catalog: LazyCatalog,
): vscode.Hover | undefined {
  const diagnostics = vscode.languages
    .getDiagnostics(document.uri)
    .filter((d) => d.range.contains(position));
  if (diagnostics.length === 0) return undefined;

  const codes: string[] = [];
  for (const d of diagnostics) {
    for (const code of ceCodesForDiagnostic(d.code, d.message)) {
      if (!codes.includes(code)) codes.push(code);
    }
  }
  if (codes.length === 0) return undefined;

  const md = new vscode.MarkdownString();
  md.isTrusted = { enabledCommands: ['spek.explainErrorCode'] };
  md.supportThemeIcons = true;
  for (const code of codes) {
    const summary = catalog.get(code)?.summary;
    md.appendMarkdown(
      `$(book) [Explain ${code}](command:spek.explainErrorCode?${encodeURIComponent(
        JSON.stringify([code]),
      )} "Open the full explanation for ${code}")` +
        (summary ? ` — ${summary}` : '') +
        '\n\n',
    );
  }
  return new vscode.Hover(md, diagnostics[0].range);
}
