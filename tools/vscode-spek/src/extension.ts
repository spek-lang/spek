import * as vscode from 'vscode';
import * as path from 'path';
import * as fs from 'fs';
import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
  TransportKind,
} from 'vscode-languageclient/node';
import { ObserveController } from './observe/controller';
import { registerDebugAutoAttach } from './observe/debugAutoAttach';
import { registerRunDebug } from './run/runDebug';
import { registerCeExplain } from './errors/explain';

let client: LanguageClient | undefined;

export async function activate(context: vscode.ExtensionContext): Promise<void> {
  // `spek.runTest` backs the "▶ Run test" code lenses the language server emits
  // on `…Tests` containers and their test methods. It's registered up front so it
  // works whether or not the language server itself starts.
  context.subscriptions.push(
    vscode.commands.registerCommand('spek.runTest', (filter: string, filePath: string) =>
      runDotnetTest(filter, filePath),
    ),
  );

  // Experimental peek at the transpiler output. Off by default (see the
  // `spek.experimental.showEmittedCSharp` setting) so it stays a diagnostic aid,
  // not a habit — Spek's premise is that you shouldn't need to read the C#.
  context.subscriptions.push(
    vscode.commands.registerCommand('spek.showEmittedCSharp', () => showEmittedCSharp()),
  );

  // Live actor introspection: "Spek: Attach to Process" spawns
  // `spekc observe <pid> --json` and drives the actor panel, the status
  // bar item, and per-session live CodeLens. All of it detaches when the
  // panel closes or the command runs again. The panel tab wears the same
  // mark as the .spek file icon.
  const observe = new ObserveController({
    light: vscode.Uri.joinPath(context.extensionUri, 'icon.png'),
    dark: vscode.Uri.joinPath(context.extensionUri, 'icon.png'),
  });
  context.subscriptions.push(
    observe,
    vscode.commands.registerCommand('spek.attachToProcess', () => observe.attachCommand()),
    vscode.commands.registerCommand('spek.detachFromProcess', () => observe.detach()),
    vscode.commands.registerCommand('spek.showActorPanel', () => observe.showPanel()),
  );

  // Run/debug the project of the active .spek file: F5 (via the `spek`
  // debug-configuration provider) plus the two explicit commands.
  registerRunDebug(context);

  // Actor tree auto-opens while debugging: a DAP tracker on coreclr
  // sessions reads the debuggee pid and attaches the observe session.
  registerDebugAutoAttach(context, observe);

  // CE-code explanations: hover links on diagnostics plus the
  // "Spek: Explain Error Code" command, backed by the bundled catalog.
  registerCeExplain(context);

  const config = vscode.workspace.getConfiguration('spek');
  const enabled: boolean = config.get('languageServer.enabled', true);
  if (!enabled) {
    return;
  }

  // Activation can now also happen via debugging (onDebug) in workspaces
  // that contain no Spek at all — starting spek-lsp there would only produce
  // a "server didn't start" toast. Start the language server when a .spek
  // document is already open or the workspace has .spek files; otherwise
  // start it lazily the first time a .spek document opens.
  if (!(await workspaceHasSpek())) {
    const lazy = vscode.workspace.onDidOpenTextDocument(async (doc) => {
      if (doc.languageId === 'spek' && !client) {
        lazy.dispose();
        await startLanguageServer();
      }
    });
    context.subscriptions.push(lazy);
    return;
  }

  await startLanguageServer();
}

async function workspaceHasSpek(): Promise<boolean> {
  if (vscode.workspace.textDocuments.some((d) => d.languageId === 'spek')) return true;
  if (vscode.window.activeTextEditor?.document.languageId === 'spek') return true;
  try {
    const hits = await vscode.workspace.findFiles(
      '**/*.spek',
      '**/{node_modules,bin,obj,out}/**',
      1,
    );
    return hits.length > 0;
  } catch {
    return false;
  }
}

async function startLanguageServer(): Promise<void> {
  const config = vscode.workspace.getConfiguration('spek');

  const serverPath: string = config.get('languageServer.path', 'spek-lsp');

  const serverOptions: ServerOptions = {
    run:   { command: serverPath, transport: TransportKind.stdio },
    debug: { command: serverPath, transport: TransportKind.stdio },
  };

  const clientOptions: LanguageClientOptions = {
    documentSelector: [{ scheme: 'file', language: 'spek' }],
    synchronize: {
      fileEvents: vscode.workspace.createFileSystemWatcher('**/*.spek'),
    },
    outputChannelName: 'Spek Language Server',
  };

  client = new LanguageClient(
    'spekLanguageServer',
    'Spek Language Server',
    serverOptions,
    clientOptions,
  );

  try {
    await client.start();
  } catch (err) {
    const msg =
      `Spek language server "${serverPath}" didn't start. Install it with ` +
      `"dotnet tool install --global Spek.LanguageServer.Tool", or set ` +
      `"spek.languageServer.path" to a locally built spek-lsp. ` +
      `Details: ${err}`;
    vscode.window.showWarningMessage(msg);
  }
}

// Run `dotnet test` filtered to a single test (or a whole `…Tests` container),
// in a terminal rooted at the enclosing project so the right assembly is built.
function runDotnetTest(filter: string, filePath: string): void {
  const cwd = projectDirFor(filePath);
  const terminal = vscode.window.createTerminal({ name: 'Spek Tests', cwd });
  // The filter is a code-derived FQN fragment; strip quotes defensively.
  const safeFilter = (filter ?? '').replace(/"/g, '');
  terminal.show(true);
  terminal.sendText(`dotnet test --filter "FullyQualifiedName~${safeFilter}"`);
}

// Nearest ancestor directory of `filePath` that holds a .csproj, so `dotnet test`
// runs against that project; falls back to the file's workspace folder.
function projectDirFor(filePath: string): string | undefined {
  if (filePath) {
    let dir = path.dirname(filePath);
    while (dir && dir !== path.dirname(dir)) {
      try {
        if (fs.readdirSync(dir).some((f) => f.endsWith('.csproj'))) {
          return dir;
        }
      } catch {
        // Unreadable directory — stop walking up.
        break;
      }
      dir = path.dirname(dir);
    }
    return vscode.workspace.getWorkspaceFolder(vscode.Uri.file(filePath))?.uri.fsPath;
  }
  return vscode.workspace.workspaceFolders?.[0]?.uri.fsPath;
}

// Ask the language server for the C# it would generate for the active .spek file
// and open it beside the source. Gated by the experimental setting; the language
// server must be running (it holds the parsed document and the emitter).
async function showEmittedCSharp(): Promise<void> {
  const config = vscode.workspace.getConfiguration('spek');
  if (!config.get<boolean>('experimental.showEmittedCSharp', false)) {
    vscode.window.showInformationMessage(
      'Enable "spek.experimental.showEmittedCSharp" in settings to use this experimental command.',
    );
    return;
  }

  const editor = vscode.window.activeTextEditor;
  if (!editor || editor.document.languageId !== 'spek') {
    vscode.window.showInformationMessage('Open a .spek file to see its emitted C#.');
    return;
  }
  const activeClient = client;
  if (!activeClient) {
    vscode.window.showWarningMessage(
      "The Spek language server isn't running, so the emitted C# is unavailable.",
    );
    return;
  }

  let result: any;
  try {
    result = await activeClient.sendRequest('spek/emitCSharp', {
      uri: editor.document.uri.toString(),
    });
  } catch (err) {
    vscode.window.showErrorMessage(`Couldn't emit C#: ${err}`);
    return;
  }

  // Be tolerant of the server's JSON casing (cSharp / csharp / CSharp).
  const csharp: string | undefined = result?.csharp ?? result?.cSharp ?? result?.CSharp;
  const error: string | undefined = result?.error ?? result?.Error;
  if (!csharp) {
    vscode.window.showWarningMessage(error ?? 'No C# was produced.');
    return;
  }

  const doc = await vscode.workspace.openTextDocument({ language: 'csharp', content: csharp });
  await vscode.window.showTextDocument(doc, {
    viewColumn: vscode.ViewColumn.Beside,
    preview: true,
  });
}

export async function deactivate(): Promise<void> {
  if (client) {
    await client.stop();
    client = undefined;
  }
}
