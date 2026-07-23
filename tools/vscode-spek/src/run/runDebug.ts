// Run/debug the project of the active .spek file.
//
// F5 in a .spek file works through the `spek` debugger contribution: VS Code
// sees the active language, asks our DebugConfigurationProvider to resolve an
// (initially empty) configuration, and the provider redirects to a `coreclr`
// launch — .spek breakpoints already bind because Debug builds emit absolute
// #line directives into the generated C# (see build/Spek.targets), so the
// PDB maps straight back to the .spek source. Nothing here touches the
// compiler.
//
// The launch convention (documented on builtDllPath): the program is
// <projectDir>/bin/Debug/<TargetFramework>/<AssemblyName>.dll, built by a
// `dotnet build` task that runs before the debugger attaches — the
// programmatic equivalent of a launch.json preLaunchTask, without writing
// into the user's tasks.json.

import * as vscode from 'vscode';
import * as fs from 'fs';
import * as path from 'path';
import * as os from 'os';
import { builtDllPath, CsprojMeta, parseCsproj, resolveRunProjects } from './projectResolver';
import { nodeFs } from './nodeFs';
import { withDiagnosticsEnabled } from './debugDecision';

/** Marker carried on coreclr configs this extension launches; the observe
 *  auto-attach tracker treats marked sessions as Spek without any disk probe. */
export const SPEK_LAUNCH_MARKER = '__spekLaunch';

export function registerRunDebug(context: vscode.ExtensionContext): void {
  context.subscriptions.push(
    vscode.commands.registerCommand('spek.runProjectOfFile', () =>
      runOrDebugActiveFile(context, /*debug*/ false),
    ),
    vscode.commands.registerCommand('spek.debugProjectOfFile', () =>
      runOrDebugActiveFile(context, /*debug*/ true),
    ),
    // Plain registration: resolves configs of type `spek` (from F5 with no
    // launch.json, from our dynamic configuration, or from a user's
    // launch.json entry) and redirects them to a coreclr session.
    vscode.debug.registerDebugConfigurationProvider('spek', {
      resolveDebugConfiguration(folder, config) {
        return resolveSpekDebugConfiguration(context, folder, config);
      },
    }),
    // Every coreclr launch in a Spek workspace gets the diagnostics port
    // re-enabled, regardless of which config launched it — the structural
    // fix for "the actor panel can't attach to debugged processes". Gated
    // on workspace evidence so unrelated .NET projects are untouched.
    vscode.debug.registerDebugConfigurationProvider('coreclr', {
      async resolveDebugConfiguration(_folder, config) {
        if (config.request === 'launch' && (await workspaceLooksSpek())) {
          config.env = withDiagnosticsEnabled(config.env as Record<string, unknown> | undefined);
        }
        return config;
      },
    }),
    // Dynamic registration: offers "Spek: Debug project of active file" in
    // the Run and Debug view's dropdown without a launch.json.
    vscode.debug.registerDebugConfigurationProvider(
      'spek',
      {
        provideDebugConfigurations(): vscode.DebugConfiguration[] {
          return [
            {
              type: 'spek',
              request: 'launch',
              name: 'Spek: Debug project of active .spek file',
            },
          ];
        },
      },
      vscode.DebugConfigurationProviderTriggerKind.Dynamic,
    ),
  );
}

/**
 * The provider entry point. An empty configuration means the user pressed
 * F5 with no launch.json and VS Code chose this debugger from the file's
 * language; fill it from the active editor. Every spek config is then
 * launched asynchronously as coreclr, and `undefined` is returned so the
 * placeholder `spek` session itself never starts (there is no spek adapter).
 */
let workspaceSpekCache: boolean | undefined;
async function workspaceLooksSpek(): Promise<boolean> {
  if (workspaceSpekCache === true) return true;   // sticky once seen
  if (vscode.window.activeTextEditor?.document.languageId === 'spek') return (workspaceSpekCache = true);
  const hits = await vscode.workspace.findFiles(
    '**/*.spek', '**/{node_modules,bin,obj,out}/**', 1);
  return (workspaceSpekCache = hits.length > 0);
}

function resolveSpekDebugConfiguration(
  context: vscode.ExtensionContext,
  folder: vscode.WorkspaceFolder | undefined,
  config: vscode.DebugConfiguration,
): undefined {
  const editor = vscode.window.activeTextEditor;
  let file: string | undefined =
    typeof config.file === 'string' && config.file.length > 0 ? config.file : undefined;
  if (!file && editor && editor.document.languageId === 'spek') {
    file = editor.document.uri.fsPath;
  }
  if (!file) {
    void vscode.window.showErrorMessage(
      'Open a .spek file (or set "file" in the launch configuration) to run its project.',
    );
    return undefined;
  }
  const noDebug = (config as { noDebug?: boolean }).noDebug === true;
  void launchProjectOfFile(context, file, !noDebug, folder);
  return undefined; // never start the placeholder `spek` session
}

async function runOrDebugActiveFile(
  context: vscode.ExtensionContext,
  debug: boolean,
): Promise<void> {
  const editor = vscode.window.activeTextEditor;
  if (!editor || editor.document.languageId !== 'spek') {
    void vscode.window.showInformationMessage(
      `Open a .spek file to ${debug ? 'debug' : 'run'} its project.`,
    );
    return;
  }
  await launchProjectOfFile(context, editor.document.uri.fsPath, debug);
}

async function launchProjectOfFile(
  context: vscode.ExtensionContext,
  spekFile: string,
  debug: boolean,
  folderHint?: vscode.WorkspaceFolder,
): Promise<void> {
  const folder =
    folderHint ?? vscode.workspace.getWorkspaceFolder(vscode.Uri.file(spekFile));
  const csproj = await resolveProjectForFile(context, spekFile, folder);
  if (!csproj) return; // resolution already reported, or the pick was cancelled

  if (!debug) {
    await runProject(csproj, folder);
    return;
  }
  await debugProject(csproj, folder);
}

/**
 * Project resolution for one .spek file (see projectResolver.ts for the
 * heuristic). Multiple candidates → quickpick, remembered per owner project
 * in workspaceState so the question is asked once per workspace.
 */
async function resolveProjectForFile(
  context: vscode.ExtensionContext,
  spekFile: string,
  folder: vscode.WorkspaceFolder | undefined,
): Promise<string | undefined> {
  const stopDir = folder?.uri.fsPath ?? path.dirname(spekFile);
  const resolution = resolveRunProjects(spekFile, stopDir, nodeFs);

  if (resolution.candidates.length === 0) {
    void vscode.window.showErrorMessage(
      `No runnable project found for ${path.basename(spekFile)}. ` +
        'Expected a .csproj in the file’s directory (or above it), or an ' +
        'executable project nearby that references it.',
    );
    return undefined;
  }
  if (resolution.candidates.length === 1) return resolution.candidates[0];

  const memoryKey = `spek.runProject:${resolution.owner ?? spekFile}`;
  const remembered = context.workspaceState.get<string>(memoryKey);
  if (remembered && resolution.candidates.includes(remembered) && fs.existsSync(remembered)) {
    return remembered;
  }

  type Item = vscode.QuickPickItem & { csproj: string };
  const items: Item[] = resolution.candidates.map((c) => ({
    csproj: c,
    label: `$(project) ${path.basename(c, '.csproj')}`,
    description: folder ? path.relative(folder.uri.fsPath, c) : c,
  }));
  const picked = await vscode.window.showQuickPick(items, {
    title: 'Spek: Which project runs this file?',
    placeHolder: 'Several projects can run this .spek file — the choice is remembered for this workspace',
  });
  if (!picked) return undefined;
  await context.workspaceState.update(memoryKey, picked.csproj);
  return picked.csproj;
}

/** `dotnet run --project <csproj>` in the task terminal (no debugger). */
async function runProject(
  csproj: string,
  folder: vscode.WorkspaceFolder | undefined,
): Promise<void> {
  const name = path.basename(csproj, '.csproj');
  const task = new vscode.Task(
    { type: 'spek', action: 'run', project: csproj },
    folder ?? vscode.TaskScope.Workspace,
    `run ${name}`,
    'spek',
    new vscode.ShellExecution('dotnet', ['run', '--project', csproj]),
  );
  task.presentationOptions = { reveal: vscode.TaskRevealKind.Always, clear: true };
  await vscode.tasks.executeTask(task);
}

/** Build with `dotnet build`, then launch the built dll under coreclr. */
async function debugProject(
  csproj: string,
  folder: vscode.WorkspaceFolder | undefined,
): Promise<void> {
  // coreclr is the C# extension's debug adapter; without it there is nothing
  // to delegate to.
  if (
    !vscode.extensions.getExtension('ms-dotnettools.csharp') &&
    !vscode.extensions.getExtension('ms-dotnettools.csdevkit')
  ) {
    void vscode.window.showErrorMessage(
      'Debugging Spek needs the C# extension (ms-dotnettools.csharp) for its ' +
        '.NET debugger. Install it, or use "Spek: Run Project of This File" instead.',
    );
    return;
  }

  const meta = readMeta(csproj);
  if (!meta) {
    void vscode.window.showErrorMessage(`Couldn't read ${csproj}.`);
    return;
  }
  const dll = builtDllPath(meta, 'Debug');
  if (!dll) {
    void vscode.window.showErrorMessage(
      `${path.basename(csproj)} doesn't declare a <TargetFramework>, so the built ` +
        'output path can’t be derived. Add a launch.json with an explicit "program".',
    );
    return;
  }

  const name = path.basename(csproj, '.csproj');
  const exitCode = await runBuildTask(csproj, name, folder);
  if (exitCode !== 0) {
    void vscode.window.showErrorMessage(
      `dotnet build failed for ${name}${exitCode === undefined ? '' : ` (exit code ${exitCode})`} — see the task output.`,
    );
    return;
  }
  if (!fs.existsSync(dll)) {
    void vscode.window.showErrorMessage(
      `Build succeeded but ${dll} wasn't produced. The debug launch assumes the SDK ` +
        'default layout bin/Debug/<TargetFramework>/<AssemblyName>.dll; projects that ' +
        'override OutputPath or use artifacts output need their own launch.json.',
    );
    return;
  }

  await vscode.debug.startDebugging(folder, {
    type: 'coreclr',
    request: 'launch',
    name: `Spek: ${name}`,
    program: dll,
    cwd: path.dirname(csproj),
    // The C# debugger disables the diagnostics port on processes it
    // launches (COMPlus_EnableDiagnostics=0 — the same reason dotnet-counters
    // can't attach to VS-debugged processes). Spek's live actor panel IS a
    // diagnostics-port client, so re-enable it explicitly; and pin TMPDIR to
    // ours so the socket lands where spekc (inheriting our env) will look.
    env: {
      TMPDIR: os.tmpdir(),
      COMPlus_EnableDiagnostics: '1',
      DOTNET_EnableDiagnostics: '1',
    },
    [SPEK_LAUNCH_MARKER]: true,
  });
}

/** Executes `dotnet build <csproj> -c Debug` as a task; resolves its exit code. */
function runBuildTask(
  csproj: string,
  name: string,
  folder: vscode.WorkspaceFolder | undefined,
): Promise<number | undefined> {
  const task = new vscode.Task(
    { type: 'spek', action: 'build', project: csproj },
    folder ?? vscode.TaskScope.Workspace,
    `build ${name}`,
    'spek',
    new vscode.ShellExecution('dotnet', ['build', csproj, '-c', 'Debug']),
    '$msCompile',
  );
  task.presentationOptions = { reveal: vscode.TaskRevealKind.Silent, clear: true };
  return new Promise<number | undefined>((resolve) => {
    // Match on the task definition, not the TaskExecution identity — the
    // executeTask promise can resolve after a very fast process already ended.
    const done = vscode.tasks.onDidEndTaskProcess((e) => {
      const def = e.execution.task.definition;
      if (def.type === 'spek' && def.action === 'build' && def.project === csproj) {
        done.dispose();
        resolve(e.exitCode);
      }
    });
    vscode.tasks.executeTask(task).then(undefined, () => {
      done.dispose();
      resolve(undefined);
    });
  });
}

function readMeta(csproj: string): CsprojMeta | null {
  const content = nodeFs.readFile(csproj);
  return content === null ? null : parseCsproj(content, csproj);
}
