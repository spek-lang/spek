import * as vscode from 'vscode';
import { ObserveSample } from './model';
import { buildSupervisionTree, TreeNode } from './tree';

/**
 * The live actor panel: a self-contained webview rendering the
 * supervision tree for the attached process. No external resources —
 * a strict CSP allows only the inline nonce'd script and inline styles,
 * and every color comes from VS Code theme variables so the panel
 * follows light/dark/high-contrast automatically. Samples update the
 * existing DOM in place (keyed by actor path), so the tree doesn't
 * flicker or lose scroll position at refresh time.
 */
export class ActorPanel implements vscode.Disposable {
  static readonly viewType = 'spekActorPanel';

  private readonly panel: vscode.WebviewPanel;
  private readonly disposables: vscode.Disposable[] = [];

  private readonly _onDidDispose = new vscode.EventEmitter<void>();
  readonly onDidDispose = this._onDidDispose.event;

  private readonly _onOpenActor = new vscode.EventEmitter<string>();
  /** Fires with the actor type name when a tree node is clicked. */
  readonly onOpenActor = this._onOpenActor.event;

  constructor(pid: number, iconPath?: { light: vscode.Uri; dark: vscode.Uri }) {
    this.panel = vscode.window.createWebviewPanel(
      ActorPanel.viewType,
      `Spek Actors (pid ${pid})`,
      { viewColumn: vscode.ViewColumn.Beside, preserveFocus: true },
      {
        enableScripts: true,
        localResourceRoots: [], // fully inline — the webview loads nothing
        retainContextWhenHidden: true,
      },
    );
    // Same mark as the .spek file icon, so the tab reads as Spek's.
    if (iconPath) this.panel.iconPath = iconPath;
    this.panel.webview.html = buildHtml(this.panel.webview, pid);

    this.disposables.push(
      this.panel.webview.onDidReceiveMessage((msg) => {
        if (msg?.type === 'openActor' && typeof msg.actorType === 'string') {
          this._onOpenActor.fire(msg.actorType);
        }
      }),
    );
    this.panel.onDidDispose(() => this._onDidDispose.fire(), null, this.disposables);
  }

  reveal(): void {
    this.panel.reveal(undefined, true);
  }

  update(sample: ObserveSample, warnDepth: number): void {
    const roots = buildSupervisionTree(sample.actors);
    void this.panel.webview.postMessage({
      type: 'sample',
      system: sample.system,
      actorCount: sample.actors.length,
      warnDepth,
      sampledAt: new Date().toLocaleTimeString(),
      roots: roots.map(toWire),
    });
  }

  /** Shows a terminal banner when the observe process ends or errors. */
  showEnded(message: string): void {
    void this.panel.webview.postMessage({ type: 'ended', message });
  }

  /**
   * Shows a calm, non-error notice above the tree — e.g. "paused with
   * debugger — tree frozen at last sample" while a breakpoint holds the
   * debuggee (its sampler can't emit while suspended). Cleared explicitly
   * or by the next sample.
   */
  showNotice(message: string): void {
    void this.panel.webview.postMessage({ type: 'notice', message });
  }

  clearNotice(): void {
    void this.panel.webview.postMessage({ type: 'clearNotice' });
  }

  dispose(): void {
    for (const d of this.disposables) d.dispose();
    this._onDidDispose.dispose();
    this._onOpenActor.dispose();
    this.panel.dispose();
  }
}

/** The wire shape posted to the webview — a plain-JSON projection of TreeNode. */
interface WireNode {
  path: string;
  actorType: string;
  behavior: string | null;
  mailboxDepth: number;
  dispatchedCount: number;
  mailboxHead: string[];
  restarts: number;
  lastMessageType: string | null;
  handlerRunningForMs: number | null;
  stopped: boolean;
  passivated: boolean;
  synthesized: boolean;
  children: WireNode[];
}

function toWire(node: TreeNode): WireNode {
  const a = node.actor;
  return {
    path: a.path,
    actorType: a.actorType,
    behavior: a.behavior,
    mailboxDepth: a.mailboxDepth,
    dispatchedCount: a.dispatchedCount,
    mailboxHead: a.mailboxHead,
    restarts: a.restarts,
    lastMessageType: a.lastMessageType,
    handlerRunningForMs: a.handlerRunningForMs,
    stopped: a.isStopped,
    passivated: !a.isMaterialized && !a.isStopped,
    synthesized: node.synthesized,
    children: node.children.map(toWire),
  };
}

function nonce(): string {
  const chars = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789';
  let out = '';
  for (let i = 0; i < 32; i++) out += chars.charAt(Math.floor(Math.random() * chars.length));
  return out;
}

function buildHtml(webview: vscode.Webview, pid: number): string {
  const n = nonce();
  // Everything inline; CSP forbids all remote loads.
  return /* html */ `<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="UTF-8">
<meta http-equiv="Content-Security-Policy"
      content="default-src 'none'; style-src 'unsafe-inline'; script-src 'nonce-${n}';">
<meta name="viewport" content="width=device-width, initial-scale=1.0">
<title>Spek Actors</title>
<style>
  body {
    font-family: var(--vscode-font-family);
    font-size: var(--vscode-font-size);
    color: var(--vscode-foreground);
    background: transparent;
    padding: 0 12px 12px;
  }
  header {
    position: sticky; top: 0;
    background: var(--vscode-editor-background);
    padding: 8px 0 6px;
    border-bottom: 1px solid var(--vscode-widget-border, rgba(128,128,128,.25));
    margin-bottom: 8px;
    display: flex; align-items: baseline; gap: 10px; flex-wrap: wrap;
  }
  header .system { font-weight: 600; font-size: 1.1em; }
  header .meta { color: var(--vscode-descriptionForeground); font-size: .9em; }
  #banner {
    display: none;
    margin: 6px 0;
    padding: 6px 10px;
    border-left: 3px solid var(--vscode-editorWarning-foreground, #cca700);
    background: var(--vscode-inputValidation-warningBackground, rgba(204,167,0,.12));
  }
  #notice {
    display: none;
    margin: 6px 0;
    padding: 6px 10px;
    border-left: 3px solid var(--vscode-editorInfo-foreground, #3794ff);
    background: var(--vscode-inputValidation-infoBackground, rgba(55,148,255,.10));
    color: var(--vscode-descriptionForeground);
    font-style: italic;
  }
  #empty { color: var(--vscode-descriptionForeground); padding: 12px 0; }
  ul.tree { list-style: none; padding-left: 0; margin: 0; }
  ul.tree ul.tree { padding-left: 18px; border-left: 1px solid var(--vscode-tree-indentGuidesStroke, rgba(128,128,128,.25)); margin-left: 7px; }
  li.actor { margin: 2px 0; }
  .row {
    display: flex; align-items: baseline; gap: 8px; flex-wrap: wrap;
    padding: 2px 6px; border-radius: 3px; border: 1px solid transparent;
  }
  .row:hover { background: var(--vscode-list-hoverBackground); }
  .name {
    cursor: pointer; font-weight: 600;
    color: var(--vscode-textLink-foreground);
  }
  .name:hover { text-decoration: underline; }
  .type { color: var(--vscode-descriptionForeground); font-size: .9em; }
  .stat { font-family: var(--vscode-editor-font-family, monospace); font-size: .9em; }
  .stat .label { color: var(--vscode-descriptionForeground); }
  .behavior {
    font-size: .85em;
    padding: 0 6px; border-radius: 8px;
    background: var(--vscode-badge-background);
    color: var(--vscode-badge-foreground);
  }
  .flag { font-size: .85em; color: var(--vscode-descriptionForeground); font-style: italic; }
  .row.warn {
    border-color: var(--vscode-editorWarning-foreground, #cca700);
    background: var(--vscode-inputValidation-warningBackground, rgba(204,167,0,.10));
  }
  .warnIcon { color: var(--vscode-editorWarning-foreground, #cca700); }
  li.stopped > .row { opacity: .55; }
  li.stopped > .row .name, li.stopped > .row .type { text-decoration: line-through; }
  li.passivated > .row { opacity: .55; }
  li.synthesized > .row .name { font-style: italic; cursor: default; color: var(--vscode-descriptionForeground); }
</style>
</head>
<body>
<header>
  <span class="system" id="system">Spek Actors</span>
  <span class="meta" id="meta">attached to pid ${pid} — waiting for first sample…</span>
</header>
<div id="banner"></div>
<div id="notice"></div>
<div id="empty">Waiting for the first introspection sample…</div>
<ul class="tree" id="root"></ul>
<script nonce="${n}">
(function () {
  const vscode = acquireVsCodeApi();
  const rootEl = document.getElementById('root');
  const emptyEl = document.getElementById('empty');
  const systemEl = document.getElementById('system');
  const metaEl = document.getElementById('meta');
  const bannerEl = document.getElementById('banner');
  const noticeEl = document.getElementById('notice');
  let warnDepth = 10;

  function statSpan(cls) {
    const s = document.createElement('span');
    s.className = 'stat ' + cls;
    const label = document.createElement('span');
    label.className = 'label';
    const value = document.createElement('span');
    value.className = 'value';
    s.appendChild(label); s.appendChild(value);
    return s;
  }

  // Creates the fixed skeleton for one actor row; updateNode fills values.
  function createNode(node) {
    const li = document.createElement('li');
    li.className = 'actor';
    li.dataset.path = node.path;

    const row = document.createElement('div');
    row.className = 'row';

    const warn = document.createElement('span');
    warn.className = 'warnIcon';
    row.appendChild(warn);

    const name = document.createElement('span');
    name.className = 'name';
    name.addEventListener('click', () => {
      const t = li.dataset.actorType;
      if (t && !li.classList.contains('synthesized')) {
        vscode.postMessage({ type: 'openActor', actorType: t, path: node.path });
      }
    });
    row.appendChild(name);

    const type = document.createElement('span');
    type.className = 'type';
    row.appendChild(type);

    const behavior = document.createElement('span');
    behavior.className = 'behavior';
    row.appendChild(behavior);

    row.appendChild(statSpan('mailbox'));
    row.appendChild(statSpan('msgs'));
    row.appendChild(statSpan('restarts'));
    row.appendChild(statSpan('last'));

    const flag = document.createElement('span');
    flag.className = 'flag';
    row.appendChild(flag);

    li.appendChild(row);

    const children = document.createElement('ul');
    children.className = 'tree';
    li.appendChild(children);
    return li;
  }

  function setStat(row, cls, label, value, show) {
    const el = row.querySelector('.stat.' + cls);
    el.style.display = show ? '' : 'none';
    el.querySelector('.label').textContent = label;
    el.querySelector('.value').textContent = value;
  }

  function updateNode(li, node) {
    li.dataset.actorType = node.actorType;
    li.classList.toggle('stopped', node.stopped);
    li.classList.toggle('passivated', node.passivated);
    li.classList.toggle('synthesized', node.synthesized);

    const row = li.querySelector(':scope > .row');
    const running = node.handlerRunningForMs != null && node.handlerRunningForMs >= 1000;
    const deep = node.mailboxDepth >= warnDepth;
    const warned = !node.stopped && (running || deep);
    row.classList.toggle('warn', warned);
    const warnEl = row.querySelector('.warnIcon');
    warnEl.textContent = warned ? '\\u26a0' : '';
    warnEl.title = running
      ? 'handler running ' + Math.round(node.handlerRunningForMs / 1000) + 's'
      : deep ? 'mailbox depth ' + node.mailboxDepth : '';

    row.querySelector('.name').textContent = node.path;
    row.querySelector('.name').title = node.synthesized
      ? 'referenced as a child but not present in the sample'
      : 'Open actor ' + node.actorType + ' in source';

    const showType = node.actorType && node.actorType !== node.path.replace(/#\\d+$/, '');
    row.querySelector('.type').textContent = showType ? node.actorType : '';

    const behaviorEl = row.querySelector('.behavior');
    behaviorEl.style.display = node.behavior ? '' : 'none';
    behaviorEl.textContent = node.behavior || '';

    setStat(row, 'mailbox', 'mailbox ', String(node.mailboxDepth), true);
    if (node.mailboxHead && node.mailboxHead.length > 0) {
      row.querySelector('.stat.mailbox').title = 'head: ' + node.mailboxHead.join(', ');
    }
    setStat(row, 'msgs', 'msgs ', String(node.dispatchedCount), true);
    setStat(row, 'restarts', 'restarts ', String(node.restarts), true);
    setStat(row, 'last', 'last: ', node.lastMessageType || '-', true);

    const flagEl = row.querySelector('.flag');
    flagEl.textContent = node.stopped ? '(stopped)' : node.passivated ? '(passivated)' : '';
  }

  // Keyed reconciliation: existing <li>s are reused (moved into order),
  // new ones created, vanished ones removed. No full re-render, so the
  // tree updates without flicker and keeps scroll position.
  function syncChildren(containerEl, nodes) {
    const existing = new Map();
    for (const li of containerEl.children) existing.set(li.dataset.path, li);

    let cursor = null; // last placed element
    for (const node of nodes) {
      let li = existing.get(node.path);
      if (!li) li = createNode(node);
      existing.delete(node.path);
      const expectedPosition = cursor ? cursor.nextSibling : containerEl.firstChild;
      if (li !== expectedPosition) containerEl.insertBefore(li, expectedPosition);
      updateNode(li, node);
      syncChildren(li.querySelector(':scope > ul.tree'), node.children);
      cursor = li;
    }
    for (const li of existing.values()) li.remove();
  }

  window.addEventListener('message', (event) => {
    const msg = event.data;
    if (msg.type === 'sample') {
      warnDepth = msg.warnDepth || warnDepth;
      systemEl.textContent = msg.system;
      metaEl.textContent =
        msg.actorCount + ' actor' + (msg.actorCount === 1 ? '' : 's') +
        ' \\u00b7 pid ${pid} \\u00b7 sampled ' + msg.sampledAt;
      emptyEl.style.display = msg.roots.length === 0 ? '' : 'none';
      if (msg.roots.length === 0) emptyEl.textContent = 'No actors in this sample.';
      syncChildren(rootEl, msg.roots);
    } else if (msg.type === 'ended') {
      bannerEl.style.display = 'block';
      bannerEl.textContent = msg.message;
      noticeEl.style.display = 'none'; // the terminal banner supersedes any notice
    } else if (msg.type === 'notice') {
      noticeEl.style.display = 'block';
      noticeEl.textContent = msg.message;
    } else if (msg.type === 'clearNotice') {
      noticeEl.style.display = 'none';
    }
  });
})();
</script>
</body>
</html>`;
}
