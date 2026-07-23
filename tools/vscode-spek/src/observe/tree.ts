// Pure supervision-tree construction from ActorInfo Children edges.
// No 'vscode' imports — unit-testable with plain Node.

import { ActorInfo } from './model';

export interface TreeNode {
  actor: ActorInfo;
  children: TreeNode[];
  /** True when this node was only referenced as a child but never sampled. */
  synthesized: boolean;
}

function stubActor(path: string): ActorInfo {
  return {
    path,
    actorType: '(unknown)',
    behavior: null,
    mailboxDepth: 0,
    dispatchedCount: 0,
    mailboxHead: [],
    restarts: 0,
    lastMessageType: null,
    spawnedAt: null,
    isMaterialized: false,
    isStopped: false,
    children: [],
    handlerRunningForMs: null,
  };
}

/**
 * Builds the supervision forest for one sample.
 *
 * Roots are the actors nobody lists as a child. Edges come from each
 * actor's Children display identities. The builder is defensive about
 * data that shouldn't happen but must not wedge the panel if it does:
 * duplicate paths (first snapshot wins), children referenced but never
 * sampled (a synthesized stub node), a child claimed by two parents
 * (first parent keeps it), and cycles (each node is attached at most
 * once, so a cycle simply truncates; if a cycle leaves no roots at all,
 * the unattached actors are promoted to roots so nothing disappears).
 * Roots and children render in stable path order.
 */
export function buildSupervisionTree(actors: ActorInfo[]): TreeNode[] {
  const byPath = new Map<string, ActorInfo>();
  for (const a of actors) {
    if (!byPath.has(a.path)) byPath.set(a.path, a);
  }

  const childPaths = new Set<string>();
  for (const a of byPath.values()) {
    for (const c of a.children) childPaths.add(c);
  }

  const attached = new Set<string>();

  const build = (actor: ActorInfo, synthesized: boolean): TreeNode => {
    attached.add(actor.path);
    const children: TreeNode[] = [];
    for (const childPath of [...actor.children].sort()) {
      if (attached.has(childPath)) continue; // second parent or cycle — already placed
      const child = byPath.get(childPath);
      children.push(child ? build(child, false) : build(stubActor(childPath), true));
    }
    return { actor, children, synthesized };
  };

  const roots: TreeNode[] = [];
  const rootActors = [...byPath.values()]
    .filter((a) => !childPaths.has(a.path))
    .sort((a, b) => a.path.localeCompare(b.path));
  for (const a of rootActors) {
    if (!attached.has(a.path)) roots.push(build(a, false));
  }

  // A cycle among all remaining actors leaves no natural root. Promote
  // whatever is still unattached (in path order) so the panel shows it.
  for (const a of [...byPath.values()].sort((x, y) => x.path.localeCompare(y.path))) {
    if (!attached.has(a.path)) roots.push(build(a, false));
  }

  return roots;
}

/** Flattens a forest depth-first, with depth for indented rendering. */
export function flattenTree(roots: TreeNode[]): Array<{ node: TreeNode; depth: number }> {
  const out: Array<{ node: TreeNode; depth: number }> = [];
  const walk = (node: TreeNode, depth: number) => {
    out.push({ node, depth });
    for (const c of node.children) walk(c, depth + 1);
  };
  for (const r of roots) walk(r, 0);
  return out;
}
