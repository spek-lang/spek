// Unit tests for supervision-tree construction (src/observe/tree.ts).

import { test } from 'node:test';
import * as assert from 'node:assert/strict';
import { ActorInfo } from '../observe/model';
import { buildSupervisionTree, flattenTree } from '../observe/tree';

function actor(path: string, children: string[] = []): ActorInfo {
  return {
    path,
    actorType: path.replace(/#\d+$/, ''),
    behavior: null,
    mailboxDepth: 0,
    mailboxHead: [],
  dispatchedCount: 0,
    restarts: 0,
    lastMessageType: null,
    spawnedAt: null,
    isMaterialized: true,
    isStopped: false,
    children,
    handlerRunningForMs: null,
  };
}

test('roots are the actors nobody lists as a child', () => {
  const roots = buildSupervisionTree([
    actor('Root', ['Kid1', 'Kid2']),
    actor('Kid1'),
    actor('Kid2', ['Grandkid']),
    actor('Grandkid'),
    actor('Loner'),
  ]);
  assert.deepEqual(roots.map((r) => r.actor.path), ['Loner', 'Root']);
  const root = roots.find((r) => r.actor.path === 'Root');
  assert.ok(root);
  assert.deepEqual(root.children.map((c) => c.actor.path), ['Kid1', 'Kid2']);
  assert.deepEqual(root.children[1].children.map((c) => c.actor.path), ['Grandkid']);
});

test('a child referenced but never sampled becomes a synthesized stub', () => {
  const roots = buildSupervisionTree([actor('Root', ['Ghost'])]);
  assert.equal(roots.length, 1);
  const ghost = roots[0].children[0];
  assert.equal(ghost.actor.path, 'Ghost');
  assert.equal(ghost.synthesized, true);
  assert.equal(roots[0].synthesized, false);
});

test('a child claimed by two parents is attached once (first parent wins)', () => {
  const roots = buildSupervisionTree([
    actor('A', ['Shared']),
    actor('B', ['Shared']),
    actor('Shared'),
  ]);
  const placements = flattenTree(roots).filter((f) => f.node.actor.path === 'Shared');
  assert.equal(placements.length, 1);
});

test('duplicate paths keep the first snapshot', () => {
  const first = actor('Dup');
  first.mailboxDepth = 7;
  const second = actor('Dup');
  second.mailboxDepth = 99;
  const roots = buildSupervisionTree([first, second]);
  assert.equal(roots.length, 1);
  assert.equal(roots[0].actor.mailboxDepth, 7);
});

test('a pure cycle still renders: unattached members get promoted to roots', () => {
  const roots = buildSupervisionTree([actor('A', ['B']), actor('B', ['A'])]);
  // Neither is a natural root (both are children), but nothing may vanish.
  assert.equal(roots.length, 1);
  assert.equal(roots[0].actor.path, 'A');
  assert.deepEqual(roots[0].children.map((c) => c.actor.path), ['B']);
  // The cycle edge back to A truncates rather than recursing forever.
  assert.deepEqual(roots[0].children[0].children, []);
});

test('empty input produces an empty forest', () => {
  assert.deepEqual(buildSupervisionTree([]), []);
});

test('flattenTree reports depth-first order with depths', () => {
  const flat = flattenTree(
    buildSupervisionTree([actor('R', ['C1', 'C2']), actor('C1', ['G']), actor('C2'), actor('G')]),
  );
  assert.deepEqual(
    flat.map((f) => [f.node.actor.path, f.depth]),
    [['R', 0], ['C1', 1], ['G', 2], ['C2', 1]],
  );
});

test('children render in stable sorted order', () => {
  const roots = buildSupervisionTree([actor('R', ['Zeta', 'Alpha']), actor('Zeta'), actor('Alpha')]);
  assert.deepEqual(roots[0].children.map((c) => c.actor.path), ['Alpha', 'Zeta']);
});
