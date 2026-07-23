// Unit tests for the pure CodeLens helpers (src/observe/actorDecl.ts):
// actor-declaration scanning and lens-title formatting.

import { test } from 'node:test';
import * as assert from 'node:assert/strict';
import { findActorDeclarations, lensTitle } from '../observe/actorDecl';
import { normalizeActor, statsByType } from '../observe/model';

test('finds plain, modified, abstract, generic, and inheriting declarations', () => {
  const text = [
    'actor Fragile',
    '{',
    '}',
    'public actor ApiKey {',
    'abstract actor WorkerBase {',
    'internal abstract actor NotifierBase {',
    '  actor Indented {',
    'actor Doubler : WorkerBase {',
    'actor Cache<T> {',
  ].join('\n');
  const names = findActorDeclarations(text).map((d) => d.name);
  assert.deepEqual(names, [
    'Fragile',
    'ApiKey',
    'WorkerBase',
    'NotifierBase',
    'Indented',
    'Doubler',
    'Cache',
  ]);
});

test('reports the match start index for range placement', () => {
  const text = 'message Ping();\nactor Pong {\n}';
  const decls = findActorDeclarations(text);
  assert.equal(decls.length, 1);
  assert.equal(text.slice(decls[0].index, decls[0].index + 10), 'actor Pong');
});

test('does not match actor-as-substring or mid-line mentions', () => {
  const text = [
    'message ReactorStarted(string name);',
    '// an actor Foo mentioned mid-line stays unmatched',
    'let compactor = 1;',
  ].join('\n');
  assert.deepEqual(findActorDeclarations(text), []);
});

test('lensTitle formats the single-instance shape from the vision doc', () => {
  const a = normalizeActor({
    Path: 'Doubler',
    ActorType: 'Doubler',
    MailboxDepth: 5,
    Restarts: 1,
    LastMessageType: 'Work',
  });
  assert.ok(a);
  const stats = statsByType([a]).get('Doubler');
  assert.ok(stats);
  assert.equal(lensTitle(stats), '● mailbox 5 · restarts 1 · last: Work');
});

test('lensTitle shows instance count, aggregates, and stopped suffix', () => {
  const mk = (path: string, depth: number, stopped: boolean) => {
    const a = normalizeActor({
      Path: path,
      ActorType: 'Worker',
      MailboxDepth: depth,
      Restarts: 1,
      LastMessageType: 'Job',
      IsStopped: stopped,
    });
    assert.ok(a);
    return a;
  };
  const running = statsByType([mk('w1', 2, false), mk('w2', 3, false)]).get('Worker');
  assert.ok(running);
  assert.equal(lensTitle(running), '● 2 instances · mailbox 5 · restarts 2 · last: Job');

  const stopped = statsByType([mk('w1', 0, true)]).get('Worker');
  assert.ok(stopped);
  assert.equal(lensTitle(stopped), '● mailbox 0 · restarts 1 · last: Job (stopped)');
});

test('lensTitle uses a dash when no message was dispatched yet', () => {
  const a = normalizeActor({ Path: 'Idle', ActorType: 'Idle' });
  assert.ok(a);
  const stats = statsByType([a]).get('Idle');
  assert.ok(stats);
  assert.equal(lensTitle(stats), '● mailbox 0 · restarts 0 · last: -');
});
