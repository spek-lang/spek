// Unit tests for the NDJSON sample parser (src/observe/model.ts).
// Runs under the built-in Node test runner: `npm test`.

import { test } from 'node:test';
import * as assert from 'node:assert/strict';
import {
  bareTypeName,
  NdjsonLineBuffer,
  normalizeActor,
  parseSampleLine,
  statsByType,
} from '../observe/model';

const pascalActor = {
  Path: 'Inventory',
  ActorType: 'Inventory',
  Behavior: 'Restocking',
  MailboxDepth: 4,
  MailboxHead: ['Reserve', 'Reserve'],
  Restarts: 2,
  LastMessageType: 'Reserve',
  SpawnedAt: '2026-07-13T10:00:00Z',
  IsMaterialized: true,
  IsStopped: false,
  Children: ['Shelf#1', 'Shelf#2'],
};

test('parses a PascalCase sample line (System.Text.Json default casing)', () => {
  const line = JSON.stringify({ system: 'shop', actors: [pascalActor] });
  const sample = parseSampleLine(line);
  assert.ok(sample);
  assert.equal(sample.system, 'shop');
  assert.equal(sample.actors.length, 1);
  const a = sample.actors[0];
  assert.equal(a.path, 'Inventory');
  assert.equal(a.behavior, 'Restocking');
  assert.equal(a.mailboxDepth, 4);
  assert.deepEqual(a.mailboxHead, ['Reserve', 'Reserve']);
  assert.equal(a.restarts, 2);
  assert.equal(a.lastMessageType, 'Reserve');
  assert.equal(a.spawnedAt, '2026-07-13T10:00:00Z');
  assert.equal(a.isMaterialized, true);
  assert.equal(a.isStopped, false);
  assert.deepEqual(a.children, ['Shelf#1', 'Shelf#2']);
});

test('parses camelCase keys and an uppercase System wrapper too', () => {
  const line = JSON.stringify({
    System: 'shop',
    Actors: [
      {
        path: 'Cart',
        actorType: 'Cart',
        mailboxDepth: 1,
        restarts: 0,
        isStopped: true,
        children: [],
      },
    ],
  });
  const sample = parseSampleLine(line);
  assert.ok(sample);
  assert.equal(sample.system, 'shop');
  assert.equal(sample.actors[0].path, 'Cart');
  assert.equal(sample.actors[0].isStopped, true);
});

test('defaults missing optional fields without throwing', () => {
  const sample = parseSampleLine(JSON.stringify({ system: 's', actors: [{ Path: 'X' }] }));
  assert.ok(sample);
  const a = sample.actors[0];
  assert.equal(a.actorType, '(unknown)');
  assert.equal(a.behavior, null);
  assert.equal(a.mailboxDepth, 0);
  assert.deepEqual(a.mailboxHead, []);
  assert.equal(a.restarts, 0);
  assert.equal(a.lastMessageType, null);
  assert.equal(a.isMaterialized, true); // materialized unless stated otherwise
  assert.equal(a.isStopped, false);
  assert.deepEqual(a.children, []);
  assert.equal(a.handlerRunningForMs, null);
});

test('drops actors with no Path and non-object entries', () => {
  const sample = parseSampleLine(
    JSON.stringify({ system: 's', actors: [{ ActorType: 'NoPath' }, 42, null, { Path: 'Ok' }] }),
  );
  assert.ok(sample);
  assert.deepEqual(sample.actors.map((a) => a.path), ['Ok']);
});

test('rejects non-JSON, non-object, and sample-shaped-but-actorless lines', () => {
  assert.equal(parseSampleLine(''), null);
  assert.equal(parseSampleLine('   '), null);
  assert.equal(parseSampleLine('[shop] ACTOR BEHAVIOR MAILBOX'), null); // human table output
  assert.equal(parseSampleLine('{"truncated": '), null);
  assert.equal(parseSampleLine('[1,2,3]'), null);
  assert.equal(parseSampleLine('{"system":"s"}'), null); // no actors array
});

test('missing system name gets a placeholder', () => {
  const sample = parseSampleLine(JSON.stringify({ actors: [] }));
  assert.ok(sample);
  assert.equal(sample.system, '(unnamed system)');
});

test('picks up watchdog-style running-for fields when present', () => {
  const a = normalizeActor({ Path: 'P', HandlerRunningForMs: 2500 });
  assert.equal(a?.handlerRunningForMs, 2500);
  const b = normalizeActor({ Path: 'P', runningForMs: 900 });
  assert.equal(b?.handlerRunningForMs, 900);
});

test('NdjsonLineBuffer reassembles chunks split mid-line', () => {
  const buf = new NdjsonLineBuffer();
  assert.deepEqual(buf.feed('{"a":'), []);
  assert.deepEqual(buf.feed('1}\n{"b":2}\n{"c"'), ['{"a":1}', '{"b":2}']);
  assert.deepEqual(buf.feed(':3}\n'), ['{"c":3}']);
  assert.equal(buf.flush(), null);
});

test('NdjsonLineBuffer strips CRLF and flushes a trailing partial', () => {
  const buf = new NdjsonLineBuffer();
  assert.deepEqual(buf.feed('one\r\ntwo\r\npartial'), ['one', 'two']);
  assert.equal(buf.flush(), 'partial');
  assert.equal(buf.flush(), null);
});

test('bareTypeName strips CLR generic arity', () => {
  assert.equal(bareTypeName('Cache`1'), 'Cache');
  assert.equal(bareTypeName('Plain'), 'Plain');
});

test('statsByType aggregates instances and skips unmaterialized placeholders', () => {
  const mk = (over: Record<string, unknown>) => {
    const a = normalizeActor({ Path: String(Math.random()), ...over });
    assert.ok(a);
    return a;
  };
  const stats = statsByType([
    mk({ ActorType: 'Worker', MailboxDepth: 3, Restarts: 1, LastMessageType: 'Job' }),
    mk({ ActorType: 'Worker', MailboxDepth: 9, Restarts: 2, LastMessageType: 'Retry' }),
    mk({ ActorType: 'Worker`1', MailboxDepth: 1, Restarts: 0, LastMessageType: 'Ping' }),
    mk({ ActorType: '(unmaterialized)' }),
  ]);
  assert.equal(stats.size, 1);
  const w = stats.get('Worker');
  assert.ok(w);
  assert.equal(w.instances, 3);
  assert.equal(w.mailboxDepth, 13);
  assert.equal(w.restarts, 3);
  assert.equal(w.lastMessageType, 'Retry'); // from the busiest instance
  assert.equal(w.anyStopped, false);
  assert.equal(w.allStopped, false);
});

test('statsByType flags fully and partially stopped types', () => {
  const mk = (path: string, stopped: boolean) => {
    const a = normalizeActor({ Path: path, ActorType: 'S', IsStopped: stopped });
    assert.ok(a);
    return a;
  };
  const all = statsByType([mk('a', true), mk('b', true)]).get('S');
  assert.ok(all);
  assert.equal(all.allStopped, true);
  const some = statsByType([mk('a', true), mk('b', false)]).get('S');
  assert.ok(some);
  assert.equal(some.anyStopped, true);
  assert.equal(some.allStopped, false);
});

test('parseErrorLine extracts spekc error objects and rejects samples', () => {
  const { parseErrorLine } = require('../observe/model');
  assert.strictEqual(parseErrorLine('{"error":"cannot attach to pid 7 — gone"}'), 'cannot attach to pid 7 — gone');
  assert.strictEqual(parseErrorLine('{"system":"x","actors":[]}'), null);
  assert.strictEqual(parseErrorLine('not json'), null);
});

test('diagnosticDirForPid finds the dir containing the pid socket', () => {
  const { candidateDiagnosticDirs, diagnosticDirForPid } = require('../observe/diagnosticEndpoints');
  const dirs = candidateDiagnosticDirs('/a', '/b');
  assert.deepStrictEqual(dirs, ['/a', '/b', '/tmp']);
  const listing: Record<string, string[]> = {
    '/a': ['dotnet-diagnostic-111-1-socket'],
    '/b': ['dotnet-diagnostic-222-2-socket'],
    '/tmp': [],
  };
  assert.strictEqual(diagnosticDirForPid(222, dirs, (d: string) => listing[d] ?? []), '/b');
  assert.strictEqual(diagnosticDirForPid(999, dirs, (d: string) => listing[d] ?? []), null);
});


test('locateSocketDir finds pid sockets across primary and session temp dirs', () => {
  const { locateSocketDir } = require('../observe/socketLocator');
  const listing: Record<string, string[]> = {
    '/hosttmp': [],
    '/tmp': ['dotnet-diagnostic-42-1-socket'],
    '/var/folders/aa/bb/T': ['dotnet-diagnostic-77-9-socket'],
  };
  const deps = (dirs: string[]) => ({
    listDir: (d: string) => listing[d] ?? [],
    osTmp: '/hosttmp', envTmp: undefined,
    sessionTempDirs: () => dirs,
  });
  assert.strictEqual(locateSocketDir(42, deps([])), '/tmp');
  assert.strictEqual(locateSocketDir(77, deps(['/var/folders/aa/bb/T'])), '/var/folders/aa/bb/T');
  assert.strictEqual(locateSocketDir(99, deps(['/var/folders/aa/bb/T'])), null);
});
