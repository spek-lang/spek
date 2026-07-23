import { test } from 'node:test';
import * as assert from 'node:assert';
import * as path from 'path';
import { resolveSpekcPath } from '../observe/spekcLocator';

const REPO = path.resolve(__dirname, '..', '..', '..', '..');

test('explicit setting wins verbatim', () => {
  assert.strictEqual(resolveSpekcPath('/opt/spekc', [], undefined), '/opt/spekc');
});

test('finds the real workspace-built CLI in this repo', () => {
  const p = resolveSpekcPath(undefined, [REPO], '');
  assert.ok(p && p.includes(path.join('Spek.Cli', 'bin')), `resolved: ${p}`);
});

test('null when nothing resolves', () => {
  assert.strictEqual(resolveSpekcPath(undefined, ['/nonexistent-ws'], '/nonexistent-dir'), null);
});
