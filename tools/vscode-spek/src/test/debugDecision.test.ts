// Unit tests for the debug auto-attach logic (src/run/debugDecision.ts):
// DAP process-event pid extraction, the auto-attach decision matrix,
// on-disk Spek evidence, and the frozen-sampler stall check.

import { test } from 'node:test';
import * as assert from 'node:assert/strict';
import * as path from 'path';
import * as fs from 'fs';
import {
  decideAutoAttach,
  extractDapProcessId,
  hasSpekEvidence,
  isSampleStalled,
} from '../run/debugDecision';
import { ProjectFs } from '../run/projectResolver';
import { nodeFs } from '../run/nodeFs';

// ---------- DAP process-event extraction ----------

test('extractDapProcessId reads systemProcessId from a process event', () => {
  assert.equal(
    extractDapProcessId({
      seq: 9,
      type: 'event',
      event: 'process',
      body: { name: 'Elevators.Harness.dll', systemProcessId: 43210, isLocalProcess: true, startMethod: 'launch' },
    }),
    43210,
  );
});

test('extractDapProcessId ignores everything that is not a process event with a pid', () => {
  assert.equal(extractDapProcessId(undefined), null);
  assert.equal(extractDapProcessId(null), null);
  assert.equal(extractDapProcessId('process'), null);
  assert.equal(extractDapProcessId({ type: 'event', event: 'stopped', body: {} }), null);
  assert.equal(extractDapProcessId({ type: 'response', event: 'process', body: { systemProcessId: 5 } }), null);
  assert.equal(extractDapProcessId({ type: 'event', event: 'process' }), null);
  assert.equal(extractDapProcessId({ type: 'event', event: 'process', body: {} }), null);
  assert.equal(extractDapProcessId({ type: 'event', event: 'process', body: { systemProcessId: '77' } }), null);
  assert.equal(extractDapProcessId({ type: 'event', event: 'process', body: { systemProcessId: 0 } }), null);
  assert.equal(extractDapProcessId({ type: 'event', event: 'process', body: { systemProcessId: -3 } }), null);
  assert.equal(extractDapProcessId({ type: 'event', event: 'process', body: { systemProcessId: 3.5 } }), null);
});

// ---------- the decision matrix ----------

test('auto-attach decision matrix', () => {
  const yes = () => true;
  const no = () => false;
  // never: off no matter what
  assert.equal(decideAutoAttach('never', true, yes), false);
  assert.equal(decideAutoAttach('never', false, yes), false);
  assert.equal(decideAutoAttach('never', false, no), false);
  // always: on no matter what
  assert.equal(decideAutoAttach('always', true, yes), true);
  assert.equal(decideAutoAttach('always', false, no), true);
  // spekProjectsOnly: our own launches attach; foreign ones need evidence
  assert.equal(decideAutoAttach('spekProjectsOnly', true, no), true);
  assert.equal(decideAutoAttach('spekProjectsOnly', false, yes), true);
  assert.equal(decideAutoAttach('spekProjectsOnly', false, no), false);
  // unknown setting values fail closed
  assert.equal(decideAutoAttach('sometimes' as never, true, yes), false);
});

test('decision: evidence probe is lazy — never/always do not touch the disk', () => {
  let probed = 0;
  const probe = () => (probed++, true);
  decideAutoAttach('never', false, probe);
  decideAutoAttach('always', false, probe);
  decideAutoAttach('spekProjectsOnly', true, probe); // marker short-circuits
  assert.equal(probed, 0);
  decideAutoAttach('spekProjectsOnly', false, probe);
  assert.equal(probed, 1);
});

// ---------- Spek evidence ----------

function fakeFs(files: Record<string, string>): ProjectFs {
  const paths = Object.keys(files).map((p) => path.normalize(p));
  const dirs = new Set<string>();
  for (const p of paths) {
    let d = path.dirname(p);
    while (!dirs.has(d)) {
      dirs.add(d);
      const parent = path.dirname(d);
      if (parent === d) break;
      d = parent;
    }
  }
  return {
    listDir(dir: string): string[] {
      const norm = path.normalize(dir);
      const names = new Set<string>();
      for (const p of paths) if (path.dirname(p) === norm) names.add(path.basename(p));
      for (const d of dirs) if (path.dirname(d) === norm && d !== norm) names.add(path.basename(d));
      return [...names];
    },
    isDirectory: (p) => dirs.has(path.normalize(p)),
    readFile: (f) => files[path.normalize(f)] ?? null,
  };
}

const R = path.normalize('/ws');

test('evidence: a project with .spek sources in its own tree', () => {
  const fsx = fakeFs({
    [`${R}/App/App.csproj`]: '<Project Sdk="Microsoft.NET.Sdk"></Project>',
    [`${R}/App/Main.spek`]: 'actor A {}',
  });
  assert.equal(hasSpekEvidence(`${R}/App/bin/Debug/net10.0/App.dll`, fsx), true);
});

test('evidence: a pure-C# harness referencing a Spek library (elevators shape)', () => {
  const fsx = fakeFs({
    [`${R}/d/Harness/Harness.csproj`]:
      '<Project Sdk="Microsoft.NET.Sdk"><ItemGroup><ProjectReference Include="..\\Lib.Spek\\Lib.Spek.csproj" /></ItemGroup></Project>',
    [`${R}/d/Harness/Program.cs`]: '// no spek here',
    [`${R}/d/Lib.Spek/Lib.Spek.csproj`]: '<Project Sdk="Microsoft.NET.Sdk"></Project>',
    [`${R}/d/Lib.Spek/Lib.spek`]: 'actor A {}',
  });
  assert.equal(hasSpekEvidence(`${R}/d/Harness/bin/Debug/net10.0/Harness.dll`, fsx), true);
});

test('evidence: a plain C# project is not Spek', () => {
  const fsx = fakeFs({
    [`${R}/plain/Plain.csproj`]: '<Project Sdk="Microsoft.NET.Sdk"></Project>',
    [`${R}/plain/Program.cs`]: 'class P {}',
  });
  assert.equal(hasSpekEvidence(`${R}/plain/bin/Debug/net10.0/Plain.dll`, fsx), false);
});

test('evidence: no project anywhere above the program is not Spek', () => {
  const fsx = fakeFs({ [`${R}/misc/readme.txt`]: 'hi' });
  assert.equal(hasSpekEvidence(`${R}/misc/tool.dll`, fsx), false);
});

test('evidence (real tree): Elevators.Harness counts as a Spek project', (t) => {
  // out/test/ -> out -> vscode-spek -> tools -> <repo root>
  const repoRoot = path.resolve(__dirname, '..', '..', '..', '..');
  const harnessDir = path.join(repoRoot, 'demos', 'elevators', 'Elevators.Harness');
  if (!fs.existsSync(harnessDir)) {
    t.skip('demos/elevators not present in this checkout');
    return;
  }
  const dll = path.join(harnessDir, 'bin', 'Debug', 'net10.0', 'Elevators.Harness.dll');
  assert.equal(hasSpekEvidence(dll, nodeFs), true);
});

// ---------- frozen-sampler stall ----------

test('isSampleStalled: no sample yet is not a stall', () => {
  assert.equal(isSampleStalled(10_000, undefined), false);
});

test('isSampleStalled: fresh samples are not a stall, old ones are', () => {
  assert.equal(isSampleStalled(10_000, 9_000, 3_000), false);
  assert.equal(isSampleStalled(12_000, 9_000, 3_000), true);
  assert.equal(isSampleStalled(13_000, 9_000, 3_000), true);
});

test('withDiagnosticsEnabled injects defaults and lets explicit values win', () => {
  const { withDiagnosticsEnabled } = require('../run/debugDecision');
  assert.deepStrictEqual(withDiagnosticsEnabled(undefined), {
    COMPlus_EnableDiagnostics: '1', DOTNET_EnableDiagnostics: '1',
  });
  const merged = withDiagnosticsEnabled({ COMPlus_EnableDiagnostics: '0', FOO: 'bar' });
  assert.strictEqual(merged.COMPlus_EnableDiagnostics, '0');   // user's word wins
  assert.strictEqual(merged.DOTNET_EnableDiagnostics, '1');
  assert.strictEqual(merged.FOO, 'bar');
});
