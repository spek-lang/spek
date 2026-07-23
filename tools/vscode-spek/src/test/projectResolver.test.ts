// Unit tests for run-project resolution (src/run/projectResolver.ts):
// csproj parsing, the owner/harness/include-glob heuristic against an
// in-memory tree, the built-dll convention — and the real-tree proof that
// demos/elevators resolves from Elevators.spek to its harness project.

import { test } from 'node:test';
import * as assert from 'node:assert/strict';
import * as path from 'path';
import * as fs from 'fs';
import {
  builtDllPath,
  includePatternMatches,
  parseCsproj,
  ProjectFs,
  resolveRunProjects,
} from '../run/projectResolver';
import { nodeFs } from '../run/nodeFs';

// ---------- helpers: an in-memory ProjectFs over a { path: content } map ----------

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
      for (const p of paths) {
        if (path.dirname(p) === norm) names.add(path.basename(p));
      }
      for (const d of dirs) {
        if (path.dirname(d) === norm && d !== norm) names.add(path.basename(d));
      }
      return [...names];
    },
    isDirectory(p: string): boolean {
      return dirs.has(path.normalize(p));
    },
    readFile(file: string): string | null {
      return files[path.normalize(file)] ?? files[file] ?? null;
    },
  };
}

const LIB = `<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup><TargetFramework>net10.0</TargetFramework></PropertyGroup>
</Project>`;

const EXE = `<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup><OutputType>Exe</OutputType><TargetFramework>net10.0</TargetFramework></PropertyGroup>
</Project>`;

function exeReferencing(...refs: string[]): string {
  const items = refs.map((r) => `<ProjectReference Include="${r}" />`).join('\n    ');
  return `<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup><OutputType>Exe</OutputType><TargetFramework>net10.0</TargetFramework></PropertyGroup>
  <ItemGroup>
    ${items}
  </ItemGroup>
</Project>`;
}

// ---------- csproj parsing ----------

test('parseCsproj: executable detection, TFM, assembly name, references', () => {
  const meta = parseCsproj(
    `<Project Sdk="Microsoft.NET.Sdk">
      <PropertyGroup>
        <OutputType>Exe</OutputType>
        <TargetFramework>net10.0</TargetFramework>
        <AssemblyName>CustomName</AssemblyName>
      </PropertyGroup>
      <ItemGroup>
        <ProjectReference Include="..\\Lib\\Lib.csproj" />
      </ItemGroup>
    </Project>`,
    '/repo/App/App.csproj',
  );
  assert.equal(meta.isExecutable, true);
  assert.equal(meta.targetFramework, 'net10.0');
  assert.equal(meta.assemblyName, 'CustomName');
  assert.deepEqual(meta.projectReferences, [path.normalize('/repo/Lib/Lib.csproj')]);
});

test('parseCsproj: library defaults — no OutputType, name from file, first of TargetFrameworks', () => {
  const meta = parseCsproj(
    `<Project Sdk="Microsoft.NET.Sdk">
      <PropertyGroup><TargetFrameworks>net10.0;net8.0</TargetFrameworks></PropertyGroup>
    </Project>`,
    '/repo/Lib/Lib.csproj',
  );
  assert.equal(meta.isExecutable, false);
  assert.equal(meta.targetFramework, 'net10.0');
  assert.equal(meta.assemblyName, 'Lib');
});

test('parseCsproj: the Web SDK counts as executable', () => {
  const meta = parseCsproj(`<Project Sdk="Microsoft.NET.Sdk.Web"></Project>`, '/r/W/W.csproj');
  assert.equal(meta.isExecutable, true);
});

// ---------- built-dll convention ----------

test('builtDllPath follows bin/<Configuration>/<TFM>/<AssemblyName>.dll', () => {
  const meta = parseCsproj(EXE, path.normalize('/repo/App/App.csproj'));
  assert.equal(
    builtDllPath(meta),
    path.normalize('/repo/App/bin/Debug/net10.0/App.dll'),
  );
});

test('builtDllPath is null without a TargetFramework', () => {
  const meta = parseCsproj('<Project Sdk="Microsoft.NET.Sdk"></Project>', '/r/A/A.csproj');
  assert.equal(builtDllPath(meta), null);
});

// ---------- include-pattern matching ----------

test('includePatternMatches handles MSBuild-style globs', () => {
  assert.ok(includePatternMatches('**\\*.spek', 'Foo.spek'));
  assert.ok(includePatternMatches('**\\*.spek', 'sub\\dir\\Foo.spek'.replace(/\\/g, path.sep)));
  assert.ok(includePatternMatches('..\\Shared.spek', '../Shared.spek'));
  assert.ok(includePatternMatches('..\\**\\*.spek', '../nested/Foo.spek'));
  assert.ok(!includePatternMatches('..\\Shared.spek', '../Other.spek'));
  assert.ok(!includePatternMatches('*.spek', 'sub/Foo.spek'));
});

// ---------- resolution: fake trees ----------

const R = path.normalize('/ws');

test('resolution: executable owner runs directly', () => {
  const fsx = fakeFs({
    [`${R}/App/App.csproj`]: EXE,
    [`${R}/App/Main.spek`]: 'actor A {}',
  });
  const res = resolveRunProjects(`${R}/App/Main.spek`, R, fsx);
  assert.equal(res.via, 'owner-exe');
  assert.deepEqual(res.candidates, [path.normalize(`${R}/App/App.csproj`)]);
});

test('resolution: library owner resolves to the executable that references it', () => {
  const fsx = fakeFs({
    [`${R}/demo/Lib.Spek/Lib.Spek.csproj`]: LIB,
    [`${R}/demo/Lib.Spek/Lib.spek`]: 'actor A {}',
    [`${R}/demo/Lib.Harness/Lib.Harness.csproj`]: exeReferencing('..\\Lib.Spek\\Lib.Spek.csproj'),
  });
  const res = resolveRunProjects(`${R}/demo/Lib.Spek/Lib.spek`, R, fsx);
  assert.equal(res.via, 'referencing-exe');
  assert.equal(res.owner, path.normalize(`${R}/demo/Lib.Spek/Lib.Spek.csproj`));
  assert.deepEqual(res.candidates, [path.normalize(`${R}/demo/Lib.Harness/Lib.Harness.csproj`)]);
});

test('resolution: transitive harness reference (exe -> mid -> owner) is found', () => {
  const fsx = fakeFs({
    [`${R}/d/Core/Core.csproj`]: LIB,
    [`${R}/d/Core/Core.spek`]: 'actor A {}',
    [`${R}/d/Mid/Mid.csproj`]: `<Project Sdk="Microsoft.NET.Sdk"><ItemGroup><ProjectReference Include="..\\Core\\Core.csproj" /></ItemGroup></Project>`,
    [`${R}/d/App/App.csproj`]: exeReferencing('..\\Mid\\Mid.csproj'),
  });
  const res = resolveRunProjects(`${R}/d/Core/Core.spek`, R, fsx);
  assert.equal(res.via, 'referencing-exe');
  assert.deepEqual(res.candidates, [path.normalize(`${R}/d/App/App.csproj`)]);
});

test('resolution: multiple referencing executables all surface as candidates', () => {
  const fsx = fakeFs({
    [`${R}/d/Lib/Lib.csproj`]: LIB,
    [`${R}/d/Lib/A.spek`]: 'actor A {}',
    [`${R}/d/Run1/Run1.csproj`]: exeReferencing('..\\Lib\\Lib.csproj'),
    [`${R}/d/Run2/Run2.csproj`]: exeReferencing('..\\Lib\\Lib.csproj'),
  });
  const res = resolveRunProjects(`${R}/d/Lib/A.spek`, R, fsx);
  assert.equal(res.candidates.length, 2);
  assert.deepEqual(
    res.candidates.map((c) => path.basename(c)).sort(),
    ['Run1.csproj', 'Run2.csproj'],
  );
});

test('resolution: nearest ancestor level wins over a farther one', () => {
  const fsx = fakeFs({
    [`${R}/x/Lib/Lib.csproj`]: LIB,
    [`${R}/x/Lib/A.spek`]: 'actor A {}',
    [`${R}/x/Near/Near.csproj`]: exeReferencing('..\\Lib\\Lib.csproj'),
    [`${R}/Far/Far.csproj`]: exeReferencing('..\\x\\Lib\\Lib.csproj'),
  });
  const res = resolveRunProjects(`${R}/x/Lib/A.spek`, R, fsx);
  assert.deepEqual(res.candidates, [path.normalize(`${R}/x/Near/Near.csproj`)]);
});

test('resolution: library owner with no referencing exe falls back to the owner', () => {
  const fsx = fakeFs({
    [`${R}/only/Lib.csproj`]: LIB,
    [`${R}/only/A.spek`]: 'actor A {}',
  });
  const res = resolveRunProjects(`${R}/only/A.spek`, R, fsx);
  assert.equal(res.via, 'owner-fallback');
  assert.deepEqual(res.candidates, [path.normalize(`${R}/only/Lib.csproj`)]);
});

test('resolution: no owner — a sibling project that globs the file is found', () => {
  const fsx = fakeFs({
    [`${R}/loose/Shared.spek`]: 'actor A {}',
    [`${R}/loose/Runner/Runner.csproj`]: `<Project Sdk="Microsoft.NET.Sdk">
      <PropertyGroup><OutputType>Exe</OutputType><TargetFramework>net10.0</TargetFramework></PropertyGroup>
      <ItemGroup><SpekCompile Include="..\\Shared.spek" /></ItemGroup>
    </Project>`,
  });
  const res = resolveRunProjects(`${R}/loose/Shared.spek`, R, fsx);
  assert.equal(res.via, 'include-glob');
  assert.deepEqual(res.candidates, [path.normalize(`${R}/loose/Runner/Runner.csproj`)]);
});

test('resolution: nothing anywhere yields no candidates', () => {
  const fsx = fakeFs({ [`${R}/nowhere/A.spek`]: 'actor A {}' });
  const res = resolveRunProjects(`${R}/nowhere/A.spek`, R, fsx);
  assert.equal(res.via, 'none');
  assert.deepEqual(res.candidates, []);
});

// ---------- the elevators proof: the REAL repository tree ----------

// out/test/ -> out -> vscode-spek -> tools -> <repo root>
const repoRoot = path.resolve(__dirname, '..', '..', '..', '..');
const elevatorsSpek = path.join(
  repoRoot, 'demos', 'elevators', 'Elevators.Spek', 'Elevators.spek',
);

test('elevators proof: Elevators.spek resolves to Elevators.Harness in the real tree', (t) => {
  if (!fs.existsSync(elevatorsSpek)) {
    t.skip('demos/elevators not present in this checkout');
    return;
  }
  const res = resolveRunProjects(elevatorsSpek, repoRoot, nodeFs);
  // The owner is the library project that compiles the .spek file…
  assert.equal(
    res.owner,
    path.join(repoRoot, 'demos', 'elevators', 'Elevators.Spek', 'Elevators.Spek.csproj'),
  );
  // …and the runnable candidate is its executable harness, found by walking
  // up one level and scanning for executables that reference the owner.
  assert.equal(res.via, 'referencing-exe');
  assert.deepEqual(res.candidates, [
    path.join(repoRoot, 'demos', 'elevators', 'Elevators.Harness', 'Elevators.Harness.csproj'),
  ]);
});

test('elevators proof: the harness dll path follows the bin convention', (t) => {
  if (!fs.existsSync(elevatorsSpek)) {
    t.skip('demos/elevators not present in this checkout');
    return;
  }
  const harness = path.join(
    repoRoot, 'demos', 'elevators', 'Elevators.Harness', 'Elevators.Harness.csproj',
  );
  const meta = parseCsproj(nodeFs.readFile(harness)!, harness);
  assert.equal(
    builtDllPath(meta),
    path.join(
      repoRoot, 'demos', 'elevators', 'Elevators.Harness',
      'bin', 'Debug', 'net10.0', 'Elevators.Harness.dll',
    ),
  );
});

test('real tree: a directly-executable sample resolves to itself', (t) => {
  const helloBank = path.join(repoRoot, 'samples', 'HelloBank');
  const spekFiles = fs.existsSync(helloBank)
    ? fs.readdirSync(helloBank).filter((f) => f.endsWith('.spek'))
    : [];
  if (spekFiles.length === 0) {
    t.skip('samples/HelloBank not present in this checkout');
    return;
  }
  const res = resolveRunProjects(path.join(helloBank, spekFiles[0]), repoRoot, nodeFs);
  assert.equal(res.via, 'owner-exe');
  assert.equal(res.candidates.length, 1);
  assert.ok(res.candidates[0].endsWith('.csproj'));
  assert.ok(res.candidates[0].startsWith(helloBank));
});
