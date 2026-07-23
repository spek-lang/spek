// Unit tests for diagnostics-endpoint pid extraction
// (src/observe/diagnosticEndpoints.ts). Only the pure listing parser —
// the quickpick itself needs the extension host.

import { test } from 'node:test';
import * as assert from 'node:assert/strict';
import { pidsFromDiagnosticEndpoints } from '../observe/diagnosticEndpoints';

test('extracts pids from Unix socket names and Windows pipe names', () => {
  const entries = [
    'dotnet-diagnostic-4242-172893-socket', // macOS/Linux socket
    'dotnet-diagnostic-77',                 // Windows pipe form
    'dotnet-diagnostic-4242-999999-socket', // same pid, second endpoint
    'com.apple.launchd.abc',                // unrelated tmpdir noise
    'dotnet-diagnostic-notapid-socket',
    'xdotnet-diagnostic-1-socket',
  ];
  assert.deepEqual(pidsFromDiagnosticEndpoints(entries), [77, 4242]);
});

test('empty listing yields no pids', () => {
  assert.deepEqual(pidsFromDiagnosticEndpoints([]), []);
});
