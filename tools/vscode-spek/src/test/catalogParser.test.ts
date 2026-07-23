// Unit tests for the CE-catalog markdown parser (src/tools/catalogParser.ts)
// and the CE-code extraction helpers (src/errors/ceCode.ts).

import { test } from 'node:test';
import * as assert from 'node:assert/strict';
import { outsideCodeFences, parseErrorsMarkdown, rewriteLinks } from '../tools/catalogParser';
import { ceCodesForDiagnostic, ceCodesInText, ceFromCodeField } from '../errors/ceCode';

const SAMPLE_MD = `---
title: Error codes
---

# Compile-time error codes

| Code   | Status     | What it catches |
|--------|------------|-----------------|
| [CE0001](#ce0001) | active     | Syntax error (surfaced by ANTLR) |
| [CE0030](#ce0030) | reserved   | Message not in typed \`ActorRef<IFoo>\` interface |
| [CE0061](#ce0061) | retired    | ~~Old rule~~ — absorbed elsewhere |

## How diagnostics are reported

Not a CE section.

## CE0001

**Trigger:** the parser rejects the source. See [CE0030](#ce0030) and
[the async docs](/language/async/) plus [the site](https://example.org/x).

\`\`\`spek
on Foo f => { bar[0](baz); }   // [not](a-link) inside code
\`\`\`

**Fix:** balance the braces.

## CE0061 (retired)

Absorbed into another check.

## Related reading

Tail matter, not a CE section.
`;

test('parses sections keyed by CE code with table status and summary', () => {
  const catalog = parseErrorsMarkdown(SAMPLE_MD);
  const codes = Object.keys(catalog.codes).sort();
  assert.deepEqual(codes, ['CE0001', 'CE0030', 'CE0061']);

  const ce1 = catalog.codes['CE0001'];
  assert.equal(ce1.status, 'active');
  assert.equal(ce1.summary, 'Syntax error (surfaced by ANTLR)');
  assert.ok(ce1.body.startsWith('**Trigger:**'));
  assert.ok(ce1.body.includes('**Fix:** balance the braces.'));
  assert.ok(!ce1.body.includes('Related reading'));
});

test('a heading suffix like "(retired)" still keys the section by code', () => {
  const catalog = parseErrorsMarkdown(SAMPLE_MD);
  assert.equal(catalog.codes['CE0061'].body, 'Absorbed into another check.');
  assert.equal(catalog.codes['CE0061'].status, 'retired');
});

test('table-only codes (reserved) get an entry with an empty body', () => {
  const catalog = parseErrorsMarkdown(SAMPLE_MD);
  const ce30 = catalog.codes['CE0030'];
  assert.equal(ce30.status, 'reserved');
  assert.equal(ce30.summary, 'Message not in typed `ActorRef<IFoo>` interface');
  assert.equal(ce30.body, '');
});

test('summaries lose strikethrough markers', () => {
  const catalog = parseErrorsMarkdown(SAMPLE_MD);
  assert.equal(catalog.codes['CE0061'].summary, 'Old rule — absorbed elsewhere');
});

test('relative and anchor links collapse to text; http links survive', () => {
  const body = parseErrorsMarkdown(SAMPLE_MD).codes['CE0001'].body;
  assert.ok(body.includes('See CE0030 and'));
  assert.ok(body.includes('the async docs plus'));
  assert.ok(body.includes('[the site](https://example.org/x)'));
});

test('link rewriting leaves fenced code untouched', () => {
  const body = parseErrorsMarkdown(SAMPLE_MD).codes['CE0001'].body;
  assert.ok(body.includes('bar[0](baz);'));
  assert.ok(body.includes('// [not](a-link) inside code'));
});

test('rewriteLinks handles multiple links per line', () => {
  assert.equal(rewriteLinks('[a](#x) then [b](/y/) then [c](http://z)'), 'a then b then [c](http://z)');
});

test('outsideCodeFences round-trips fence markers', () => {
  const md = 'a\n```\ncode\n```\nb';
  assert.equal(outsideCodeFences(md, (s) => s.toUpperCase()), 'A\n```\ncode\n```\nB');
});

test('ceFromCodeField accepts string and {value} shapes only when exact', () => {
  assert.equal(ceFromCodeField('CE0119'), 'CE0119');
  assert.equal(ceFromCodeField(' CE0119 '), 'CE0119');
  assert.equal(ceFromCodeField({ value: 'CE0042', target: 'ignored' }), 'CE0042');
  assert.equal(ceFromCodeField('CS0266'), null);
  assert.equal(ceFromCodeField(119), null);
  assert.equal(ceFromCodeField(undefined), null);
});

test('ceCodesInText finds distinct codes in order', () => {
  assert.deepEqual(
    ceCodesInText('CE0119 fires; see CE0106 (retired into CE0119)'),
    ['CE0119', 'CE0106'],
  );
});

test('ceCodesForDiagnostic prefers the code field, dedupes against message', () => {
  assert.deepEqual(
    ceCodesForDiagnostic('CE0087', 'Reader handler writes state (CE0087); see CE0112'),
    ['CE0087', 'CE0112'],
  );
  assert.deepEqual(ceCodesForDiagnostic(undefined, 'plain message'), []);
});
