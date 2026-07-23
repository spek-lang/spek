// Build-time generator for the bundled CE-code catalog.
//
//   npm run compile
//   node out/tools/generateErrorCatalog.js <path-to-errors-reference.md> [output.json]
//
// The input is the Spek documentation's error-codes reference (the
// markdown behind /reference/errors/). The output — errors/catalog.json
// in this extension, checked in — is what ships in the .vsix, so the
// extension explains CE codes fully offline with no dependency on the
// docs site or any other repository. Re-run after the reference gains
// or changes a code.

import * as fs from 'fs';
import * as path from 'path';
import { parseErrorsMarkdown } from './catalogParser';

function main(argv: string[]): number {
  const input = argv[0];
  if (!input) {
    console.error(
      'Usage: node out/tools/generateErrorCatalog.js <path-to-errors-reference.md> [output.json]',
    );
    return 1;
  }
  const defaultOut = path.join(__dirname, '..', '..', 'errors', 'catalog.json');
  const output = argv[1] ?? defaultOut;

  const md = fs.readFileSync(input, 'utf8');
  const catalog = parseErrorsMarkdown(md);
  const codeCount = Object.keys(catalog.codes).length;
  if (codeCount === 0) {
    console.error(`No CE sections found in ${input} — is this the error-codes reference?`);
    return 1;
  }

  const payload = {
    $comment:
      'Generated from the Spek error-codes reference (docs/reference/errors.md). ' +
      'Do not edit by hand — regenerate with: node out/tools/generateErrorCatalog.js <errors.md>',
    codes: catalog.codes,
  };

  fs.mkdirSync(path.dirname(output), { recursive: true });
  fs.writeFileSync(output, JSON.stringify(payload, null, 2) + '\n', 'utf8');
  console.log(`Wrote ${codeCount} CE codes to ${output}`);
  return 0;
}

process.exitCode = main(process.argv.slice(2));
