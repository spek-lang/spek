// Pure CE-code extraction from diagnostic shapes. No 'vscode' imports —
// unit-testable. The Spek language server maps compiler diagnostics to
// LSP with the CE code in the diagnostic's `code` field (a plain string
// like "CE0119") and `source: "spek"`; on the vscode side that arrives
// as Diagnostic.code, which the API types as
// string | number | { value: string | number; target: Uri }.
// The message-text fallback covers any producer that only embeds the
// code in prose.

const CE_RE = /\bCE\d{4}\b/g;

/** Normalizes any diagnostic `code` value to a CE string, or null. */
export function ceFromCodeField(code: unknown): string | null {
  const value =
    typeof code === 'object' && code !== null && 'value' in code
      ? (code as { value: unknown }).value
      : code;
  if (typeof value === 'string') {
    const m = /^CE\d{4}$/.exec(value.trim());
    if (m) return m[0];
  }
  return null;
}

/** All distinct CE codes mentioned in a message string, in order. */
export function ceCodesInText(text: string): string[] {
  const seen = new Set<string>();
  for (const m of text.matchAll(CE_RE)) seen.add(m[0]);
  return [...seen];
}

/**
 * The CE codes one diagnostic refers to: the structured code field
 * first, message-text mentions as fallback, deduplicated.
 */
export function ceCodesForDiagnostic(code: unknown, message: string): string[] {
  const out: string[] = [];
  const structured = ceFromCodeField(code);
  if (structured) out.push(structured);
  for (const c of ceCodesInText(message ?? '')) {
    if (!out.includes(c)) out.push(c);
  }
  return out;
}
