// Pure parser that turns the Spek error-codes reference markdown into
// the bundled CE catalog. Used by the build-time generator
// (out/tools/generateErrorCatalog.js) and unit tests. No 'vscode'
// imports.

export interface ErrorEntry {
  code: string;
  /** Lifecycle from the reference's status table: active | reserved | retired. */
  status: string | null;
  /** One-line "what it catches" summary from the status table. */
  summary: string | null;
  /** Full markdown body of the code's section, heading stripped. */
  body: string;
}

export interface ErrorCatalog {
  codes: Record<string, ErrorEntry>;
}

/** Removes markdown decorations that don't survive extraction (links, strikethrough). */
function plainify(text: string): string {
  return rewriteLinks(text).replace(/~~/g, '').trim();
}

/**
 * Rewrites markdown links for standalone rendering: http(s) targets are
 * kept, while intra-document anchors and site-relative paths (which
 * can't resolve inside the editor) collapse to their link text.
 */
export function rewriteLinks(text: string): string {
  return text.replace(
    /\[([^\]]*)\]\(([^)\s]+)\)/g,
    (whole, label: string, target: string) =>
      /^https?:\/\//i.test(target) ? whole : label,
  );
}

/**
 * Applies a transform to prose only, leaving fenced code blocks
 * untouched — a ```spek example containing brackets must reach the
 * reader byte-for-byte.
 */
export function outsideCodeFences(text: string, transform: (prose: string) => string): string {
  const lines = text.split('\n');
  const out: string[] = [];
  let inFence = false;
  let prose: string[] = [];
  const flush = () => {
    if (prose.length > 0) {
      out.push(transform(prose.join('\n')));
      prose = [];
    }
  };
  for (const line of lines) {
    if (/^\s*(```|~~~)/.test(line)) {
      if (!inFence) flush();
      inFence = !inFence;
      out.push(line);
    } else if (inFence) {
      out.push(line);
    } else {
      prose.push(line);
    }
  }
  flush();
  return out.join('\n');
}

/** Parses `| [CE0001](#ce0001) | active | summary |` status-table rows. */
function parseStatusTable(md: string): Map<string, { status: string; summary: string }> {
  const rows = new Map<string, { status: string; summary: string }>();
  for (const line of md.split('\n')) {
    if (!line.trimStart().startsWith('|')) continue;
    const cells = line.split('|').map((c) => c.trim());
    // ['', code cell, status cell, summary cell, ''] — tolerate extra cells.
    if (cells.length < 4) continue;
    const codeMatch = /CE\d{4}/.exec(cells[1]);
    if (!codeMatch) continue;
    const status = plainify(cells[2]).toLowerCase();
    if (!/^[a-z]+$/.test(status)) continue; // separator or header row
    rows.set(codeMatch[0], { status, summary: plainify(cells[3]) });
  }
  return rows;
}

/**
 * Parses the full reference markdown: the status table for one-liners
 * plus every `## CEXXXX` section for the teaching text.
 */
export function parseErrorsMarkdown(md: string): ErrorCatalog {
  const table = parseStatusTable(md);
  const codes: Record<string, ErrorEntry> = {};

  const headingRe = /^## +(CE\d{4})\b.*$/gm;
  const headings: Array<{ code: string; start: number; bodyStart: number }> = [];
  let m: RegExpExecArray | null;
  while ((m = headingRe.exec(md)) !== null) {
    headings.push({ code: m[1], start: m.index, bodyStart: m.index + m[0].length });
  }

  for (let i = 0; i < headings.length; i++) {
    const h = headings[i];
    // The section runs to the next `## ` heading of any kind, not just CE ones.
    const nextHeading = md.indexOf('\n## ', h.bodyStart);
    const end = nextHeading >= 0 ? nextHeading : md.length;
    const rawBody = md.slice(h.bodyStart, end).replace(/^\s*\n/, '').trimEnd();
    const body = outsideCodeFences(rawBody, rewriteLinks);
    const meta = table.get(h.code);
    codes[h.code] = {
      code: h.code,
      status: meta?.status ?? null,
      summary: meta?.summary ?? null,
      body,
    };
  }

  // Codes present in the table but with no section still get an entry so
  // "Explain CE0030" says something useful about reserved codes.
  for (const [code, meta] of table) {
    if (!(code in codes)) {
      codes[code] = { code, status: meta.status, summary: meta.summary, body: '' };
    }
  }

  return { codes };
}
