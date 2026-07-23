// Pure helpers behind the live actor CodeLens: declaration scanning and
// lens-title formatting. No 'vscode' imports — unit-testable.

import { TypeStats } from './model';

/**
 * Matches `actor Name` declarations, with optional visibility and
 * `abstract` modifiers (mirrors the grammar's
 * `visibility? ABSTRACT? ACTOR IDENTIFIER`).
 */
export const ACTOR_DECL_RE =
  /^[ \t]*(?:(?:public|internal|protected|private)\s+)?(?:abstract\s+)?actor\s+([A-Za-z_][A-Za-z0-9_]*)/gm;

/** Finds `actor X` declarations in a document's text. */
export function findActorDeclarations(text: string): Array<{ name: string; index: number }> {
  const out: Array<{ name: string; index: number }> = [];
  ACTOR_DECL_RE.lastIndex = 0;
  let m: RegExpExecArray | null;
  while ((m = ACTOR_DECL_RE.exec(text)) !== null) {
    out.push({ name: m[1], index: m.index });
  }
  return out;
}

/** Formats the CodeLens title for one actor type's live stats. */
export function lensTitle(stats: TypeStats): string {
  const parts: string[] = [];
  if (stats.instances > 1) parts.push(`${stats.instances} instances`);
  parts.push(`mailbox ${stats.mailboxDepth}`);
  parts.push(`restarts ${stats.restarts}`);
  parts.push(`last: ${stats.lastMessageType ?? '-'}`);
  const suffix = stats.allStopped ? ' (stopped)' : '';
  return `● ${parts.join(' · ')}${suffix}`;
}
