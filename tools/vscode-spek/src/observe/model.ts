// Pure data model for `spekc observe <pid> --json` NDJSON samples.
// No 'vscode' imports here — everything in this file is unit-testable
// with plain Node.
//
// The producer emits one JSON object per line:
//
//   {"system": "...", "actors": [ { ...ActorSnapshot fields... }, ... ]}
//
// ActorSnapshot fields (from the runtime's introspection record): Path,
// ActorType, Behavior, MailboxDepth, MailboxHead, Restarts,
// LastMessageType, SpawnedAt, IsMaterialized, IsStopped, Children.
// The parser below is deliberately defensive: it accepts PascalCase or
// camelCase keys, tolerates missing fields with sensible defaults, and
// skips lines that aren't valid JSON objects (a producer that doesn't
// support --json prints a human table instead — those lines must not
// crash the session, just register as malformed).

/** One actor's normalized snapshot. */
export interface ActorInfo {
  path: string;
  actorType: string;
  behavior: string | null;
  mailboxDepth: number;
  mailboxHead: string[];
  dispatchedCount: number;
  restarts: number;
  lastMessageType: string | null;
  spawnedAt: string | null;
  isMaterialized: boolean;
  isStopped: boolean;
  children: string[];
  /**
   * Milliseconds the current handler has been running, when the producer
   * includes a watchdog-style field. The snapshot record doesn't carry
   * this today; several plausible spellings are probed so the panel
   * lights up automatically if the CLI starts emitting one.
   */
  handlerRunningForMs: number | null;
}

/** One `{"system": ..., "actors": [...]}` sample line, normalized. */
export interface ObserveSample {
  system: string;
  actors: ActorInfo[];
}

/** Case-tolerant property lookup: tries the given names, then camelCase variants. */
function pick(obj: Record<string, unknown>, ...names: string[]): unknown {
  for (const name of names) {
    if (name in obj) return obj[name];
    const camel = name.charAt(0).toLowerCase() + name.slice(1);
    if (camel in obj) return obj[camel];
  }
  return undefined;
}

function asString(v: unknown): string | null {
  return typeof v === 'string' ? v : null;
}

function asNumber(v: unknown): number | null {
  return typeof v === 'number' && Number.isFinite(v) ? v : null;
}

function asBool(v: unknown, fallback: boolean): boolean {
  return typeof v === 'boolean' ? v : fallback;
}

function asStringArray(v: unknown): string[] {
  if (!Array.isArray(v)) return [];
  return v.filter((x): x is string => typeof x === 'string');
}

/** Normalizes one raw actor object; returns null when there's no usable identity. */
export function normalizeActor(raw: unknown): ActorInfo | null {
  if (typeof raw !== 'object' || raw === null || Array.isArray(raw)) return null;
  const obj = raw as Record<string, unknown>;

  const path = asString(pick(obj, 'Path'));
  if (!path) return null; // an actor with no identity can't be rendered or correlated

  const spawnedAtRaw = pick(obj, 'SpawnedAt');
  const runningRaw =
    asNumber(pick(obj, 'HandlerRunningForMs', 'RunningForMs', 'HandlerRunningMs', 'CurrentHandlerMs'));

  return {
    path,
    actorType: asString(pick(obj, 'ActorType')) ?? '(unknown)',
    behavior: asString(pick(obj, 'Behavior')),
    mailboxDepth: asNumber(pick(obj, 'MailboxDepth')) ?? 0,
    mailboxHead: asStringArray(pick(obj, 'MailboxHead')),
    dispatchedCount: asNumber(pick(obj, 'DispatchedCount')) ?? 0,
    restarts: asNumber(pick(obj, 'Restarts')) ?? 0,
    lastMessageType: asString(pick(obj, 'LastMessageType')),
    spawnedAt: asString(spawnedAtRaw),
    isMaterialized: asBool(pick(obj, 'IsMaterialized'), true),
    isStopped: asBool(pick(obj, 'IsStopped'), false),
    children: asStringArray(pick(obj, 'Children')),
    handlerRunningForMs: runningRaw,
  };
}

/**
 * Parses one NDJSON line into a sample. Returns null for anything that
 * isn't a JSON object with an actor list — blank lines, human-readable
 * table output, partial writes.
 */
export function parseSampleLine(line: string): ObserveSample | null {
  const trimmed = line.trim();
  if (trimmed.length === 0 || trimmed[0] !== '{') return null;

  let raw: unknown;
  try {
    raw = JSON.parse(trimmed);
  } catch {
    return null;
  }
  if (typeof raw !== 'object' || raw === null || Array.isArray(raw)) return null;
  const obj = raw as Record<string, unknown>;

  const actorsRaw = pick(obj, 'Actors');
  if (!Array.isArray(actorsRaw)) return null;

  const actors: ActorInfo[] = [];
  for (const a of actorsRaw) {
    const info = normalizeActor(a);
    if (info) actors.push(info);
  }

  return {
    system: asString(pick(obj, 'System', 'SystemName')) ?? '(unnamed system)',
    actors,
  };
}

/**
 * Accumulates stream chunks and yields complete lines. NDJSON producers
 * write a line per sample, but stdout chunks split anywhere — this
 * rejoins them. Handles \n and \r\n.
 */
export class NdjsonLineBuffer {
  private pending = '';

  /** Feed a chunk; returns the complete lines it finished. */
  feed(chunk: string): string[] {
    this.pending += chunk;
    const lines = this.pending.split('\n');
    this.pending = lines.pop() ?? '';
    return lines.map((l) => (l.endsWith('\r') ? l.slice(0, -1) : l));
  }

  /** Any trailing text that never got its newline (call at stream end). */
  flush(): string | null {
    const rest = this.pending;
    this.pending = '';
    return rest.length > 0 ? rest : null;
  }
}

/** Strips CLR generic arity (`Cache\`1` → `Cache`) so live types match source names. */
export function bareTypeName(actorType: string): string {
  const tick = actorType.indexOf('`');
  return tick >= 0 ? actorType.slice(0, tick) : actorType;
}

/** Per-actor-type aggregate for the live CodeLens. */
export interface TypeStats {
  instances: number;
  mailboxDepth: number;
  restarts: number;
  lastMessageType: string | null;
  anyStopped: boolean;
  allStopped: boolean;
}

/**
 * Groups a sample's actors by bare type name for CodeLens lookup.
 * Mailbox and restarts sum across instances; the last message comes from
 * the busiest instance (deepest mailbox) as the most interesting one.
 */
export function statsByType(actors: ActorInfo[]): Map<string, TypeStats> {
  const byType = new Map<string, TypeStats & { _busiestDepth: number }>();
  for (const a of actors) {
    const name = bareTypeName(a.actorType);
    if (name === '(unknown)' || name === '(unmaterialized)') continue;
    let s = byType.get(name);
    if (!s) {
      s = {
        instances: 0, mailboxDepth: 0, restarts: 0, lastMessageType: null,
        anyStopped: false, allStopped: true, _busiestDepth: -1,
      };
      byType.set(name, s);
    }
    s.instances += 1;
    s.mailboxDepth += a.mailboxDepth;
    s.restarts += a.restarts;
    s.anyStopped ||= a.isStopped;
    s.allStopped &&= a.isStopped;
    if (a.lastMessageType !== null && a.mailboxDepth > s._busiestDepth) {
      s.lastMessageType = a.lastMessageType;
      s._busiestDepth = a.mailboxDepth;
    } else if (s.lastMessageType === null && a.lastMessageType !== null) {
      s.lastMessageType = a.lastMessageType;
    }
  }
  const result = new Map<string, TypeStats>();
  for (const [k, v] of byType) {
    const { _busiestDepth, ...stats } = v;
    result.set(k, stats);
  }
  return result;
}

/** A `{"error": "..."}` line from `spekc observe --json` — the CLI's
 *  machine-readable failure report (attach failure, unknown actor path,
 *  session ended). Returns the message, or null if the line isn't one. */
export function parseErrorLine(line: string): string | null {
  try {
    const obj = JSON.parse(line);
    if (obj && typeof obj === 'object' && typeof obj.error === 'string') return obj.error;
  } catch { /* not JSON */ }
  return null;
}
