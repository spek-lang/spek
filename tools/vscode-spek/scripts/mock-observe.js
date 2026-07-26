#!/usr/bin/env node
// Mock NDJSON producer for developing the live actor panel before (or
// without) a running Spek process. Emits the same stream shape as
// `spekc observe <pid> --json`: one JSON object per line,
// {"system": ..., "actors": [...]} with ActorSnapshot fields.
//
// To use: set the "spek.spekc.path" setting to this file's absolute
// path (chmod +x first), then run "Spek: Attach to Process" with any
// pid. The script ignores its arguments.

const start = Date.now();
let tick = 0;

function sample() {
  tick++;
  const surge = 20 + Math.round(18 * Math.sin(tick / 4)); // wanders across the warn threshold
  return {
    system: 'checkout-demo',
    actors: [
      {
        Path: 'Storefront',
        ActorType: 'Storefront',
        Behavior: 'Default',
        MailboxDepth: tick % 3,
        MailboxHead: tick % 3 ? ['PlaceOrder'] : [],
        Restarts: 0,
        LastMessageType: 'PlaceOrder',
        SpawnedAt: new Date(start).toISOString(),
        IsMaterialized: true,
        IsStopped: false,
        Children: ['Inventory', 'PaymentGateway', 'Shipping#2'],
      },
      {
        Path: 'Inventory',
        ActorType: 'Inventory',
        Behavior: 'Restocking',
        MailboxDepth: surge,
        MailboxHead: ['Reserve', 'Reserve', 'Reserve', 'Release'],
        Restarts: 1,
        LastMessageType: 'Reserve',
        SpawnedAt: new Date(start).toISOString(),
        IsMaterialized: true,
        IsStopped: false,
        Children: [],
      },
      {
        Path: 'PaymentGateway',
        ActorType: 'PaymentGateway',
        Behavior: 'Default',
        MailboxDepth: 0,
        MailboxHead: [],
        Restarts: 3,
        LastMessageType: 'Charge',
        SpawnedAt: new Date(start).toISOString(),
        IsMaterialized: false, // passivated → dimmed
        IsStopped: false,
        Children: [],
      },
      {
        Path: 'Shipping#2',
        ActorType: 'Shipping',
        Behavior: null,
        MailboxDepth: 0,
        MailboxHead: [],
        Restarts: 0,
        LastMessageType: 'Dispatch',
        SpawnedAt: new Date(start).toISOString(),
        IsMaterialized: true,
        IsStopped: true, // stopped → struck through
        Children: [],
      },
    ],
  };
}

const timer = setInterval(() => {
  process.stdout.write(JSON.stringify(sample()) + '\n');
}, 1000);

process.stdout.write(JSON.stringify(sample()) + '\n');
process.on('SIGTERM', () => { clearInterval(timer); process.exit(0); });
process.on('SIGINT', () => { clearInterval(timer); process.exit(0); });
