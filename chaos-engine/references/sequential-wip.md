# Sequential WIP (one item at a time)

## Rule

Default mode: **one** in-progress item per bot / stream. Finish build →
independent adversarial review → sign-off → lessons before starting the next.
Workers implement; a separate worker reviews; the lead signs and merges.
Re-review returns to the reviewer who raised the blocker.

Human-only blockers route to the orchestrator contact as **NEEDS-HUMAN** (or
the host's equivalent). Bots do not bypass radio-silence charters.

## Why

Parallel WIP without isolation multiplies conflicts, stale boards, and missed
retros. Sequential mode keeps evidence and ownership clear.

## Check

Board / queue shows at most one In progress row per bot. Closed items have a
retro note. Keep-alive still dispatches the next item when the current one
ends ([worker keep-alive](worker-keep-alive.md)).

## Follow-up

[multi-agent PDCA](multi-agent-pdca.md), [parallel streams](parallel-streams.md)
(when explicit parallel is authorized), [roles](roles.md).
