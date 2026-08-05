---
name: retrieve-first
description: >-
  Knowledge retrieval gate. Use at session start and before broad manual
  discovery, at the depth the entrypoint's triage already selected; also
  carries the completion half that keeps the stores from drifting.
---

# Retrieve

You are here because the work has earned a look at what is already known.
Three stores answer three different questions, and none of them answers
another's.

## Depth comes from triage, not from reflex

The entrypoint already computed blast radius and reversibility. Read retrieval
depth off that answer rather than judging it twice.

| Triage result | Retrieve |
| --- | --- |
| One file, reversible | Main thread: nothing; SessionStart injected standing constraints. Delegates: use `memory search` before discovery, because SessionStart does not reach them. |
| One module, reversible | `memory search "<subsystem>"`. Add MemPalace when the change touches history you were not present for. |
| Public contract, many callers, or hard to reverse | All three, then verify every hit against the live file. |

The first row is the load-bearing one. A rule that demands three queries before
a typo is a rule nobody keeps, and the harness would rather have a coarse rule
everyone follows than a thorough one everyone routes around.

## What each store is for

| Question you actually have | Store |
| --- | --- |
| Has this constraint or gotcha already bitten us? | native Memory — `memory search`, then `memory inspect <id>` |
| What happened around this before, and what does it touch? | MemPalace |
| What calls or depends on this? | [Graphify](../act-as-mohab/references/graphify.md) |
| What does the code do right now? | targeted `rg` and exact reads |

Only the last one settles a disagreement. A retrieved claim is a lead: confirm
it against the file on disk before acting, and never let an index outrank what
is written there today. Your own plan ranks below all of them — it is the
oldest thing you hold, written before the reads.

`memory load` compiles a whole context pack and will exceed a tool-result
budget on a store of any size; `memory search` is the one to reach for.

## Before discovery, not after

The point is the ordering. Querying a store after `rg` has already answered the
question costs the same tokens and buys nothing, because by then the expensive
part — deciding where to look — is done. The stores are worth most when they
tell you a path is a dead end before you walk it.

## The completion half

The [learning loop](../act-as-mohab/references/work-github-playbook.md#learned-lessons-workflow)
routes each learning to its home. That is where a *fact* goes. It is not what
keeps the stores usable:

- `memory remember` for a durable fact or decision, with the evidence.
- Flag a Graphify refresh when the change alters what calls or depends on what.
  A structure graph that describes last week's tree is worse than none, because
  it reads as current.
- Mine the session into MemPalace when it produced relations spanning entities
  or sessions.

Nothing durable is a valid result. Say so; do not manufacture an entry.

## Degraded mode

Configuration is source-controlled and travels; a built index does not, and
generated indexes are never committed. A machine with no MCP servers still has
the injected constraints, which is the floor rather than a failure.

If a store whose trigger fired is unavailable, **name the degraded mode in your
report** and continue with the rest. Skipping a store silently is the failure
this gate exists to prevent — and the one that actually happened, which is why
the gate is a skill the entrypoint names rather than a paragraph inside a file
you reach only after routing.
