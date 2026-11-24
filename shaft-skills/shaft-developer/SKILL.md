---
name: shaft-developer
description: Use when any SHAFT testing task starts, to load exactly one specialist reference before the work.
---

# SHAFT Developer

## Routing

Read [the deterministic routing map](references/routing.md) and load exactly one
specialist reference: the single row for the current deliverable. Do not open a
second specialist in that turn. Return here only when the user asks for a distinct
next deliverable.

A lane with no row is no skill. Desktop automation has no skill. Do not load
`shaft-web-actions` in its place. The CLI of a product under test has no skill.
`shaft-cli` is the SHAFT CLI. `shaft` is an accepted alias of that launcher.

Never guess SHAFT syntax, tool names, execution evidence, or product behavior.

Bracketed citation keys (for example `ISTQB-CTFL`) in every specialist reference
resolve in [the shared bibliography](references/sources.md).
