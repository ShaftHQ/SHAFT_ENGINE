---
name: shaft-developer
description: Use when starting any SHAFT testing task to select exactly one lifecycle, implementation, or tool specialist before doing the work.
---

# SHAFT Developer

## Routing

Read [the deterministic routing map](references/routing.md) and load exactly one
next skill: the single row for the current deliverable. Do not open another
specialist in that turn. Return here only when the user asks for a distinct
next deliverable.

A lane with no row is no skill. Desktop automation has no skill. Do not load
`shaft-web-actions` in its place. The CLI of a product under test has no skill.
`shaft-cli` is the SHAFT CLI.

Select the test with Maven Surefire `-Dtest` (`Class#method`, or a
comma-separated class list) or a `suiteXmlFiles` value the project already
owns. Do not add a TestNG XML file.

Never guess SHAFT syntax, tool names, execution evidence, or product behavior.

Bracketed citation keys (for example `ISTQB-CTFL`) in every playbook resolve in
[the shared bibliography](references/sources.md).
