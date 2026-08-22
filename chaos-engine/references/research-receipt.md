# Research receipt

Load this before the first implementation mutation, except mechanical one-file
reversible work: name the eight steps, then record store irrelevance without
querying. Default and most-intelligent work query a store only when it can
shorten the task.

## Implementation preflight

Do these in order:

1. Read live files and current instructions.
2. Load the routed skill and any directly required references.
3. Query native Memory once for a concrete prior constraint or gotcha; otherwise
   record irrelevance.
4. Query MemPalace once for concrete cross-session history or relations;
   otherwise record irrelevance. After a plan is approved, do not start the
   first implementation mutation until this MemPalace attempt has been made
   (`used`, `skipped` with a concrete irrelevance reason, or `degraded`).
5. Query Graphify once for structural leads; live-verify returned paths and use
   targeted `rg` for blast radius, or record irrelevance.
6. Do authoritative online research, preferring current primary documentation,
   standards, and proven upstream implementations. Record source URLs and date.
7. Compare proven approaches, steelman the rejected approach, and choose the
   smallest root-owner fix that preserves the invariants.
8. Record a concrete plan and deferred consolidated proof commands in the issue
   for issue-backed work, otherwise in transient working context.

This list is the research receipt. Memory, MemPalace, and Graphify are advisory
for ordinary tasks: store failure records `degraded` and never blocks work.
Missing non-store evidence blocks implementation. Live evidence outranks every
index or recollection; reuse proven solutions before inventing one.
The dated [adoption matrix](../RESEARCH.md) records the portable harness
baseline; revalidate a row when its relevant discovery, schema, or install
contract changes.

## Planning quality and ownership

Every substantive plan is thorough and decision-ready. Establish the main
objective and reasoning, success criteria, audience, included and excluded
scope, constraints, current state, callers, assumptions, tradeoffs, risks, and
proof before reducing the work to files or steps. Never ask a question the
repository, retrieval stores, or authoritative sources can answer. Then ask
every material question needed for high confidence in the user's intent, and
keep asking follow-ups until the plan is decision-ready. Record the answer, any
remaining unknown, and the evidence behind the confidence level.

Every plan records each store as `used` (scoped query plus verified evidence),
`skipped` (concrete irrelevance), or `degraded` (attempt plus sanitized reason),
and includes dated online research. Legacy query/evidence means `used`. Compare
two complete approaches and steelman the rejected option. Use Mermaid when
dependencies, components, state, or workflows become materially clearer;
otherwise record why a diagram would be decorative. Own implementation of the
plan: after approval, go completely unattended and carry it proactively through
complete implementation, consolidated Check/Act, one independent PR review,
PR delivery, authorized merge, and
scoped cleanup. Do not ask the user for implementation clarifications; dispatch
a consultant agent. HALT only when merge authority was never granted or a new
request contradicts the approved plan, not for approval already granted.

Install and learning contracts live in [dependencies.json](../dependencies.json)
and are enforced by `tests/scripts/test_chaos_engine_installer.py`,
`tests/scripts/test_chaos_engine_bootstrap.py`,
`tests/scripts/test_chaos_engine_dependencies.py`,
`tests/scripts/test_chaos_engine_hosts.py`,
`tests/scripts/test_chaos_engine_learning.py`, and
`tests/scripts/test_chaos_engine_research.py`.
