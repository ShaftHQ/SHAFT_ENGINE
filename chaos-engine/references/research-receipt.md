# Research receipt

Phase-gate map: [delivery-phase-gates.md](delivery-phase-gates.md).
Broad research / multi-file explore: [context firewall](context-firewall.md).

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

Every receipt carries one retrieve line:
`retrieve: used|skipped(<reason>)|exempt(harness)`. Instruction-only hosts
(OpenCode, Cursor, Grok Bot, Copilot cloud) have no guard, so this field is
their enforcement; the Learning Session flags a missing field instead of
blocking ([retrieve-first](retrieve-first.md)).

This list is the research receipt. Memory, MemPalace, and Graphify are advisory
for ordinary tasks: store failure records `degraded` and never blocks work.
Missing non-store evidence blocks implementation. Live evidence outranks every
index or recollection; reuse proven solutions before inventing one.
The dated adoption matrix (repo-only `chaos-engine/RESEARCH.md`) records the portable harness
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

## Instruction-only hosts

OpenCode, Cursor and Grok Bot load `AGENTS.md` but run no project hook, so the
`retrieve:` field replaces the read gate. Write the receipt to
`.chaos-engine-state/research-receipt.md` (untracked); `tool.py retrieve`
also fills the retrieve ledger. `learning_session.py finalize --host <id>`
flags `missing-retrieve-receipt` when neither exists; it never blocks (#6201).

Receipt shims (#6218): Copilot cloud opens it in its setup job; install and `repair --component hooks` wire the
[receipt shim](../hooks/receipt_shim.py) for each host with a project marker or
CLI on PATH, and uninstall removes it (#6230); it opens a pending receipt before the first
project read, never blocks, never overwrites, and never fills `retrieve:`.

| Host | Native surface | Verdict |
| --- | --- | --- |
| OpenCode | `.opencode/plugins/` `tool.execute.before` | Shipped: plugin runs the shim before read tools. |
| Cursor | `.cursor/hooks.json` `beforeReadFile`, `beforeShellExecution` | Shipped: hook runs the shim and allows. |
| Grok Bot | cloud box session, no project hook runtime | Receipt sink only. |
| Copilot cloud | `copilot-setup-steps` job on a fresh clone | Shipped: the job runs `receipt_shim.py ensure --host copilot-cloud` before the agent starts; the overlay stays absent (`missing-overlay`). |
