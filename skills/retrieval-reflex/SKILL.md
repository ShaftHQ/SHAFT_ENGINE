---
name: retrieval-reflex
version: 0.1.0
description: When/what to retrieve from gbrain — query the brain before asserting non-trivial repo facts from memory.
triggers:
  - "how does"
  - "where is"
  - "what do we know about"
mutating: false
writes_pages: false
writes_to: []
tools: [query, get_page, traverse_graph, get_backlinks]
---

# Retrieval Reflex (SHAFT_ENGINE)

gbrain semantically indexes this repo (code + markdown). Retrieve on demand
when an entity becomes salient; never bulk-load.

Retrieve when the page is not already in context AND:

- A class/module/subsystem/incident/release is the subject of the task, or
  you are about to assert a non-trivial detail about it — check first:
  `gbrain query "<question>"` (CLI) or MCP `query`.
- A brain-page pointer appeared in context this turn — open it
  (`gbrain get <slug>` CLI; `get_page` MCP) before relying on specifics.
- An unfamiliar SHAFT term looks load-bearing — a quick resolve beats a guess.

Escalate only as far as needed: pointer → full page. Do NOT escalate further
to `gbrain graph`/`backlinks`/`traverse_graph`/`get_backlinks` or
`code-callers`/`code-callees` — verified 2026-07-17 that this brain's link
graph is empty (`gbrain extract links --source db` found 0 links across all
1450 shaft-engine pages) and the tool that would populate it,
`reconcile-links`, is unreachable from the CLI (gbrain issue
[#2900](https://github.com/garrytan/gbrain/issues/2900), fix pending
upstream). `code-def`/`code-refs` (symbol lookup, not graph traversal) still
work fine. For "what depends on/calls/relates to X" structural questions,
use `graphify` instead — its deterministic map doesn't depend on gbrain's
link graph. Re-check this note once #2900 lands; graph/backlink escalation
may become viable again.

gbrain supplements `.memory/` and grep; it never replaces them. Use
`memory load`/`memory search` for durable decisions and gotchas; use gbrain
for semantic code/doc retrieval (`query --lang java`, `code-def`, `code-refs`);
use `graphify` for structural/dependency relationships (division of labor:
`.claude/skills/graphify/SKILL.md`).
