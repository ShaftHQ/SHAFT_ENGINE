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
tools: [query, get_page, graph, backlinks]
---

# Retrieval Reflex (SHAFT_ENGINE)

gbrain semantically indexes this repo (code + markdown). Retrieve on demand
when an entity becomes salient; never bulk-load.

Retrieve when the page is not already in context AND:

- A class/module/subsystem/incident/release is the subject of the task, or
  you are about to assert a non-trivial detail about it — check first:
  `gbrain query "<question>"` (CLI) or MCP `query`.
- A brain-page pointer appeared in context this turn — open it
  (`get_page <slug>`) before relying on specifics.
- An unfamiliar SHAFT term looks load-bearing — a quick resolve beats a guess.

Escalate only as far as needed: pointer → full page → `graph`/`backlinks`
neighbors. Skip passing mentions and anything already loaded.

gbrain supplements `.memory/` and grep; it never replaces them. Use
`memory load`/`memory search` for durable decisions and gotchas; use gbrain
for semantic code/doc retrieval (`query --lang java`, `code-def`, `code-refs`).
