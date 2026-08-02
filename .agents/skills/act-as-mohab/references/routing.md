# Intent routing

Act-as-mohab selects minimum relevant playbooks and tools. Do not make users
name an implementation detail they could not reasonably know.

## Always

Apply the entrypoint's embedded Caveman, Ponytail, and lightweight PDCA rules;
apply its TDD workflow to behavior changes and refactors. Main thread uses
`delegation.md`; each role uses `roles.md`.

## Knowledge and discovery

1. Native Memory for durable constraints/gotchas.
2. MemPalace for cross-session history, relations, and impact RAG.
3. Graphify for current structural map.
4. Targeted `rg`/exact reads to verify live files.

Use every applicable available source. If one fails, name degraded mode and
continue with others. The repo-local Graphify resolver is
`tools/repository-map/resolve_graph_out.py`; use it when that path exists.
Deeper usage lives in [graphify](graphify.md).

## Task routes

- Agent guidance, host adapters, skills, hooks, Memory setup:
  [guidance boundary](playbooks/agent-guidance-boundary-guard.md).
- Explicit PDCA, Kevin/Bob/Bruce, repeated refinement:
  [PDCA](playbooks/agentic-pdca-loop.md).
- Main Java: [framework source](playbooks/framework-source.md).
- Test Java: [Java tests](playbooks/java-tests.md).
- CI failure, flake, release/dependency, MCP transport, module boundary,
  reports, docs, UI, or marketing: matching canonical playbook under
  [playbook index](playbooks/README.md).
- Deep SHAFT internals: [SHAFT mastery](shaft-mastery.md), then only relevant chapter.
- Issue through merged PR: [work GitHub](work-github.md).
- Past-cutoff external library API: context7 MCP; prefer repo exemplar otherwise.
- Live Maven Central facts: Maven MCP; for in-tree dependency declarations,
  inspect POMs first. Never start unavailable infrastructure implicitly.

UI routes preserve native technology: Swing uses IDE/JVM inspection and
renderer evidence; web surfaces use browser evidence and accessibility audit.
