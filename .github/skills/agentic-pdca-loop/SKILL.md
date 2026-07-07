---
name: agentic-pdca-loop
description: Use for SHAFT PDCA, Kevin/Bob/Bruce roles, or refinement loops.
---

# PDCA

Each activity uses exactly one isolated persona: Kevin plans spec/value/acceptance/risks/Mermaid/wireframe; Bob implements the smallest cross-platform change/tests; Bruce checks E2E evidence/defects/ambiguity/confidence. Do not merge roles.

Run PDCA in Kevin->Bob->Bruce order, then repeat that order for two passes: quality/simplicity, then intuitiveness/acceptability. Rerun the smallest check each pass. Stop at >=90% confidence or a blocker.

## Execution

All three personas run sequentially on the main thread of the same session -- never as spawned agents, workflows, or orchestrators (see `AGENTS.md` Agent Hierarchy: workflows are forbidden and `.claude/workflows/` must never be recreated). Switch persona by switching phase: Kevin plans, then Bob implements the smallest change, then Bruce judges the actual diff plus real checks per `AGENTS.md` Validation (never Bob's self-report; hunt stubs and weakened assertions) and closes remaining gaps himself after at most three Bob rounds. Record which phase produced each commit. Decision: the marketplace `ralph-loop` plugin stays unadopted here -- an unbounded same-context Stop-hook loop invites token/CPU runaways given this repo's Maven fork gotchas; the capped main-thread PDCA loop is the sanctioned loop.
