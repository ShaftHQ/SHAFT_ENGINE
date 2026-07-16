---
name: agentic-pdca-loop
description: Use for SHAFT PDCA, Kevin/Bob/Bruce roles, or refinement loops.
---

# PDCA

Each activity uses exactly one isolated persona: Kevin plans spec/value/acceptance/risks/Mermaid/wireframe; Bob implements the smallest cross-platform change/tests; Bruce checks E2E evidence/defects/ambiguity/confidence. Do not merge roles.

Run PDCA in Kevin->Bob->Bruce order, then repeat that order for two passes: quality/simplicity, then intuitiveness/acceptability. Rerun the smallest check each pass. Stop at >=90% confidence or a blocker.

## Execution

All three personas run sequentially on the main thread of one session, never as agents/workflows/orchestrators (per `AGENTS.md` Agent Hierarchy). Switch persona by switching phase: Kevin plans, Bob implements the smallest change, Bruce judges the actual diff plus real checks per `AGENTS.md` Validation (never Bob's self-report; hunt stubs and weakened assertions), closing remaining gaps himself after at most three Bob rounds. Record which phase produced each commit.
