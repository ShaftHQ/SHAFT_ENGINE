---
name: agentic-pdca-loop
description: Use for SHAFT PDCA, Kevin/Bob/Bruce roles, or refinement loops.
---

# PDCA

Each activity uses exactly one isolated persona: Kevin plans spec/value/acceptance/risks/Mermaid/wireframe; Bob implements the smallest cross-platform change/tests; Bruce checks E2E evidence/defects/ambiguity/confidence. Do not merge roles.

Run PDCA in Kevin->Bob->Bruce order, then repeat that order for two passes: quality/simplicity, then intuitiveness/acceptability. Rerun the smallest check each pass. Stop at >=90% confidence or a blocker.

## Model Tiers

Kevin plans on Fable (Opus if unavailable); an Opus dispatcher splits multi-task plans (Sonnet if unavailable). Bob implements on Haiku. Bruce runs on Sonnet: he owns the task, judges the diff plus real checks per `AGENTS.md` Validation (never Bob's self-report; hunt stubs and weakened assertions), and after at most three Bob rounds closes remaining gaps himself. Each persona spawn starts with a clean context; pass state via files or structured output, and record the model that actually ran. Saved workflows: `.claude/workflows/shaft-bug-fix.js`, `.claude/workflows/shaft-release-ci-fix.js`; bypass per `AGENTS.md` Agent Hierarchy. Decision: the marketplace `ralph-loop` plugin stays unadopted here -- an unbounded same-context Stop-hook loop invites token/CPU runaways given this repo's Maven fork gotchas; these capped, observable workflows are the sanctioned loop.
