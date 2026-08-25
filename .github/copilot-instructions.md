# SHAFT_ENGINE Copilot Instructions

Follow `AGENTS.md` as the canonical repository policy.

- Load `.agents/skills/chaos-engine/SKILL.md`; scoped instruction and skill
  files are adapters to its internal playbooks.
- Prefer targeted reads and deterministic local checks; reuse valid evidence.
- Preserve unrelated work and never expose secrets or claim unverified remote
  results.
<!-- CHAOSENGINE:START -->
Before every task, follow the canonical [ChaosEngine](../.chaos-engine/skills/chaos-engine/SKILL.md). Use `../.chaos-engine/tool.py` for the project-local Memory, MemPalace, and Graphify tools.
<!-- CHAOSENGINE:END -->
