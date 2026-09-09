# Claude adapter

@AGENTS.md

Load [ChaosEngine](.claude/skills/chaos-engine/SKILL.md) before every task.
`AGENTS.md` and its canonical entrypoint own policy; this file adds none.
<!-- CHAOSENGINE:START -->
Before every task, follow the canonical [ChaosEngine](.chaos-engine/skills/chaos-engine/SKILL.md). Use `.chaos-engine/tool.py` for the project-local Memory, MemPalace, and Graphify tools.
<!-- CHAOSENGINE:END -->
