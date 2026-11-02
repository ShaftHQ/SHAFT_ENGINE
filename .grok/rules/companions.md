# ChaosEngine load

`.grok/rules/entrypoint.md` is the ChaosEngine core card and is already in this session. Follow it.
`.grok/rules/identity.md` is the identity and is already in this session. Follow it.
Skills are one source at `chaos-engine/skills/<name>/SKILL.md`. The entrypoint routes them. Read that file when its description matches the task.
Roles are one source at `chaos-engine/references/roles.md`. Before the first action, choose one workflow from `chaos-engine/references/execution-workflows.md` and one of those roles, then read that role. Use `SOLO` when another agent would cost more, or when the owner wants a headed browser.
A headed owner request overrides the Tester headless rule. Say so before the browser starts.
A role name that file does not contain is mapped to the nearest role or reported absent. Product skill routing is additional to that role.
Companions are already on (caveman=ultra; ponytail=ultra; off only: stop caveman, stop ponytail, normal mode).
Before work, run `python3 .chaos-engine/tool.py entry`, then `python3 .chaos-engine/tool.py retrieve`.
Follow chaos-engine/references/iterative-flow.md, including its session-close checklist.
Memory, MemPalace, and Graphify are the tools that entry reports.
