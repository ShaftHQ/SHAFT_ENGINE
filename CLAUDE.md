# CLAUDE.md

@AGENTS.md

## Claude Adapter

- Imported `AGENTS.md` is canonical (including binding `act-as-mohab`);
  do not restate it or append logs.
- Read one matching `.agents/skills/<name>/SKILL.md` bridge (not `Skill`-tool
  invocable) only when its trigger applies; native Graphify via `.claude/skills/graphify`.
- Keep plans and final responses proportional to the task; stop when the
  requested behavior is verified.
