# CLAUDE.md

@AGENTS.md

## Claude Adapter

- Treat imported `AGENTS.md` as canonical; do not restate it or append logs.
- Load one matching `.agents/skills/` bridge only when its trigger applies.
- Keep plans and final responses proportional to the task and stop when the
  requested behavior is verified.
