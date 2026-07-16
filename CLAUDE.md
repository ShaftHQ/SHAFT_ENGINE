# CLAUDE.md

@AGENTS.md

## Claude Adapter

- Imported `AGENTS.md` is canonical (including binding `act-as-fable`);
  do not restate it or append logs.
- Load one matching `.agents/skills/` bridge only when its trigger applies;
  native Graphify via `.claude/skills/graphify`.
- Keep plans and final responses proportional to the task; stop when the
  requested behavior is verified.
