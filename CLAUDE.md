# CLAUDE.md

@AGENTS.md

## Claude Adapter

- Treat imported `AGENTS.md` as canonical; do not restate it or append logs.
- Follow the imported new-task flow for branch cleanup, fresh `origin/main`
  branches, ready PRs, and PR links.
- Load one matching `.agents/skills/` bridge only when its trigger applies;
  use `.claude/skills/graphify` for native Graphify discovery.
- Load `.claude/skills/act-as-fable` for nontrivial tasks; prefer the
  minimal working change (YAGNI; user-level `ponytail` when installed).
- Keep plans and final responses proportional to the task and stop when the
  requested behavior is verified.
