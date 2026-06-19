# CLAUDE.md

@AGENTS.md

## Claude Adapter

- Treat imported `AGENTS.md` as canonical; do not restate it or append logs.
- Follow the imported new-task flow for branch cleanup, fresh `origin/main`
  branches, ready PRs, and PR links.
- Load one matching `.agents/skills/` bridge only when its trigger applies.
- Keep plans and final responses proportional to the task and stop when the
  requested behavior is verified.

<!-- memory:start -->
## Memory

- Use `AGENTS.md` as the canonical Memory guidance for this repo.
- CLI fallback: `memory load "<task>"`; save durable changes with `memory remember --stdin`.
<!-- memory:end -->
