# GitHub Copilot Entry Point

The single source of truth and authoritative AI instruction set is:

1. `/AGENTS.md` for default/conditional reading and work rules.
2. `/docs/ai/context.md` as the only always-read shared context.
3. Other `/docs/ai/` policies and one `.agents/skills/*/SKILL.md` only when the task matches.

Do not duplicate repository rules here and do not look for path-scoped instruction files. The former `.github/instructions/` guidance has been consolidated into `docs/ai/coding-standards.md` and `docs/ai/testing-policy.md`.
