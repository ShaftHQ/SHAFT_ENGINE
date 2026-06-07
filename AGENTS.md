# AI Agent Operating Rules

## Default reading
Always read only `docs/ai/context.md`. It is the sole always-read shared context file. Do not preload any other document or skill.

## Conditional reading
Read additional guidance only when the current task matches the condition below:
- Architecture, modules, data flow, or deployment: `docs/ai/architecture.md`
- Behavior changes, validation, or tests: `docs/ai/testing-policy.md`
- Authentication, permissions, data isolation, logs, PII, admin behavior, or integrations: `docs/ai/security-policy.md`
- Release or production-impacting changes: `docs/ai/release-policy.md`
- Code review tasks: `docs/ai/code-review.md`
- Unclear domain terms: `docs/ai/glossary.md`
- Implementation conventions: `docs/ai/coding-standards.md`

## Instruction source of truth
This file routes AI work to the task-specific policies under `docs/ai/`. Together, `AGENTS.md` and `docs/ai/` are the single authoritative instruction set for this repository. `CLAUDE.md` and `.github/copilot-instructions.md` are tool entry-point bridges only and must not define competing rules. Path-scoped instruction files are not used; keep module-specific rules in the relevant `docs/ai/` policy and load them conditionally.

## Skill usage
Do not load skills by default. Use only the one relevant skill when the task matches:
- Backend/framework change: `.agents/skills/backend-change/SKILL.md`
- Frontend/dashboard change: `.agents/skills/frontend-change/SKILL.md`
- Bug fix: `.agents/skills/bug-fix/SKILL.md`
- Code review: `.agents/skills/code-review/SKILL.md`
- Release check: `.agents/skills/release-check/SKILL.md`

## Work rules
- Do not perform a full codebase scan unless explicitly required for the task or safety.
- Inspect only impacted modules and nearby related files.
- Make the smallest safe change and follow existing patterns.
- Do not perform broad refactors or remove existing behavior unless explicitly requested.
- Preserve public API backward compatibility; prefer additive overloads and deprecation cycles.
- Do not hardcode customer-specific, tenant-specific, or environment-specific behavior.
- Do not bypass permissions, audit/reporting behavior, validation, or security checks.
- Do not expose secrets, tokens, credentials, private test data, or sensitive report content.
- Do not call `System.getProperty()` directly in framework code; use SHAFT's properties layer.
- Do not change a database schema without a migration and rollback notes. No application schema or migration system was found in this repository, so confirm scope first.
- Do not change dependencies, release versions, generated artifacts, or deployment workflows unless requested.
- For Java source and tests, follow `docs/ai/coding-standards.md` and `docs/ai/testing-policy.md`; these include the former path-scoped rules.

## Final response
Always include:
- Changed files
- What changed
- Tests run
- Tests not run, if any
- Risks
- Rollback notes
