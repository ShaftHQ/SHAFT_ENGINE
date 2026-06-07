# Claude Code Guidance

@docs/ai/context.md

`AGENTS.md` and `docs/ai/` are the single source of truth for AI development guidance in this repository. `docs/ai/context.md` is the only default import; do not import any other document or skill by default.

## Read conditionally
Load an additional document only when the current task matches its condition:
- Architecture, modules, data flow, deployment: `docs/ai/architecture.md`
- Implementation conventions: `docs/ai/coding-standards.md`
- Behavior changes and tests: `docs/ai/testing-policy.md`
- Authentication, permissions, data isolation, logging, PII, admin behavior, integrations: `docs/ai/security-policy.md`
- Releases or production impact: `docs/ai/release-policy.md`
- Reviews: `docs/ai/code-review.md`
- Unclear project terminology: `docs/ai/glossary.md`

Do not load skills by default. Use only the one relevant workflow skill when the task matches:
- Backend/framework: `.agents/skills/backend-change/SKILL.md`
- Frontend/dashboard: `.agents/skills/frontend-change/SKILL.md`
- Bug fix: `.agents/skills/bug-fix/SKILL.md`
- Code review: `.agents/skills/code-review/SKILL.md`
- Release check: `.agents/skills/release-check/SKILL.md`

## Work rules
- Do not scan the full repository unless explicitly required for the task or safety.
- Inspect only impacted modules and nearby callers, tests, properties, and documentation.
- Make the smallest safe change; follow existing patterns.
- Do not broadly refactor, remove behavior, or break public API compatibility unless explicitly requested.
- Do not hardcode customer-, tenant-, or environment-specific behavior.
- Do not bypass permissions, reporting/audit behavior, validation, or security checks.
- Never expose secrets, tokens, credentials, private test data, or sensitive logs/reports.
- Use SHAFT's properties layer rather than direct `System.getProperty()` calls in framework code.
- Do not change database schemas without migrations and rollback notes; confirm scope because no application migration system was found.
- Do not change dependencies, versions, generated files, or release/deployment configuration unless requested.
- Do not look for path-scoped instruction files; module-specific rules are consolidated in `docs/ai/`.

In the final response, list changed files, what changed, tests run, tests not run, risks, and rollback notes.
