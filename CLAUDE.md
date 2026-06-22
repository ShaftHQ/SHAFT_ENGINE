# CLAUDE.md

@AGENTS.md

## Claude Adapter

- Treat imported `AGENTS.md` as canonical; do not restate it or append logs.
- Follow the imported new-task flow for branch cleanup, fresh `origin/main`
  branches, ready PRs, and PR links.
- Load one matching `.agents/skills/` bridge only when its trigger applies.
- Keep plans and final responses proportional to the task and stop when the
requested behavior is verified.

## gstack

Use gstack for all browser tasks and guided workflow in this repository.

- For web browsing, run `/browse` and avoid using `mcp__claude-in-chrome__*` tools.
- Prefer these active workflow skills before implementation-heavy changes:
  - `/office-hours` for ideation and framing validation
  - `/plan-ceo-review` and `/plan-eng-review` for strategy/architecture planning
  - `/plan-design-review` and `/design-html` for design-heavy work
  - `/review` for pre-land logic and production-readiness checks
  - `/qa` or `/qa-only` for behavior validation
  - `/investigate` for complex defects
  - `/cso` for security review when touching auth/input/privilege flows
  - `/ship` for release candidate checks and PR workflow

## Skill routing

When the user request matches an available skill, invoke it via the Skill tool.
When in doubt, invoke a skill.

- Product ideas/brainstorming → `/office-hours`
- Scope/plan alignment → `/plan-ceo-review`
- Architecture and implementation planning → `/plan-eng-review`
- Design system/plan review → `/plan-design-review`
- Bugs/errors/failures → `/investigate`
- QA/testing site behavior → `/qa` or `/qa-only`
- Security review → `/cso`
- Code review and diff check → `/review`
- Documentation work for shipped changes → `/document-release` or `/document-generate`
- Release readiness → `/ship`
- Save context → `/context-save`
- Resume context → `/context-restore`
- Add routing reminders/observations for future sessions → `/learn`

<!-- memory:start -->
## Memory

- Use `AGENTS.md` as the canonical Memory guidance for this repo.
- CLI fallback: `memory load "<task>"`; save durable changes with `memory remember --stdin`.
<!-- memory:end -->
