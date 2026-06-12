# Agent Guidance Maintenance

## Goal

Keep automatically loaded guidance small, stable, and repository-specific.
Executable configuration is authoritative; volatile details and long
procedures belong in runbooks or skills.

## Ownership

| Content | Canonical location |
|---|---|
| Repository identity, global safety, routing, validation | `AGENTS.md` |
| Host-only behavior | `CLAUDE.md`, `.github/copilot-instructions.md` |
| Codex skill discovery metadata | `.agents/skills/*/SKILL.md` |
| Production and test Java rules | `.github/instructions/*.instructions.md` |
| CI, flaky-test, and release workflows | `.github/skills/*/SKILL.md` |
| Architecture, operations, and history | `docs/`, executable config, Git history |

Codex bridge skills contain frontmatter and a link to one canonical rule file.
Do not copy the canonical body into `.agents/skills/`.

## Update Rules

1. Add a durable rule only when it is recurring, repository-specific, and not
   already enforced by executable configuration.
2. Put it in the narrowest owning surface. Keep generic agent behavior out of
   repository guidance.
3. Replace or remove an existing rule instead of adding another version.
4. Keep dates, incidents, volatile versions, long examples, and session logs
   outside automatically loaded files.
5. Use task skills for procedures and path-scoped instructions for Java rules.
6. Update a Codex bridge description when its trigger changes, but keep the
   bridge body minimal.

## Audit

Budgets and routing rules live in `scripts/ci/agent_guidance_budget.json`.

```bash
python3 scripts/ci/validate_agent_guidance.py
python3 -m unittest tests.scripts.test_validate_agent_guidance
git diff --check
```

The audit checks file and host context budgets, both skill roots, local links,
path scopes, stale references, costly mandates, duplicate paragraphs, and the
manual paid-refresh gate.

The refresh workflow runs only by manual dispatch. It invokes paid AI review
only when the deterministic audit fails or a maintainer supplies a reason and
forces review.
