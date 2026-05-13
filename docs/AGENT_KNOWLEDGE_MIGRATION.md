# Agent Knowledge Migration Runbook

Use this runbook when an AI-agent session is asked to scan the repository, migrate between agents, consolidate instructions, or preserve repository knowledge for future work.

## Goals

- Preserve repository-specific knowledge without relying on transient chat context.
- Keep global instructions, scoped instructions, skills, and memory ledgers aligned.
- Make migration work traceable through a ticket, branch, commit, and PR.
- Avoid knowledge loss by recording durable lessons in the canonical memory ledger.

## Canonical Knowledge Sources

Start with these repository sources before making implementation decisions:

| Source | Purpose |
|---|---|
| `.github/copilot-instructions.md` | Global AI-agent policy, PDCA workflow, validation requirements, and self-improvement rules. |
| `.github/copilot-memory.md` | Append-only ledger for reusable lessons and decision context. |
| `.github/instructions/*.instructions.md` | Path-scoped rules for framework source and Java test files. |
| `.github/skills/` | Agent/model-specific skills and task playbooks. |
| `CLAUDE.md` | Claude Code operating notes, environment requirements, architecture notes, and command reference. |
| `README.md` and `docs/` | User-facing framework overview, architecture, features, quick start, and technical context. |
| Executable config (`pom.xml`, `.mvn/jvm.config`, workflow files) | Source of truth for versions, build constraints, and CI behavior when prose conflicts. |

## Migration Workflow

Follow this sequence for every non-trivial knowledge migration or instruction consolidation task.

### 1. Create a ticket

Create or link a ticket before implementation. If authenticated GitHub access is unavailable, create a local ticket under `docs/tickets/` using the date and a short slug, then state that it should be mirrored to GitHub Issues by maintainers.

A good ticket includes:

- Problem statement.
- Proposed solution.
- Scope and non-goals.
- Acceptance criteria.
- Branch name.
- Notes about environment limitations.

### 2. Create a task branch

Use a descriptive branch name before editing files, for example:

```bash
git switch -c task/agent-memory-migration-runbook
```

Do not mix migration/documentation work with unrelated code changes.

### 3. Discover instructions, skills, and memory

Perform a focused repository scan:

```bash
find .. -name AGENTS.md -print
rg --files -g '*instruction*' -g '*memory*' -g 'CLAUDE.md' -g '.github/skills/**' .github CLAUDE.md
```

Then read only the files required to understand the task. Prefer progressive disclosure over loading the entire repository into context.

### 4. Reconcile conflicts

When guidance conflicts, apply this precedence order:

1. Direct user/developer/system instructions for the current session.
2. `AGENTS.md` files that scope to the changed files, if present.
3. Path-scoped repository instructions in `.github/instructions/`.
4. Global repository instructions in `.github/copilot-instructions.md` and `CLAUDE.md`.
5. Agent/model-specific skills in `.github/skills/`.
6. General docs and README prose.

Executable configuration wins over stale prose for build versions and runtime constraints. For example, prefer `pom.xml` and enforcer rules over a prose language-version summary if they disagree.

### 5. Consolidate without duplication

- Keep stable policy in `.github/copilot-instructions.md` or `CLAUDE.md`.
- Keep path-specific coding/test rules in `.github/instructions/`.
- Keep task-specific playbooks in `.github/skills/`.
- Keep reusable lessons and rationale in `.github/copilot-memory.md`.
- Move long explanatory workflows to `docs/` and link to them from instruction files.

### 6. Validate and commit

For documentation-only changes, run lightweight checks that prove the edited files are present and traceable. For code changes, follow the mandatory Maven compile/test sequence from the repository instructions.

Suggested documentation checks:

```bash
git status --short
rg -n "Agent Knowledge Migration|knowledge migration|docs/tickets" docs .github CLAUDE.md
```

Commit the final changes on the task branch with a concise message.

### 7. Open a PR

Open a PR when the initial implementation is ready for collaboration and merge review. Prefer a draft PR while the implementation is intentionally incomplete; otherwise open a ready-for-review PR. The PR body should include:

- Summary of files changed.
- Validation commands and outcomes.
- Any environment limitations.
- Follow-up actions, such as mirroring a local ticket into GitHub Issues.

If a session cannot publish to GitHub, it must explicitly report the missing access instead of implying that a GitHub PR exists. Before claiming that a PR is visible on GitHub, verify all of the following:

```bash
git remote -v
command -v gh
gh auth status
```

When any of those checks are unavailable or unauthenticated, create the local PR/handoff artifact required by the execution environment, include the intended PR title/body in the final response, and ask a maintainer to push the branch and open the GitHub PR.

## Session Memory Checklist

Before finishing a migration task, update `.github/copilot-memory.md` with:

- Date.
- Area.
- Trigger.
- Lesson.
- Evidence.
- Action taken.

Keep the memory entry compact and reusable. Do not paste long transcripts or duplicate the runbook.
