# Ticket: Agent Knowledge Migration and Instruction Consolidation

- **Date created:** 2026-05-08
- **Branch:** `task/agent-memory-migration-runbook`
- **Type:** Documentation / AI-agent enablement
- **Status:** Initial implementation committed; GitHub PR publication blocked by missing remote/GitHub CLI access in this environment

## Problem Statement

Agent knowledge for SHAFT_ENGINE is distributed across repository-level instructions, memory ledgers, scoped source/test guidance, agent-specific skills, and general project documentation. New AI-agent sessions need a repeatable way to discover, reconcile, and preserve this knowledge without losing local constraints during migration or handoff.

## Proposed Solution

Create a repository knowledge migration runbook and add durable instruction/memory entries that require future agents to:

1. Discover repository instruction, memory, skill, and tool files before changing code.
2. Reconcile conflicts by favoring executable configuration and path-scoped instructions over stale prose.
3. Record reusable learnings in the canonical memory ledger.
4. Create a task ticket, isolate work on a task branch, and open a PR for collaborative review when a migration/consolidation task is requested; if GitHub publication access is unavailable, provide a PR handoff and say so clearly.

## Scope

- Add a durable runbook under `docs/` for future agent migrations.
- Update global agent instructions to reference the runbook.
- Update Claude guidance so Claude Code sessions follow the same migration workflow.
- Append a compact memory ledger entry preserving the decision context.

## Acceptance Criteria

- [x] Ticket exists in the repository.
- [x] Dedicated implementation branch exists: `task/agent-memory-migration-runbook`.
- [x] Migration runbook documents discovery, consolidation, ticket, branch, and PR workflow.
- [x] Agent instructions and memory ledger reference the process for future reuse.
- [x] Initial implementation is committed and a PR handoff is prepared for human collaboration.
- [x] Missing GitHub publication access is explicitly documented so maintainers know why no PR appears in GitHub.

## Notes for Reviewers

This ticket was created locally because this execution environment does not expose an authenticated GitHub CLI or Git remote. The agent can record PR metadata through the local `make_pr` tool, but cannot publish a visible GitHub PR from this checkout. Maintainers should push `task/agent-memory-migration-runbook`, open the GitHub PR, and mirror/link this ticket in GitHub Issues if desired.
