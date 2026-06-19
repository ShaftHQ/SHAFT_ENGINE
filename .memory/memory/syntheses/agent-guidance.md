# Agent guidance

Project-specific operating rules for AI coding agents are maintained in:
- `AGENTS.md`

`AGENTS.md` is canonical for both Codex and Claude Code. `CLAUDE.md` imports and adapts it, including the new-task flow for cleaning stale worktrees/branches, updating from `origin/main`, creating a fresh branch, publishing a ready PR when validation passes, and sharing the PR link.

Keep this synthesis focused on where agent instructions live and when they should be consulted. Coding conventions and verification commands from those files are represented in separate convention and workflow memory.

Update this synthesis when agent instruction files are added, removed, renamed, or replaced.