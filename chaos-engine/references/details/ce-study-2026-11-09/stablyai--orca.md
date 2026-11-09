# stablyai/orca

- URL: https://github.com/stablyai/orca
- Lists: Charlie Hills snapshot, 6 Nov 2026, row 10 (117K stars on that snapshot).
- GitHub description: Orca runs a fleet of coding agents with your own subscription, on desktop, mobile, and a remote runtime.
- README: README headings seen: chat or terminal, orchestrate agents, run anywhere, mobile app, review output, git, parallel worktrees.

## Reusable patterns

- One agent per worktree, with the conversation movable between a chat UI and the raw CLI.
- Review of the diff stays in the same surface as the run.

## Adopt

- ChaosEngine already isolates parallel work by worktree. Keep one writer per worktree on this machine.
- Do not add a desktop, mobile, or remote fleet. A run stays on local tools unless the user approves a named cloud agent or an extra machine.

## Skip

- Do not vendor the Orca app or its remote runtime.
- Do not subscribe ChaosEngine to a hosted agent fleet.

## License check

- Checked 2026-11-10. GraphQL licenseInfo.spdxId is MIT. gh api repos/stablyai/orca/license returned HTTP 200 and path LICENSE.
- No skill edit from this note.

Adopt the idea. Do not copy third-party code. Do not vendor the repository.
