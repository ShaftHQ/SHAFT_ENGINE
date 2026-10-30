# Work ordering (close open PRs first)

## Rule

Close all currently open PRs before opening new ones. Ticket priority:

0. Open PRs (including Dependabot)
1. Bugs and issues — broken main, security, and red CI at the top of this band
2. Standalone issues
3. New features
4. Maintenance and code coverage
5. Big epics or revamps

**Exception:** lessons that only change gates fold into an already-open
ChaosEngine PR instead of opening a new PR.

## Why

Open PRs accumulate rebase conflict, stale CI, and review debt. Starting new
work while PRs sit open multiplies WIP and delays delivery. Folding gate-only
lessons into an in-flight CE PR keeps the open-PR count from growing for
process fixes.

## Check

Board / queue shows open PRs ahead of bugs, features, and epics. A new feature
branch is not opened while the bot still owns mergeable open PRs. Gate-only
lesson PRs are not filed separately when a CE PR is already open.

## Follow-up

[sequential WIP](sequential-wip.md), [worker keep-alive](worker-keep-alive.md),
[cleanup at close](cleanup-at-close.md).
