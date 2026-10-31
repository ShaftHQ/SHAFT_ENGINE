---
name: explicit-staging
description: Use when writing shell scripts or preparing a git commit; require set -e and explicit path staging, never commit -a.
---

# set -e and explicit staging

## Rule

1. Shell scripts that create, move, or delete files start with `set -e` or
   `set -euo pipefail`.
2. Stage commits with explicit paths (`git add path1 path2`). Never
   `git commit -a` or `git add -A` for agent work.
3. If overlay pre-push / pre-commit shows a diff far larger than the planned
   change, **stop**, unstage, and restage only the intended paths.

## Why

A near-miss committed unrelated dirt when `-a` swept the worktree. Fail-fast
shells plus explicit staging keep the blast radius to the plan.
