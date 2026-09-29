---
name: git-cleanup
description: Use when a git worktree is dirty or another local branch or worktree exists. Ask before cleanup unless the session is unattended.
---

# Git cleanup

One checkout. Only the default branch. `HEAD` equals the remote default branch. Clean status.

## When

The worktree is dirty, or another local branch or linked worktree exists.

Attended: ask first, and explain this procedure before changing anything.
Unattended: decide and run it.

## Procedure

1. Fetch `origin`.
2. Classify each dirty or untracked path as land (commit, push, pull request, merge), delete, or gitignore.
3. Remove every local branch and linked worktree except the default branch.
4. Check out the default branch and fast-forward it to the remote.

## Done

One checkout, only the default branch, `HEAD` equals the remote default branch, and `git status` is clean.

## Limits

Do not stash. Do not `reset --hard`. Do not force-push. Do not commit generated plugin directories.
