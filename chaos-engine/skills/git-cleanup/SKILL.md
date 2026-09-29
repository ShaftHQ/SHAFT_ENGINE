---
name: git-cleanup
description: Use when a git worktree is dirty or another local branch or worktree exists. Ask before cleanup unless the session is unattended.
---

# Git cleanup

One checkout. Only `main`. `HEAD` equals `origin/main`. Clean status.

## When

The worktree is dirty, or another local branch or linked worktree exists.

Attended: ask first, and explain this procedure before changing anything.
Unattended: decide and run it.

## Procedure

1. Fetch `origin`.
2. Classify each dirty or untracked path as land (commit, push, pull request, merge), delete, or gitignore.
3. Remove every local branch and linked worktree except `main`.
4. Check out `main` and fast-forward to `origin/main`.

## Done

One checkout, only `main`, `HEAD` equals `origin/main`, and `git status` is clean.

## Limits

Do not stash. Do not `reset --hard`. Do not force-push. Do not commit generated plugin directories.
