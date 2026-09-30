---
name: git-cleanup
description: Use when a git worktree is dirty or another local branch or worktree exists. Ask before cleanup unless the session is unattended.
---

# Git cleanup

One checkout on the configured default branch. `HEAD` equals that remote tip. `git status` is clean.

## When

The worktree is dirty, or another local branch or linked worktree exists.

Attended: ask first, and explain this procedure before changing anything.
Unattended: decide and run it.

## Procedure

Name the configured default branch from the remote HEAD or the selected profile. Do not hardcode a repository branch name.

1. Fetch the configured upstream.
2. Classify each dirty or untracked path as land (commit, push, pull request, merge), delete, or gitignore.
3. Remove extra local branches and linked worktrees only after that classification. Keep a branch or worktree that still holds a unique commit until that commit is landed or its discard is explicitly authorized.
4. Check out the configured default branch in the primary checkout and fast-forward it so `HEAD` equals the remote tip.

## Done

One checkout, only the configured default branch, `HEAD` equals that remote tip, and `git status` is clean.

## Limits

Do not stash. Do not `reset --hard`. Do not force-push. Do not delete remote branches. Do not discard unique commits without explicit authorization. Do not commit generated plugin directories.
