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

Name the configured default branch from the remote HEAD or the selected profile. Do not hardcode a repository branch name. This is the repository-scope recipe in cleanup-scopes. Do not fork a second policy.

1. Fetch and prune the configured upstream. Inventory dirty and untracked paths, extra local branches, linked worktrees, locks, and unique commits before mutation.
2. Classify each dirty or untracked path as land (commit, push, pull request, merge), delete, or gitignore. Halt on a locked or concurrently owned worktree.
3. Check out the configured default branch in the primary checkout and fast-forward it so `HEAD` equals the remote tip and `git status` is clean.
4. Remove extra linked worktrees only after that classification, and only when they are clean, unlocked, and not concurrently owned. Then remove a recreated local branch only when `git branch -r --contains` shows that exact tip on an origin ref. Also remove extra local branches that are fully merged into the remote tip, or whose unique commits were explicitly authorized for discard.
5. Check out the configured default branch and fast-forward it. Write the verification transcript in the same process, after that fast-forward. Do not check out another branch afterward.

## Done

One checkout, only the configured default branch, `HEAD` equals that remote tip, and `git status` is clean.

## Limits

Do not stash. Do not `reset --hard`. Do not force-push. Do not use `--force-with-lease`. Do not delete remote branches. Do not rewrite remote history. Do not discard unique commits without explicit authorization. Do not commit generated plugin directories.
