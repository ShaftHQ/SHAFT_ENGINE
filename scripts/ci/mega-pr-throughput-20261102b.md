---
name: mega-pr-throughput-20261102b
description: Use when a mega PR is green but merge-queue enqueue fails, when linked-worktree cap blocks a new worker, or when a session overlay doctor disagrees with the primary install.
---

# Mega-PR throughput follow-ups (2026-11-02b)

Companion to [mega-pr-throughput-20261102.md](mega-pr-throughput-20261102.md).
Lessons from finishing PR #7731 and scoping #7731 follow-ups.

## Unresolved code-quality review threads

Even when every check is green, `enqueuePullRequest` / merge-queue can refuse
the PR while Codacy (or similar) review threads stay **unresolved**.

Rules:

1. Before `gh pr merge --auto`, list unresolved review threads
   (`gh api graphql` / `gh pr view --json reviewThreads` when available).
2. Fix real findings; reply briefly on false positives; **Resolve** every thread.
3. Fingerprint: `unresolved-review-threads` in
   [ci-status-economy.md](../../chaos-engine/references/ci-status-economy.md).
4. Do not raise the merge timeout or weaken the quality bot.

## Linked-worktree cap

`box_disk_guard.refuse_new_worker` blocks a new worktree at the linked-worktree
cap (24) or when free disk is under 10 GiB.

Rules:

1. Before `git worktree add`, call `refuse_new_worker` / `new_worker_blockers`.
2. Prune **own**, **merged**, **clean** trees via
   `scripts/ci/worktree_hygiene.py` proofs only (never dirty/unpushed/foreign).
3. `__pycache__` under a doomed tree may be removed after `tool.py delete-check`
   allow; never route around a delete refusal.
4. Prefer one blobless primary + linked worktrees over N full clones.

## Heal route: distribution id mismatch and session overlays

1. Consumer reinstall blocked when the distribution id differs: **uninstall,
   then install** from the matching source — never re-run the bootstrap
   one-liner over a broken install. See
   [heal-route.md](../../chaos-engine/references/heal-route.md) and INSTALL.md.
2. `worktree_overlay.py upgrade-session` can leave session mempalace/tools
   `recovery-required` while the **primary** doctor still passes. Treat the
   primary install as source of truth; repair session components in place;
   do not treat overlay doctor-failed as a primary install failure.

## Verify

- [ ] Green CI + zero unresolved review threads before merge-queue enqueue.
- [ ] New worker refused at cap; pruning own merged clean trees unblocks add.
- [ ] Heal-route documents uninstall-then-install and session-vs-primary doctor.
