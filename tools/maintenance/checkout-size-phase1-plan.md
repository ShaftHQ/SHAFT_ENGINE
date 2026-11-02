# #7547 Phase 1 plan — history rewrite (owner-gated)

Status: **DRAFT FOR DUAL ADVERSARIAL REVIEW**. Do not force-push `main` until
both independent reviewers approve this plan (Mohab t612u, 2026-11-02).

Issue: [#7547](https://github.com/ShaftHQ/SHAFT_ENGINE/issues/7547).
Phase 0 (partial clones, worktree sharing, apk/ipa externalize + fail-closed
gate) is already on `main`. This plan is the destructive follow-up.

## Goal

Replace the heavy object history with a slim root that keeps today's tree
(minus already-externalized binaries), preserves contributor credit, and
archives the old history read-only.

## Chosen option

**Option L** from the Oct 29 measurement report (preferred over filter-repo J):

1. Push a read-only archive of today's full history **before** any rewrite
   (`ShaftHQ/SHAFT_ENGINE-history` mirror, or a `history-archive/*` tag bundle
   on a separate remote). Verify the archive with `git fsck` and a fresh clone.
2. Build a new root commit of `main`'s tree at the cut SHA:
   - Keep all tracked source; apk/ipa stay external (Phase 0).
   - Generate `CONTRIBUTORS.md` from `git shortlog -sne --all` (humans only;
     bots listed separately). Put `Co-authored-by` trailers for human
     contributors on the root commit. **Do not print emails in the PR body.**
3. Force-push the new root to `main` **only after** both reviewers approve and
   Mohab's standing force-push rules are satisfied (this plan is that sign-off
   surface under t612u).
4. Recreate any still-open PRs onto the new root (at cut time: coverage #7679
   if unmerged; others as listed by `gh pr list`).
5. Announce that forks must re-clone; retarget CI caches (expect one cold cycle).
6. Keep ignore + CI guards so `graphify-out/`, Javadoc site output, and
   apk/ipa cannot re-enter history (already partially present from Phase 0).

## Why L over J

| | J filter-repo keep graph | L squash+slim |
| --- | --- | --- |
| Pack after | ~248 MiB history few agents need | ~20–30 MiB pack + ~75 MiB tree (~98M total measured) |
| SHAs | all change | all change |
| Blame across old SHAs | possible in same remote | only in archive remote |
| Matches owner "start fresh" preference | no | yes |

## Safety checklist (must all pass before force-push)

- [ ] Archive remote/tag verified (`git fsck`, clone smoke).
- [ ] `CONTRIBUTORS.md` + Co-authored-by trailers reviewed (names only in PR).
- [ ] Open PR list snapshotted; recreation plan named per PR.
- [ ] Both adversarial reviewers **Approve** this plan in PR review comments.
- [ ] No SHAFT release cut from the rewrite commit.
- [ ] Post-push: CI green on the new root; Phase 0 guards still red on apk/ipa.

## Out of scope

- Rewriting forks in place.
- Changing Maven Central published artifacts.
- Force-push without both reviewer Approvals on this document.

## Rollback

The archive remote is the rollback. If `main` rewrite misbehaves, restore
`main` from the archive tip and reopen a fix PR (no second rewrite without a
new dual review).
