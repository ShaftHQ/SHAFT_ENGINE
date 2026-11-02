# #7547 Phase 1 plan — history rewrite (owner-gated)

Status: **DRAFT FOR DUAL ADVERSARIAL REVIEW**. Do not force-push `main` until
both independent reviewers approve this plan (Mohab t612u, 2026-11-02).

Issue: [#7547](https://github.com/ShaftHQ/SHAFT_ENGINE/issues/7547).
Phase 0 (partial clones, worktree sharing, apk/ipa externalize + fail-closed
gate) is already on `main`. This plan is the destructive follow-up.

**Closing honesty:** a PR that only lands this document must **not** use
`Closes #7547`. Use `Addresses #7547` until the rewrite below has executed and
post-push CI is green. Auto-closing #7547 without the rewrite is dishonest.

## Goal

Replace the heavy object history with a slim root that keeps today's tree
(minus already-externalized binaries), preserves contributor credit, and
archives the old history read-only.

## Chosen option

**Option L** from the Oct 29 measurement report (preferred over filter-repo J):

1. Push a read-only archive of today's full history **before** any rewrite.
2. Build a new root commit of `main`'s tree at the cut SHA.
3. Force-push the new root to `main` **only after** both reviewers Approve and
   Mohab's standing force-push rules are satisfied.
4. Recreate still-open PRs onto the new root.
5. Announce forks must re-clone; retarget CI caches (one cold cycle).
6. Keep ignore + CI guards so `graphify-out/`, Javadoc site output, and
   apk/ipa cannot re-enter history (Phase 0).

## Why L over J

| | J filter-repo keep graph | L squash+slim |
| --- | --- | --- |
| Pack after | ~248 MiB history few agents need | ~20–30 MiB pack + ~75 MiB tree (~98M total measured 2026-10-29 on box) |
| SHAs | all change | all change |
| Blame across old SHAs | possible in same remote | only in archive remote |
| Matches owner "start fresh" preference | no | yes |
| Risk | filter-repo bugs leave orphans | single cut; archive is sole rollback |

Reproduce sizes before cut:

```bash
git count-objects -vH
git rev-list --objects --all | git cat-file --batch-check='%(objecttype) %(objectname) %(objectsize) %(rest)' \
  | awk '/^blob/ {print $3, $4}' | sort -n | tail -20
```

## Concrete archive procedure (must complete before force-push)

```bash
CUT_SHA=$(git rev-parse origin/main)
ARCHIVE_REMOTE=git@github.com:ShaftHQ/SHAFT_ENGINE-history.git   # or agreed mirror

# Create empty private mirror repo in org (gh), then:
git clone --mirror git@github.com:ShaftHQ/SHAFT_ENGINE.git /tmp/shaft-history-mirror
cd /tmp/shaft-history-mirror
git remote set-url --push origin "$ARCHIVE_REMOTE"
git push --mirror
git fsck --full
# Smoke: fresh clone from archive must contain CUT_SHA
git clone "$ARCHIVE_REMOTE" /tmp/shaft-history-smoke
git -C /tmp/shaft-history-smoke cat-file -t "$CUT_SHA"
# Tag the cut for humans:
git -C /tmp/shaft-history-smoke tag -a "pre-rewrite-$CUT_SHA" "$CUT_SHA" -m "Archive tip before Option L"
git -C /tmp/shaft-history-smoke push origin "pre-rewrite-$CUT_SHA"
```

## Concrete rewrite procedure (after dual Approve)

```bash
cd /path/to/SHAFT_ENGINE
git fetch origin
git checkout -B rewrite-root "$CUT_SHA"
# CONTRIBUTORS (names only in PR; emails only in trailers locally):
git shortlog -sne --all | grep -vE '\[bot\]|dependabot|github-actions|web-flow' \
  > /tmp/contributors-humans.txt
# Review /tmp/contributors-humans.txt; write CONTRIBUTORS.md (names + counts, no emails).
# Build orphan root from current tree:
git checkout --orphan slim-main
git add -A
# Commit with Co-authored-by trailers from the humans file (emails from shortlog).
git commit -F /tmp/slim-root-message.txt
NEW_ROOT=$(git rev-parse HEAD)
# Force-push ONLY with overlay_pre_push + owner rules satisfied:
git push --force-with-lease origin slim-main:main
```

## Open-PR recreation

At cut time snapshot:

```bash
gh pr list --repo ShaftHQ/SHAFT_ENGINE --state open --json number,title,headRefName,baseRefName > /tmp/open-prs.json
```

For each open PR whose base is `main`:

1. Note head SHA and branch.
2. After rewrite, `git fetch origin` and recreate: cherry-pick or rebase the
   unique commits onto `NEW_ROOT`, push a new branch, open a replacement PR,
   close the old one as superseded with a link.
3. Coverage #7679: if still open at cut, recreate first (highest traffic).

## Safety checklist (must all pass before force-push)

- [ ] Archive remote/tag verified (`git fsck`, clone smoke, `cat-file -t $CUT_SHA`).
- [ ] `CONTRIBUTORS.md` + Co-authored-by trailers reviewed (names only in PR body).
- [ ] Open PR list snapshotted (`/tmp/open-prs.json`); recreation owner named.
- [ ] Both adversarial reviewers **Approve** this plan in PR review comments.
- [ ] No SHAFT release cut from the rewrite commit.
- [ ] Post-push: CI green on the new root; Phase 0 guards still red on apk/ipa.

## Out of scope

- Rewriting forks in place.
- Changing Maven Central published artifacts.
- Force-push without both reviewer Approvals on this document.

## Rollback

```bash
# If main misbehaves after rewrite:
git fetch "$ARCHIVE_REMOTE"
git push --force-with-lease origin "pre-rewrite-$CUT_SHA:main"
# Then open a fix PR from a normal branch. No second rewrite without a new dual review.
```

The archive remote is the only rollback. Do not delete the archive for ≥90 days.
