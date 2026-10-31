# L78 REVIEW-SCRATCH-CLEANUP

Reviewer copies must be removable without an owner delete code.

1. Prefer `git archive <pushed-sha> | tar -x -C temp/ce-review-<pr>-<sha12>`
   (no linked worktree; outside git → scratch allow-list under `temp`).
2. Or `git worktree add --detach <path> <pushed-sha>` where sha is on
   `origin` (HEAD not ahead; clean tree — no install dirt / `__pycache__`).
3. Gate: `scripts/ci/review_scratch.py` (repo CI helper; not in portable overlay)
   `evaluate()` FAILs receipts that used an unpushed tip or left porcelain dirt.
