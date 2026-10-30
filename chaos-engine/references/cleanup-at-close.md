# Cleanup at item close

## Rule

1. **Clean at item close** only after the stream's delivery is pushed and
   verified (merged / published receipt). Never delete the only copy of
   unpushed work.
2. **Disk budget (free GiB):** use `box_disk_guard` tiers — **warn** under
   **15 GiB** free; **block** new workers under **10 GiB** free. On warn or
   block, set **BLOCKED-INFRA** and stop starting new heavy workers until
   space recovers. Do not use percent-used as the gate.
3. **Scoped worker dirs:** each worker owns one worktree / scratch dir; the
   lead owns cleanup of that stream after DoD.
4. **Large media** goes to a release artifact or Git LFS — not unbounded box
   scratch.

## Why

Shared boxes fill from orphaned worktrees and media. Cleanup before push
loses unique commits; cleanup after verify keeps the box reusable. Free-GiB
tiers match the executable guard so agents do not disagree on when to stop.

## Check

Post-merge: worktree hygiene proofs (own, merged, clean). Pre-heavy:
`scripts/ci/box_disk_guard.py` (`disk_level` → ok / warn / block).
BLOCKED-INFRA row when warn or block.

## Follow-up

[cleanup scopes](cleanup-scopes.md), [delete-check discipline](delete-check-discipline.md),
`scripts/ci/box_disk_guard.py`, `scripts/ci/worktree_hygiene.py`.
