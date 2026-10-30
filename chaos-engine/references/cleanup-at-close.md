# Cleanup at item close

## Rule

1. **Clean at item close** only after the stream's delivery is pushed and
   verified (merged / published receipt). Never delete the only copy of
   unpushed work.
2. **Disk budget:** run a free-space check before heavy work. At ≥80% used
   (or free under the harness warn tier), set **BLOCKED-INFRA** and stop
   starting new heavy workers until space recovers.
3. **Scoped worker dirs:** each worker owns one worktree / scratch dir; the
   lead owns cleanup of that stream after DoD.
4. **Large media** goes to a release artifact or Git LFS — not unbounded box
   scratch.

## Why

Shared boxes fill from orphaned worktrees and media. Cleanup before push
loses unique commits; cleanup after verify keeps the box reusable.

## Check

Post-merge: worktree hygiene proofs (own, merged, clean). Pre-heavy:
`box_disk_guard` warn/block tiers. BLOCKED-INFRA row when over budget.

## Follow-up

[cleanup scopes](cleanup-scopes.md), [delete-check discipline](delete-check-discipline.md),
`scripts/ci/box_disk_guard.py`, `scripts/ci/worktree_hygiene.py`.
