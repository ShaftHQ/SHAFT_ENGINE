# GitHub delivery

Load this reference only when GitHub is part of the requested endpoint. The canonical skill controls
workflow; this file supplies GitHub mechanics.

## Establish live state

- Resolve repository, default branch, current head, issue, PR, and granted authority.
- Read active feedback and required checks when they affect delivery.
- Preserve unrelated work and never infer authority from a branch name or old receipt.

## Deliver

1. Keep a short plan on the existing issue or PR when it helps another owner resume.
2. Commit one coherent change and push the exact head to the requested target.
3. Create or update a PR only when the delivery shape uses one; always name the base explicitly.
4. For CI failure, inspect the exact job, fix its demonstrated cause, and avoid unrelated suite runs.
5. Review is optional unless the user or server-side policy requires it.
6. Merge only with granted authority. Confirm `mergedAt` when merge is the requested endpoint.
7. Update and close issues whose scope is actually delivered.

Explicit owner authority may select direct default-branch work, force-with-lease, server-side bypass,
skipped local checks, or skipped review. GitHub unavailability blocks only an operation that truly
requires GitHub; it never blocks unrelated local work or completion reporting.

Hooks, planning commits, zero-file drafts, issue matrices, CI, review, receipts, arming, watching,
and cleanup are never local mutation or Stop gates. Do not implement delivery policy with command
parsing or expanding allowlists/denylists.

## Report

State the exact commit, remote/PR/merge state, checks actually observed, issue state, and material
residue. Do not present a push, auto-merge request, green check, or comment as proof of merge.
