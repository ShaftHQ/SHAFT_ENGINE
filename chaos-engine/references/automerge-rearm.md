# Dependabot queue diagnostic (no re-arm)

## Rule

1. Do not re-arm Dependabot after a merge-queue eviction (`checks_timed_out`,
   `merge_conflict`, and the same family). Eviction clears auto-merge and is
   not a signal to enqueue again.
2. `schedule` and `workflow_dispatch` are read-only. They inventory real queue
   membership, the synthetic merge-group SHA (`headCommit.oid`, not the pull
   request head SHA), and both required checks (`PR Gate Summary` and
   `Release-note governance`). They do not dequeue and they do not enable
   auto-merge. A diagnostic failure exits non-zero. The old per-item soft-fail
   (warning, then exit 0) hid a green "all armed" result.
3. Opened, reopened, and synchronize events do not enqueue. A future bot merge
   needs a maintainer-explicit opt-in bound to that head SHA, and the opt-in
   has to be verified. A manual hold or removal is authoritative for that head.
   Synchronize does not clear the hold. Until that opt-in exists, a maintainer
   enqueues by hand.
4. Null `autoMergeRequest` does not mean the pull request is outside the queue.
   Read `mergeQueueEntry`. An API error is not an empty run list. One missing
   required check is not healthy. A young entry inside the age grace is not a
   stall. Repeated timeouts stop; they do not start another loop. Abandoned or
   cancelled installer results are not success.
5. Do not replace `GITHUB_TOKEN` with a stronger token.
6. Every `gh pr` call passes `-R "$GITHUB_REPOSITORY"`, and every `gh api`
   call sets `GH_REPO="$GITHUB_REPOSITORY"`. Without that, a schedule runner
   can print an empty list and look green while it saw nothing. That empty
   list is a failure.

## Why

The 2026-11-06 schedule treated seven open Dependabot pull requests as unarmed
because `autoMergeRequest` was null, then tried to put them back. The heal
path looked up workflow runs with the pull request head SHA, so a live queue
entry could look like it had no merge-group runs. Soft-fail turned that into
a green job. Merge-queue eviction was the trigger the old re-arm rule was
written for, and it is the behavior this rule retires.

## Check

Workflow file: no auto-merge command, no dequeue mutation, no
`pull_request_target` enqueue, `contents: read`, and a non-zero exit when the
inventory fails. Predicate tests cover an explicit hold, a queued pull request
with null auto-merge, synchronize after a hold, repeated timeout, the wrong
SHA, either required check missing, a young or healthy entry, an API error,
retry exhaustion, and an abandoned or cancelled installer. Contract:
`tests/scripts/test_dependabot_auto_merge_workflow.py` and
`tests/scripts/test_merge_queue_awaiting_checks_heal.py`.

## Follow-up

[work GitHub playbook](work-github-playbook.md), [ci status economy](ci-status-economy.md),
repo `dependabot-auto-merge` workflow. Enabling the workflow on GitHub is a
separate maintainer action after this file is on the default branch. This
page does not claim the live schedule is already on the new policy.
