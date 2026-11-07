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
3. Opened, reopened, and synchronize events for Dependabot enqueue once,
   with `BOT_TOKEN`, and only when `dependabot_enqueue_decision` returns
   `enqueue`. A `dependabot-hold` label is a manual hold for that pull
   request. Synchronize does not clear the hold. The schedule does not
   enqueue, so an eviction is not a reason to arm auto-merge again.
4. Null `autoMergeRequest` does not mean the pull request is outside the queue.
   Read `mergeQueueEntry` with GraphQL. `gh pr view --json` has no such field,
   and that error is not "not queued". An API error is not an empty run list. One missing
   required check is not healthy. A young entry inside the age grace is not a
   stall. Repeated timeouts stop; they do not start another loop. Abandoned or
   cancelled installer results are not success.
5. The enqueue credential is `BOT_TOKEN`. `GITHUB_TOKEN` events start no workflows, so a merge-queue entry created that way never receives merge_group checks and times out. Do not fall back to `GITHUB_TOKEN`.
   The schedule still uses `GITHUB_TOKEN` for the read-only inventory.
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

Workflow file: the schedule and dispatch jobs do not enable auto-merge and
do not dequeue. The Dependabot `pull_request_target` job checks out the
default branch only, decides with `enqueue-decision`, and enables auto-merge
with `BOT_TOKEN`. `contents: read`. A failed inventory exits non-zero.
Predicate tests cover an explicit hold, a queued pull request with null
auto-merge, synchronize after a hold, repeated timeout, the wrong SHA,
either required check missing, a young or healthy entry, an API error,
retry exhaustion, an abandoned or cancelled installer, and the Dependabot
enqueue decision (hold, already queued, already armed, conflict, draft).
Contract: `tests/scripts/test_dependabot_auto_merge_workflow.py` and
`tests/scripts/test_merge_queue_awaiting_checks_heal.py`.

## Follow-up

[work GitHub playbook](work-github-playbook.md), [ci status economy](ci-status-economy.md),
repo `dependabot-auto-merge` workflow. Enabling the workflow on GitHub is a
separate maintainer action after this file is on the default branch. This
page does not claim the live schedule is already on the new policy.
