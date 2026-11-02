# Auto-merge re-arm and scheduled soft-fail

## Rule

1. Dependabot (or equivalent) auto-merge must **re-arm** after merge-queue
   eviction (`checks_timed_out`, `merge_conflict`, and similar). Eviction
   clears auto-merge and does not re-fire the original PR event.
2. Scheduled re-arm jobs **soft-fail per item**: log a warning for a PR that
   cannot be armed (draft, conflicting, permissions) and continue; exit 0 so
   one stuck PR cannot spam red scheduled runs.
3. Schedule/dispatch runners do **not** check out the repo. Every `gh`
   invocation must pass `-R "$GITHUB_REPOSITORY"` (or an absolute PR URL).
   Without `-R`, `gh pr list` prints a git error and returns an empty list,
   so the job reports success while arming nothing.

## Why

Merge-queue timeouts drop seats silently. A hard-failing schedule turns one
unarmable PR into dozens of red workflow runs per day without merging anything.
A schedule job with no checkout and no `-R` looks green forever while
Dependabot PRs sit `MERGEABLE`/`CLEAN` with `autoMergeRequest: null`.

## Check

Schedule/dispatch path: skip drafts and conflicting PRs; on arm failure emit
`::warning` and proceed; assert workflow conclusion success when at least the
loop completed. Manual: dequeue via timeout → next schedule restores
`autoMergeRequest`. Contract: workflow YAML contains `gh pr list -R` (see
`tests/scripts/test_dependabot_auto_merge_workflow.py`).

## Follow-up

[work GitHub playbook](work-github-playbook.md), [ci status economy](ci-status-economy.md),
repo `dependabot-auto-merge` workflow.
