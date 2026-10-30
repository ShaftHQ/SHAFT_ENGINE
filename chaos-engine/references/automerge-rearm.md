# Auto-merge re-arm and scheduled soft-fail

## Rule

1. Dependabot (or equivalent) auto-merge must **re-arm** after merge-queue
   eviction (`checks_timed_out`, `merge_conflict`, and similar). Eviction
   clears auto-merge and does not re-fire the original PR event.
2. Scheduled re-arm jobs **soft-fail per item**: log a warning for a PR that
   cannot be armed (draft, conflicting, permissions) and continue; exit 0 so
   one stuck PR cannot spam red scheduled runs.

## Why

Merge-queue timeouts drop seats silently. A hard-failing schedule turns one
unarmable PR into dozens of red workflow runs per day without merging anything.

## Check

Schedule/dispatch path: skip drafts and conflicting PRs; on arm failure emit
`::warning` and proceed; assert workflow conclusion success when at least the
loop completed. Manual: dequeue via timeout → next schedule restores
`autoMergeRequest`.

## Follow-up

[work GitHub playbook](work-github-playbook.md), [ci status economy](ci-status-economy.md),
repo `dependabot-auto-merge` workflow.
