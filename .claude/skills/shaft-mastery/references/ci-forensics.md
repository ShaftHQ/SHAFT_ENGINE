# GitHub Actions CI Forensics

## Triage method (proven on multi-job red runs)
1. Pull logs with `gh run view <id> --log` / `--log-failed`; never diagnose
   from the UI summary alone.
2. Group failures by EXACT exception signature first — a 10-job red run is
   usually 2-3 signatures plus noise (PR #3408: 4 jobs one signature, 2
   another, 3 unique).
3. Classify each group: code defect / stale-already-fixed (check merge
   timestamps vs run trigger time) / external provider or target-site
   incident (curl the target live; check the same test's pass history).
4. Only then fix — one root cause per commit, workflow-level fixes get extra
   verification (higher blast radius than app code).

## Signals that lie
- `workflow_run` conclusion `success` ≠ delivery: job-level `if:` skips
  count as success.
- Releases created with the default `GITHUB_TOKEN` emit NO release event
  (anti-recursion) — downstream `on: release` workflows silently never run.
- A job can stay green while a post-test teardown step OOMs/crashes
  (post-test bookkeeping isn't test failure) — read to the END of logs of
  "passing" jobs when hunting incidental defects.
- Fail-fast pipelines mask later-stage bugs: after fixing failure N, expect
  a NEW failure at stage N+1 that no run has ever reached before.

## SHAFT-specific gates
PR-blocking: CodeQL + dependency-review, path-filtered intellij-plugin,
shaft-pilot-release. E2E suites are schedule/dispatch-only; the guidance
validator is manual. So "CI is green" on a PR proves much less than it
suggests — run `scripts/ci/local_gate.py` (changed-module verify) and the
relevant scoped tests yourself.

## Workflow YAML edits
- Parse with PyYAML locally before pushing (syntax + step order).
- Unawaited elevated `Start-Process` in PowerShell steps returns instantly —
  poll for the real readiness condition, never trust step exit.
- Keep diagnostic artifacts alive on failure: `Receive-Job` before
  `Stop-Job`, upload logs in `if: always()` steps.
- Sharded runs: merge via explicit post-step (SHAFT: `report_merge_shards`,
  `assemble_shard_blob.py`) — shard-local reports alone hide cross-shard
  patterns.
