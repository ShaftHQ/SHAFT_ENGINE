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

## Targeted dispatch — never run the full E2E DAG to verify one test (#4115)
Verifying a fix means running the failing test(s) plus a few plausibly
impacted neighbours — never a full `E2E Tests` / `Local E2E Tests` dispatch.
Both `.github/workflows/e2eTests.yml` and `e2eLocalTests.yml` take
`workflow_dispatch` inputs for exactly this:
- `jobs` — comma-separated job names, **no spaces** (e.g. `Ubuntu_APIs` or
  `Ubuntu_APIs,Ubuntu_Database`). Defaults to `all` (every job — today's
  behaviour, unchanged). Enforced per-job via a job-level `if:` guard keyed
  off `github.event.inputs.jobs`, not a matrix — so it composes with each
  job's pre-existing OS/browser-specific steps unchanged.
- `tests` — a Maven `-Dtest=` selector, threaded into *every selected job's*
  Run-tests step. Empty (the default) leaves each job's own hard-coded
  selector untouched; a non-empty value overrides it identically across
  whichever job(s) you selected. There's only one `tests` field for the
  whole workflow (not per-job), so pick one job when you also pass `tests`
  unless you deliberately want the same selector tried against several jobs.

Dispatch exactly one job with one test, no full-suite run:
```
gh workflow run e2eTests.yml --ref <branch-or-main> \
  -f jobs=Ubuntu_APIs \
  -f tests=SomeApiTest#someMethod
```
Pick a fast job for any exploratory dispatch — `Ubuntu_APIs` and
`Ubuntu_Database` finish in a couple of minutes. Never pick a Grid or
BrowserStack job just to check one test ran.

**Why the scheduled nightly is unaffected:** on a `schedule` trigger,
`github.event_name != 'workflow_dispatch'` is unconditionally true (a
scheduled run's event name is always exactly `'schedule'`), which is the
first clause of every job's `if:` guard — so the guard short-circuits to
`true` and every job runs, with zero dependence on how `github.event.inputs.*`
happens to evaluate on a non-dispatch trigger. That said,
`github.event.inputs.<name> == ''` is also true on non-dispatch events (the
property is present-but-empty, not unset, per GitHub's loose-equality
coercion of the missing/null value to `''`) — a second, independent reason
the same guard degrades safely even without the `event_name` clause.
A partial (non-`all`) `workflow_dispatch` also flips the failing-nightly
tracking-issue `outcome` input to `'skip'` in both workflows' notify job —
otherwise a single targeted job passing would read as the whole suite
having recovered.
