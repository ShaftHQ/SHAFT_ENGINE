# CI Failure Investigator

Use this playbook only for CI failure triage.

## Workflow

1. Identify the exact run, failed jobs, changed commit, and failed steps. Use
   local logs supplied by the user before making GitHub calls.
2. If remote data is required, normalize authentication with
   `scripts/ci/github-auth-env.sh`, then fetch failed-job logs and relevant
   artifact metadata only. Do not download successful-job logs by default.
   A job-log download is not a MemPalace or Graphify retrieve. `gh api`
   `.../actions/jobs/<id>/logs`, `gh run view --log-failed`, and curl of that
   logs URL may be saved under a scratch directory and filtered with
   `sed`/`tail` without a store citation.
3. Search logs for the first actionable exception and its surrounding setup or
   teardown lines. Separate primary failure from reporting and cleanup noise.
4. For SHAFT artifacts, follow
   [the CI investigation runbook](https://shafthq.github.io/docs/maintainers/ci-failure-investigation).
   Parse self-contained Allure HTML or result JSON instead of opening large
   reports manually.
5. Count result files before trusting statuses. Inspect failed, broken, and
   retried/skipped attempts when the final summary hides an intermittent
   failure.
6. Classify the cause as code, test isolation, configuration, dependency,
   runner/infrastructure, credentials, or external provider. Do not weaken
   assertions to hide provider or credential failures.
7. Inspect only the source and recent changes connected to the failure
   signature. Propose or implement the smallest root-cause fix.
8. Validate with the narrowest local reproduction. Request a remote rerun only
   when local evidence cannot prove the environment-specific behavior.

## Known SHAFT fingerprints

- `setup-lock-pin-drift`: a bundled setup `package-lock.json` changed but the
  planner SHA-256 pin did not, so `Unit Tests (shaft-infrastructure)` fails
  with "lock does not match the approved plan". Keep the pin. Same-repo
  Dependabot pull requests self-heal through `setup-lock-pins.yml`; otherwise
  run `python3 scripts/ci/refresh_setup_lock_pins.py --write` in the same tip.

## Output

Report the run/job, concise signature, root cause and confidence, affected
files, validation evidence, and remaining environment risk. Avoid templated
tables when a short finding is clearer.
