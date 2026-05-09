# CI Coverage Readiness Workflow

`Coverage Readiness` is a lightweight, dispatchable GitHub Actions workflow for coverage and E2E-selector triage before running the full cloud/browser matrix.

## What it runs

The workflow performs the following checks on Ubuntu:

1. Python unit tests for CI helper scripts.
2. `scripts/ci/e2e_test_inventory.py --format markdown --fail-on-unselected` to ensure every concrete Java test class is selected by at least one E2E workflow selector.
3. A caller-provided Maven Surefire selector, defaulting to `PathParamTest,SwaggerContractTest`.
4. Allure failure extraction from `allure-results` when those results are generated.
5. `mvn jacoco:report -Dgpg.skip` plus `scripts/ci/jacoco_coverage_gate.py` to summarize JaCoCo metrics and optionally enforce a threshold.
6. Artifact upload for the inventory summary, Allure failure table, JaCoCo summary/report, Surefire reports, and Allure results.

## Manual dispatch inputs

| Input | Default | Purpose |
| --- | --- | --- |
| `test-selector` | `PathParamTest,SwaggerContractTest` | Maven Surefire `-Dtest` selector to execute before generating the JaCoCo report. Use a broader selector when validating larger coverage changes. |
| `coverage-metric` | `line` | JaCoCo metric to gate when enforcement is enabled. Supported values: `line`, `branch`, `instruction`, `method`. |
| `coverage-minimum` | `80` | Minimum percentage for the selected metric. |
| `enforce-coverage` | `false` | When `true`, the workflow fails if the selected metric is below the configured minimum. |

## Credentials and permissions needed to trigger and monitor workflows

A maintainer or automation identity needs GitHub permissions that can dispatch workflows and read run results:

- Repository `Actions: write` permission, or a fine-grained GitHub token with `actions:write`, to trigger `workflow_dispatch` runs.
- Repository `Actions: read`, `Checks: read`, and `Contents: read` permissions to monitor workflow status, jobs, logs, and artifacts.
- `Pull requests: read` is useful to map a PR branch/SHA to workflow runs.
- `Pull requests: write` or `Issues: write` is only needed if the automation should post progress comments back to PRs/issues.

The full E2E matrix also needs repository/environment secrets for third-party integrations and reporting:

- `CODECOV_TOKEN` for coverage upload/reporting in the existing post-test-report action.
- BrowserStack credentials corresponding to SHAFT properties `browserStack.userName` and `browserStack.accessKey` for jobs that run with `executionAddress=browserstack`.
- LambdaTest credentials corresponding to SHAFT properties `LambdaTest.username` and `LambdaTest.accessKey` for `e2eLambdaTestTests.yml` jobs.

Without those cloud credentials, local and lightweight CI checks can still compile, run deterministic tests, generate JaCoCo reports, and verify workflow selector coverage, but BrowserStack/LambdaTest jobs cannot be proven green.
