# CI Failure and Allure Artifact Investigation Runbook

Use this runbook when an agent is assigned a ticket for failing GitHub Actions E2E, local, cloud, or scheduled workflows.

## 1. Start from authenticated GitHub metadata

Normalize GitHub CLI credentials first so private logs and artifacts are available without exposing tokens:

```bash
source scripts/ci/github-auth-env.sh
GH_TOKEN="${GITHUB_TOKEN:-${GH_TOKEN:-}}" gh auth status -h github.com
```

Collect run, job, failed-step, and artifact metadata before changing code:

```bash
RUN_ID=<run-id>
GH_TOKEN="${GITHUB_TOKEN:-${GH_TOKEN:-}}" gh run view "$RUN_ID" \
  --repo ShaftHQ/SHAFT_ENGINE \
  --json name,workflowName,conclusion,status,createdAt,updatedAt,url,jobs \
  | jq '{name,workflowName,conclusion,status,createdAt,updatedAt,url,jobs:[.jobs[]|{name,conclusion,status,databaseId,failedSteps:[.steps[]|select(.conclusion=="failure")|{name,number,conclusion}]}]}'

GH_TOKEN="${GITHUB_TOKEN:-${GH_TOKEN:-}}" gh api \
  "repos/ShaftHQ/SHAFT_ENGINE/actions/runs/$RUN_ID/artifacts" \
  --jq '.artifacts[] | [.name,.size_in_bytes,.expired,.archive_download_url] | @tsv'
```

Download job logs for each failed job and search for post-test summaries, setup failures, and root-cause stack traces:

```bash
JOB_ID=<job-id>
mkdir -p /tmp/shaft-ci/logs
GH_TOKEN="${GITHUB_TOKEN:-${GH_TOKEN:-}}" gh api \
  "repos/ShaftHQ/SHAFT_ENGINE/actions/jobs/$JOB_ID/logs" \
  > "/tmp/shaft-ci/logs/$JOB_ID.txt"
rg -n "(Allure Report Summary|::error::|FAILED|BROKEN|AssertionError|Exception|SessionNotCreated|401|TimeoutException)" \
  "/tmp/shaft-ci/logs/$JOB_ID.txt"
```

## 2. Traverse SHAFT single-file Allure artifacts quickly

Many SHAFT workflow artifacts named `*_Allure.html` are already self-contained HTML reports, not zip archives. `gh run download` may fail with `zip: not a valid zip file`; in that case, save the artifact API response directly as an HTML file.

```bash
RUN_ID=<run-id>
mkdir -p /tmp/shaft-ci/allure-html
GH_TOKEN="${GITHUB_TOKEN:-${GH_TOKEN:-}}" gh api \
  "repos/ShaftHQ/SHAFT_ENGINE/actions/runs/$RUN_ID/artifacts" \
  --jq '.artifacts[] | select(.name | endswith("_Allure.html")) | [.name,.archive_download_url] | @tsv' \
  | while IFS=$'\t' read -r name url; do
      GH_TOKEN="${GITHUB_TOKEN:-${GH_TOKEN:-}}" gh api "$url" > "/tmp/shaft-ci/allure-html/$name"
    done
```

The self-contained Allure HTML embeds report files as JavaScript calls like `d("data/test-results/...json","<base64>")`. Parse those embedded JSON payloads instead of opening a browser when you need the failure list fast:

```bash
python3 - <<'PY'
import base64
import json
import re
from pathlib import Path

for report in sorted(Path('/tmp/shaft-ci/allure-html').glob('*_Allure.html')):
    failures = []
    for match in re.finditer(r'd\("([^"]+)","([A-Za-z0-9+/=]+)"\)', report.read_text(errors='ignore')):
        path, payload = match.groups()
        if path.startswith('data/test-results/') and path.endswith('.json'):
            data = json.loads(base64.b64decode(payload))
            if data.get('status') in {'failed', 'broken'}:
                error = data.get('error') or {}
                failures.append((data.get('status'), data.get('fullName') or data.get('name'), error.get('message', '')))
    print(f'\n==== {report.name}: {len(failures)} failed/broken ====')
    for status, name, message in failures:
        print(f'- {status}: {name}')
        print('  ' + message.replace('\r', '').replace('\n', ' | ')[:1000])
PY
```

Always count populated result JSON files before trusting any Allure status analysis:

```bash
python3 - <<'PY'
import base64
import json
import re
from pathlib import Path

for report in sorted(Path('/tmp/shaft-ci/allure-html').glob('*_Allure.html')):
    total = 0
    statuses = {}
    for match in re.finditer(r'd\("([^"]+)","([A-Za-z0-9+/=]+)"\)', report.read_text(errors='ignore')):
        path, payload = match.groups()
        if path.startswith('data/test-results/') and path.endswith('.json'):
            total += 1
            status = json.loads(base64.b64decode(payload)).get('status', 'unknown')
            statuses[status] = statuses.get(status, 0) + 1
    print(report.name, total, statuses)
PY
```

If the count is zero or unexpectedly low, treat the report as empty/invalid and investigate report generation before diagnosing tests.

When a run passes locally or after retries but the logs show transient failures, parse all result JSON files, not only the final summary. Retried attempts can appear as `skipped` in Allure with useful failure messages. In parallel TestNG grid jobs, messages such as `FileNotFoundException`, `Can't create an ImageInputStream`, or missing files under `target/temp/...` often point to test setup/cleanup state shared through instance fields rather than a browser-specific defect.

## 3. Separate test defects from environment/provider defects

Document each failed/broken test with:

- workflow/run ID, job ID, and artifact name;
- Allure `fullName`, status, and concise failure signature;
- whether the failure is deterministic in local targeted tests;
- whether the signature points to code, test isolation, CI host behavior, credentials, or an external provider outage.
- whether nearby log lines show concurrent methods from the same TestNG class sharing or deleting a resource. Under `setParallel=METHODS`, shared instance fields can race even when every method has `@BeforeMethod` and `@AfterMethod` hooks.

Do not hide provider or credential failures by changing assertions. For example, a cloud-provider `401` during setup should be reported as credential/provider evidence; only fix framework/test teardown if it adds a secondary `NullPointerException` or masks the real setup failure.

## 4. PR authorship and title hygiene for agent-created work

When an AI agent creates a branch or PR, make that explicit even if the GitHub token belongs to a human maintainer:

- Branch names should be descriptive and preferably start with `codex/` for Codex-authored work.
- PR titles created by Codex should start with `codex:` unless the user explicitly requests a different prefix.
- PR bodies must state that Codex/AI authored the branch and PR, for example: `This PR was created by Codex, not manually by the maintainer whose token opened it.`
- Avoid placeholder titles or bodies. Include summary, validation, limitations, and links to the issue/run evidence.
