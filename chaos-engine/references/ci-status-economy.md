# CI status economy (babysit, digest, one channel)

Learned in coalesce wave: babysit, process-owner, and parent turns burned tokens re-reading unchanged CI state. This policy is portable. Codex, Claude, Grok CLI, Gemini, Copilot, and Grok Bot share it; no host-only memory exception.

## Contents

- [Agent Plugin Live Acceptance dispatch](#agent-plugin-live-acceptance-dispatch)
- [Wait by event wake, never by LLM poll](#wait-by-event-wake-never-by-llm-poll)
- [A green check can hide failing tests](#a-green-check-can-hide-failing-tests)
- [A late label can leave a cancelled required check](#a-late-label-can-leave-a-cancelled-required-check)
- [A registry 404 for a published artifact is infrastructure first](#a-registry-404-for-a-published-artifact-is-infrastructure-first)
- [An upstream "latest" lists a version before its platform asset](#an-upstream-latest-lists-a-version-before-its-platform-asset)
- [Merge queue and same-account pull requests](#merge-queue-and-same-account-pull-requests)
- [A rerun replays the old event](#a-rerun-replays-the-old-event)
- [Release watch](#release-watch)
- [Gate and CI pitfalls](#gate-and-ci-pitfalls)
- [Digest only](#digest-only)
- [One status channel](#one-status-channel)
- [Fingerprint-first failed logs](#fingerprint-first-failed-logs)
- [Known fingerprints](#known-fingerprints)
- [Executor prompt schema](#executor-prompt-schema)
- [Related](#related)

## Agent Plugin Live Acceptance dispatch

- Dispatch only the jobs the gate names. Installer part 1 on windows-2025 and
  macos-15 is `workflow_dispatch` input
  `jobs=installer-part-1-windows-2025,installer-part-1-macos-15` on
  `agent-plugin-acceptance.yml`. That selection skips the other matrix cells
  and the weekly full harness job. An empty `jobs` input still runs the full
  dispatch. Monday's schedule is unchanged.
- Watch those named jobs with one conclusion line each
  (`scripts/ci/acceptance_job_filter.py` `watch_acceptance_jobs`). Do not paste
  intermediate `gh run view` rows into the agent. Hosts share this helper;
  do not keep a private poll loop.

## Wait by event wake, never by LLM poll

This applies to all GitHub work on every host. While checks run, the agent ends its turn. It wakes on an event, not on a timer it pays tokens for.

Use the first option the host supports:

1. **Host event listener (preferred).** Arm a listener scoped to the PR for `ci-passed`, `ci-failed`, `pr-merged`, and `pr-closed`. Examples are a Grok Bot routine with a GitHub trigger, or a Cursor automation. Including `pr-merged` and `pr-closed` makes it remove itself. It costs no tokens while it waits.
2. **Zero-LLM armed watch.** Run `watch_pr_checks.py --until-merged --digest --status-lease` as a background shell job. It is a script, not a model, and it emits only on a state change.
3. **Scheduled resume.** A coarse routine that is silent while the status lease is live and unchanged (see One status channel).

Forbidden:

- An LLM worker, executor, or subagent whose job is to poll CI. That spends tokens on every check.
- Foreground poll loops that keep a turn alive while waiting. Long-lived turns get killed by hosts ([session cap](bot-entry.md#session-cap-grok-bot-default)), and the work dies with them.

A zero-token waiter (option 1 or 2) is the only exception to [single thread](delegation.md). Real work stays on the one thread. When the wake fires, merge on green or fix on red, then continue the queue without asking the owner whether to proceed.

## A green check can hide failing tests

Some jobs run the build with a flag that ignores test failures (for example `testFailureIgnore=true`), so the job concludes success while tests fail. When a delivery adds or changes an acceptance test, read that job's test-runner summary line (for example `Tests run: N, Failures: F`) and the named test's `Finished test method` line before declaring it proven. File any failure outside the delivery's scope; do not rely on the check conclusion alone.

Reading is the manual check. The durable remedy is a step in the job itself, directly after the test step, that parses the test reports and exits non-zero on any failure (use the repo's report-assertion helper under `scripts/ci/`). A delivery that adds a job running named tests under a failure-ignoring build is incomplete without that step.

## A late label can leave a cancelled required check

A classification label added after the PR opened starts a second governance run and cancels the first. If the PR stays `BLOCKED` with every check green, the newest run's `CANCELLED` required check is the cause. Rerun that newest run (`gh run rerun <run-id>`), not the oldest. Prefer the label in the same `gh pr create` step.

## A registry 404 for a published artifact is infrastructure first

When a job fails with "could not find artifact" for a release that should exist:

1. `curl -sI <artifact-url>` and read `last-modified`. An old date with HTTP 200 now is a transient registry fault, not a code or version problem.
2. Reproduce with an empty local cache (`-Dmaven.repo.local`-style override). A pass confirms it.
3. Fix by retrying in a fresh process (the repo's retry helper under `scripts/ci/`), never by editing versions.
4. If `gh run view --log-failed` omits a failed job, read it with `gh api repos/<owner>/<repo>/actions/jobs/<job-id>/logs`.

## An upstream "latest" lists a version before its platform asset

A vendor channel can list a new GA while one OS's download still returns 404
(Temurin: Linux 200, Windows 404). Fix forward in the resolver: on HTTP 404 only,
fall back to the pinned version from the dependency spec, with a test. A frozen
acceptance base that resolves "latest" gets a narrow skip (exact base SHA, exact
command, 404 mark); the candidate install still runs.

## Merge queue and same-account pull requests

- With a merge queue, `gh pr merge --auto` hands the PR to the queue. Then
  `autoMergeRequest` can read null while the PR is still queued. Check
  `mergeQueueEntry` in GraphQL before you re-arm it, and re-check after every push.
- GitHub refuses an approval from the account that authored the PR. When a
  bot pushes as the owner's account, don't retry the approval. Rely on the
  required checks and the merge queue, and say in the report that no
  approval was given.
- If GitHub suspends a bot machine account, keep working: commit locally
  with the bot identity, and the owner's account pushes and opens the PR.
  This is current practice; don't create a replacement bot account to get
  around the suspension.

## A rerun replays the old event

`gh run rerun` replays the original event payload, so it never sees an edited
PR body or title. To re-evaluate a body gate, trigger a new event (push a
commit, or close and reopen the PR).

`gh run rerun` on a run that is still in progress fails with a message that
reads as if the workflow file were broken. The workflow is fine: wait for the
run to finish (or `gh run cancel <run-id>`), then rerun.

## Release watch

- A release-prep PR opened by the workflow gets no PR Gate checks
  (`GITHUB_TOKEN` events start no workflows). If it shows no PR Gate checks,
  close and reopen it as the owner to start them.
- A release-prep PR only bumps versions. Merging it is the publish: the
  delivery workflow releases from the default branch when the version
  changes. Before you merge, check that no release blockers are open.
- Watching a release takes a whole session. Pre-deploy gates and the queue
  take most of it. Watch every downstream job, IDE plugin jobs first.
- A marketplace upload reported as "accepted, pending moderation" is not a
  failure. Never re-run a publish: versions are single-use.
- A scanner vulnerability alert (for example Scorecard) is a blocker only
  after an OSV query over the resolved dependency list confirms it.

## Gate and CI pitfalls

- Before blaming your PR for a red check, check the same check on the default
  branch: a red that is also on main is drift, not your change.
- Wait only on your own queue's PRs; another stream's open PR is not a blocker.
- Every human-format PR needs exactly one release-note classification as a
  label (for example `skip-release-notes`); text in the body is not enough.
  Add it at create time.
- Run `scripts/ci/code_quality_preflight.py` before the first push. Touching a
  function already over the complexity gate fails diff-scoped static analysis;
  wrap it (a new caller or helper) instead of editing it.
- Code-quality bots flag `import subprocess` (B404) without a `nosec` reason,
  bare `assert` (B101), `xml.etree` parsing (use `re` on known text), partial
  executable paths (B607: `shutil.which`), and D213 docstrings, in tests too.
- `python3 -I -m unittest` cannot import the repo's `tests` package (`-I`
  drops the cwd); run the test file by path with `python3 -I`, or set
  `PYTHONPATH=.` where `-I` is not needed.
- Join preflight and push with `&&` under `set -o pipefail`, never `;`: a
  failed preflight must not push.
- Verify a workflow fix with `workflow_dispatch` on the branch before merging;
  a targeted dispatch (named jobs and tests) is minutes, not a nightly.
- Windows `mvn` in pwsh is `mvn.cmd` through `cmd.exe` (8191-character command
  line); long test selections need `shell: bash`.
- A test report's `total` can count ignored methods; use
  passed + failed + skipped to decide whether anything ran.
- A single gate flake that a rerun clears is noise; file an issue if it recurs.
- A dry run that computes "tomorrow" collides with a version already bumped
  today; such gates must accept or move past the head version.

## Digest only

- Agent-facing CI status comes from `python3 scripts/agents/watch_pr_checks.py --pr <n> --until-merged --digest` (one blocking watch). (repo-only)
- Never paste a raw `statusCheckRollup` or a full check-runs list into chat, an executor prompt, or a status report.
- Digest schema:

```json
{
  "sha": "<head>",
  "state": "green|red|pending|merged",
  "failing": [{"name": "...", "url": "..."}],
  "failing_total": 0,
  "pending_count": 0,
  "static_analysis_action_required": [],
  "updated_at": "<UTC>"
}
```

`failing` is capped at 10; the whole digest stays under 4096 bytes (`DIGEST_MAX_BYTES`). RED keeps `failingJobs` in the same JSON object.
- The watch emits only on state change or terminal state; no heartbeat tokens.
- `rollup_is_waste()` in the same script flags a tool result that pastes a rollup above the digest budget.

## One status channel

- Exactly one status channel per in-flight PR: the armed watch. Start it with `--status-lease --digest-out .chaos-engine/runtime/digest-<pr>.json`; it records `.chaos-engine/runtime/status-lease-<pr>.json`.
- Scheduled process-owner status routines run `python3 scripts/agents/status_lease.py routine --pr <n> --last-reported <state>`; it prints nothing while the lease is live and unchanged, and one line on `red` / `merged` or when no live watch exists. (repo-only)
- A scheduled status run reports directly in one capped table of changed rows; no relay turn, no unchanged rows, silence when nothing changed.
- Bots wait with the armed watch only: no ad-hoc wait or poll scripts, and no automatic update-branch (each one restarts every check).
- Parent re-entry does not re-narrate unless the watch returned RED/MERGED or the owner asked (`--owner-asked`).
- Link: [process-owner](process-owner-scrum-master.md), [orchestrator follow-through](orchestrator-follow-through.md).

## Fingerprint-first failed logs

- If the job summary, annotation, or evidence JSON already names a fingerprint, read at most 40 lines around it:

  ```bash
  python3 .chaos-engine/skills/local-agency/scripts/executor_brief.py log-budget <log> --fingerprint <text>
  ```

- Otherwise spill the full log to disk and keep only path + fingerprint + tail in context. `tip_preflight.classify_failure()` maps a summary line to a known fingerprint.

## Known fingerprints

| Fingerprint | Meaning | Babysit move |
|-------------|---------|--------------|
| `graphify-empty-output-after-version` | Windows graphify.exe exited non-zero with empty streams after a healthy `--version`; installer retries once, then absorbs and emits the fingerprint | Known flake: do not open a tip; the installer on `main` absorbs it |
| `inventory-drift` | `source-derived inventory drift: <section>` | Refresh README inventory in the same tip ([tip-churn preflight](tip-churn-preflight.md)) |
| `bandit-b607` | partial executable path | Resolve with `shutil.which`; see [tip-churn preflight](tip-churn-preflight.md) |
| `memory-content-hash` | stale Memory `content_hash` | `tip_preflight.py --rehash` in the same tip |
| `unresolved-review-threads` | all checks green but merge state `BLOCKED` by unresolved review threads; the watch goes RED | Fix or reply, then resolve each thread; see [Static-analysis gate](static-analysis-gate.md) |
| `static-analysis-action-required` | static-analysis ≥medium finding | Blocking; see [Static-analysis ACTION_REQUIRED gate](static-analysis-gate.md) |
| `codecov-patch-partial` | `codecov/patch` FAILURE while another check is still pending | Not RED. The same check often updates after a later shard uploads. A FAILURE after every other check has completed stays RED. |

## Executor prompt schema

Task / executor / `codex exec` prompts carry a pointer plus the delta slice only; lint with `executor_brief.py lint` (cap 4096 inline bytes; no completed todos from another wave).

```text
brief_path: /tmp/<wave>/brief.md
goal: <one sentence>
constraints: <budgets, forbidden paths>
files: <paths this slice may touch>
red: python3 -m unittest <module> -v
```

## Related

- `scripts/agents/watch_pr_checks.py` (repo-only), `scripts/agents/status_lease.py` (repo-only), [`executor_brief.py`](../skills/local-agency/scripts/executor_brief.py)
- [Tip-churn preflight](tip-churn-preflight.md) · [Static-analysis ACTION_REQUIRED gate](static-analysis-gate.md)

## Local gates CI ratchets

- Before pushing new or changed code in a module with an Error Prone profile, run `mvn -Perrorprone -pl <module> -am compile`
  and fix every new warning; the Error Prone / NullAway ratchet counts
  warnings that `code_quality_preflight.py` and SpotBugs do not see.
- A bot-blocked link in a research reference stays linked: add the host to
  `lychee.toml` `exclude` instead of unlinking it, since link-count contract
  tests count linked references.
