# CI status economy (babysit, digest, one channel)

Learned in coalesce wave: babysit, process-owner, and parent turns burned tokens re-reading unchanged CI state. This policy is portable. Codex, Claude, Grok CLI, Gemini, Copilot, and Grok Bot share it; no host-only memory exception.

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
- Foreground poll loops that keep a turn alive while waiting. Long-lived turns get killed by hosts (about 50 minutes has been observed), and the work dies with them.

A zero-token waiter (option 1 or 2) is the only exception to [single thread](delegation.md). Real work stays on the one thread. When the wake fires, merge on green or fix on red, then continue the queue without asking the owner whether to proceed.

## A green check can hide failing tests

Some jobs run the build with a flag that ignores test failures (for example `testFailureIgnore=true`), so the job concludes success while tests fail. When a delivery adds or changes an acceptance test, read that job's test-runner summary line (for example `Tests run: N, Failures: F`) and the named test's `Finished test method` line before declaring it proven. File any failure outside the delivery's scope; do not rely on the check conclusion alone.

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
