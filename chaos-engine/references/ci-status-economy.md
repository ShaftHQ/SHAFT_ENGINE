# CI status economy (babysit, digest, one channel)

Learned in the PR #6158 coalesce wave (epic #6161): babysit, process-owner, and parent turns burned tokens re-reading unchanged CI state. This policy is portable. Codex, Claude, Grok CLI, Gemini, Copilot, and Grok Bot share it; no host-only memory exception.

## Digest only (#6162)

- Agent-facing CI status comes from `python3 scripts/agents/watch_pr_checks.py --pr <n> --until-merged --digest` (one blocking watch).
- Never paste a raw `statusCheckRollup` or a full check-runs list into chat, an executor prompt, or a status report.
- Digest schema:

```json
{
  "sha": "<head>",
  "state": "green|red|pending|merged",
  "failing": [{"name": "...", "url": "..."}],
  "failing_total": 0,
  "pending_count": 0,
  "codacy_action_required": [],
  "updated_at": "<UTC>"
}
```

`failing` is capped at 10; the whole digest stays under 4096 bytes (`DIGEST_MAX_BYTES`). RED keeps `failingJobs` in the same JSON object.
- The watch emits only on state change or terminal state; no heartbeat tokens.
- `rollup_is_waste()` in the same script flags a tool result that pastes a rollup above the digest budget.

## One status channel (#6163)

- Exactly one status channel per in-flight PR: the armed watch. Start it with `--status-lease --digest-out .chaos-engine/runtime/digest-<pr>.json`; it records `.chaos-engine/runtime/status-lease-<pr>.json`.
- Scheduled process-owner status routines run `python3 scripts/agents/status_lease.py routine --pr <n> --last-reported <state>`; it prints nothing while the lease is live and unchanged, and one line on `red` / `merged` or when no live watch exists.
- Parent re-entry does not re-narrate unless the watch returned RED/MERGED or the owner asked (`--owner-asked`).
- Link: [process-owner](process-owner-scrum-master.md), [orchestrator follow-through](orchestrator-follow-through.md).

## Fingerprint-first failed logs (#6167)

- If the job summary, annotation, or evidence JSON already names a fingerprint, read at most 40 lines around it:

  ```bash
  python3 chaos-engine/skills/local-agency/scripts/executor_brief.py log-budget <log> --fingerprint <text>
  ```

- Otherwise spill the full log to disk and keep only path + fingerprint + tail in context. `tip_preflight.classify_failure()` maps a summary line to a known fingerprint.

## Known fingerprints

| Fingerprint | Meaning | Babysit move |
|-------------|---------|--------------|
| `graphify-empty-output-after-version` | Windows graphify.exe exited non-zero with empty streams after a healthy `--version` (#6166); installer retries once, then absorbs and emits the fingerprint | Known flake: do not open a tip; the installer on `main` absorbs it |
| `inventory-drift` | `source-derived inventory drift: <section>` | Refresh README inventory in the same tip ([tip-churn preflight](tip-churn-preflight.md)) |
| `bandit-b607` | partial executable path | Resolve with `shutil.which`; see [tip-churn preflight](tip-churn-preflight.md) |
| `memory-content-hash` | stale Memory `content_hash` | `tip_preflight.py --rehash` in the same tip |
| `codacy-action-required` | Codacy ≥medium finding | Blocking; see [Codacy ACTION_REQUIRED gate](codacy-action-required-gate.md) |

## Executor prompt schema (#6167)

Task / executor / `codex exec` prompts carry a pointer plus the delta slice only; lint with `executor_brief.py lint` (cap 4096 inline bytes; no completed todos from another wave).

```text
brief_path: /tmp/<wave>/brief.md
goal: <one sentence>
constraints: <budgets, forbidden paths>
files: <paths this slice may touch>
red: python3 -m unittest <module> -v
```

## Related

- [`watch_pr_checks.py`](../../scripts/agents/watch_pr_checks.py), [`status_lease.py`](../../scripts/agents/status_lease.py), [`executor_brief.py`](../skills/local-agency/scripts/executor_brief.py)
- [Tip-churn preflight](tip-churn-preflight.md) · [Codacy ACTION_REQUIRED gate](codacy-action-required-gate.md)
