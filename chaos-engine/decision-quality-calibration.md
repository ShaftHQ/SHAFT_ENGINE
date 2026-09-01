# ChaosEngine decision-quality public calibration

Accessed: 2026-09-01. Parent tracking epic: #5514. Deliverable for #5522.

Imports by reference:

- metrics dictionary, sampling protocol, and taxonomy from
  `chaos-engine/decision-quality-baseline.md` (#5520)
- operational Model A rubric from `chaos-engine/decision-quality-rubric.md`
  (#5521)
- immutable Harbor / ChaosGauge public calibration contracts under
  `scripts/ci/chaos_gauge/` (#5450 lineage)

Does not redefine those contracts. Does not change public task identities,
seed, arm names, attempt count, or treatment digests.

Privacy: no prompts, transcripts, secrets, private paths, provider routes,
endpoint URLs, or runtime indexes. Model identity remains only where existing
ChaosGauge manifests already pin it.

---

## Campaign identity (frozen)

| Field | Value |
| --- | --- |
| Campaign | `calibration` |
| Seed | `5450` |
| Public tasks | 12 |
| Arms | `control`, `chaos-engine` |
| Attempts per task | 5 |
| Planned trials | 120 |
| Private resolution | not required |

Public task names and content digests stay exactly as published in
`scripts/ci/chaos_gauge/experiment.json`.

---

## Metric mapping

Ticket comparison fields map onto ChaosGauge compare output as follows.
Missing telemetry remains `UNAVAILABLE`. It is never substituted with 0, an
estimate, or a null.

| Decision-quality field | ChaosGauge source | Missing policy |
| --- | --- | --- |
| `correctness` | arm `effectiveness` | `UNAVAILABLE` |
| `tokens` | `tokensPerSuccess` when `tokenProvenance=reported` | `UNAVAILABLE` |
| `latency_seconds` | `secondsPerSuccess` | `UNAVAILABLE` |
| `external_run_minutes` | not emitted by Harbor compare today | `UNAVAILABLE` |
| `actions` | not emitted by Harbor compare today | `UNAVAILABLE` |
| `retries` | compare `retries[arm]` | `UNAVAILABLE` when absent |
| `cost_usd` | `costPerSuccess` when present | `UNAVAILABLE` |
| `variance` | width of paired 95% bootstrap interval when both bounds exist | `UNAVAILABLE` |

Harness owner: `scripts/ci/chaos_gauge/decision_quality_calibration.py`.

---

## Runtime gate

Paid public calibration may start only when all of the following are true:

1. Harbor package version is exactly `0.22.0`.
2. Docker engine access works for Harbor trial sandboxes.
3. A provider credential is present in the process environment for the pinned
   ChaosGauge agent.
4. Owner authorization for the full 120-trial public calibration budget is
   granted through `CHAOS_GAUGE_PUBLIC_CALIBRATION_AUTHORIZED=1`.

The excluded public canary workflow is not this campaign. It authorizes only
one public task and two accounted arms under a separate token budget and must
not be reused as silent permission for 120 paid trials.

Probe command:

```bash
python3 scripts/ci/chaos_gauge/decision_quality_calibration.py probe
```

Blocked evidence command (records exact missing inputs, observed trials = 0):

```bash
python3 scripts/ci/chaos_gauge/decision_quality_calibration.py blocked --out /tmp/dq-calibration-blocked.json
```

---

## Evidence status for #5522

Live probe in the implementing environment found the campaign blocked. Exact
missing inputs:

1. `harbor==0.22.0`
2. `docker-engine-access`
3. `OPENAI_API_KEY`
4. `CHAOS_GAUGE_PUBLIC_CALIBRATION_AUTHORIZED=1` (owner-authorized 120-trial
   public calibration budget)

**120 paid trials did not run.** No trial metrics were invented. The landable
slice for this issue is the harness wiring, redacted aggregate schema, blocked
evidence path, and focused regressions above.

When the four inputs are supplied, rerun the existing ChaosGauge calibration
launcher/collector/compare path, then:

```bash
python3 scripts/ci/chaos_gauge/decision_quality_calibration.py build \
  --comparison /path/to/comparison.json \
  --collect-receipt /path/to/collect.json \
  --out /path/to/redacted-aggregate.json
```

Preserve only secret-scanned redacted aggregate evidence in git or public
comments.

---

## Rollback

Delete this artifact, `scripts/ci/chaos_gauge/decision_quality_calibration.py`,
and `tests/scripts/test_decision_quality_calibration.py`, then refresh ChaosGauge
harness digests if the `chaos-engine/` tree hash changes. Baseline and rubric
artifacts remain owned by #5520 and #5521.
