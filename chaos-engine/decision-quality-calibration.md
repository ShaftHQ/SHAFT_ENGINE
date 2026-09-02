# ChaosEngine OmniRoute decision-quality calibration

Accessed: 2026-09-02. Parent tracker: #5549. Deliverable for #5522.

Transport: local OmniRoute loopback free/remaining catalog only.
Public ChaosGauge task identities are an unchanged subset of #5450.
Missing telemetry is `UNAVAILABLE` (never `0`). No prompts, transcripts,
secrets, private paths, or provider routes are persisted.

## Campaign

| Field | Value |
| --- | --- |
| Planned trials | 12 |
| Observed trials | 12 |
| Status | complete |
| Models used | nvidia/nemotron-3-ultra-550b-a55b |
| Preferred model | agy/gemini-3.7-flash-high |
| Gate verdict | NO |

## Arm metrics

| Metric | control | chaos-engine |
| --- | --- | --- |
| `correctness` | 0.0 | 0.0 |
| `tokens` | 351.6666666666667 | 371.1666666666667 |
| `latency_seconds` | 24.54195623733055 | 36.893572025665584 |
| `external_run_minutes` | UNAVAILABLE | UNAVAILABLE |
| `actions` | 1.0 | 1.0 |
| `retries` | 0.0 | 0.0 |
| `cost_usd` | UNAVAILABLE | UNAVAILABLE |
| `variance` | 0.0 | 0.0 |

## Gate

- Verdict: **NO**
- Reason: correctness did not beat control
- Correctness delta (treatment - control): 0.0

## Method notes

- Tasks: unchanged public ChaosGauge identities
  `diagnosis-failure-trace`, `repair-regression-test`, `delivery-focused-proof`.
- Arms: `control` (bare instruction) vs `chaos-engine` (Model A treatment prompt).
- Transport: `omniroute chat` against loopback `127.0.0.1:20128` with pinned
  `provider/model` identical on both arms of each pair.
- Preferred primary `agy/gemini-3.7-flash-high` was cooling (429); remaining pairs
  used free most-intelligent failover `nvidia/nemotron-3-ultra-550b-a55b`.
- Local OmniRoute `requestQueue.maxWaitMs=15000` may bound queue wait; several
  observed latencies still exceeded 15s. Applyable patches were not emitted,
  so correctness stayed 0.0 on both arms.
- Companion machine-readable aggregate:
  `chaos-engine/decision-quality-calibration.aggregate.json`.

## Failover events

- `agy/gemini-3.7-flash-high` → `nvidia/nemotron-3-ultra-550b-a55b` (operator-pin-after-preferred-unavailable)

## Rollback

Delete this artifact, `scripts/ci/chaos_gauge/omniroute_calibration.py`,
`tests/scripts/test_omniroute_calibration.py`, and the companion aggregate
JSON; refresh ChaosGauge digests if the `chaos-engine/` tree hash changes.

