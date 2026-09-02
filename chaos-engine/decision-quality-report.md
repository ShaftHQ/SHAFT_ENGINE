# ChaosEngine decision-quality final report

Accessed: 2026-09-02. Parent tracker: #5549. Deliverable for #5525.
Supersedes the methodology-publication scope previously tracked as #5463.

Evidence source: the committed #5522 OmniRoute 12-trial paired free-model
campaign. Numeric scorecard values are not hand-copied here; they live in
[`decision-quality-calibration.md`](decision-quality-calibration.md) and
[`decision-quality-calibration.aggregate.json`](decision-quality-calibration.aggregate.json),
and the primary README evidence block is generated from that aggregate by
`scripts/ci/validate_chaos_engine_readme.py`.

Transport: local OmniRoute loopback, free/remaining catalog only. No paid
Harbor trials were required for this program slice.

---

## Findings

1. The walking skeleton completed **12/12** planned trials with balanced cover
   (3 public ChaosGauge task identities × 2 arms × 2 attempts).
2. Both arms scored **correctness 0.0**. Applyable patches were not emitted
   under the local OmniRoute generation ceiling observed during the campaign.
3. Gate verdict is **NO** (`correctness did not beat control`). Treatment did
   not win an efficiency dimension without regression: observed mean tokens and
   latency were higher on the chaos-engine arm than on control.
4. `cost_usd` and `external_run_minutes` remain literal **`UNAVAILABLE`**
   (never coerced to `0`).
5. Preferred free model `agy/gemini-3.7-flash-high` cooled (429); remaining
   pairs used free most-intelligent failover
   `nvidia/nemotron-3-ultra-550b-a55b` with the same named model on both arms
   of each pair.
6. The only global harness policy change shipped in this program is the
   process-owner / Scrum-master reference (#5550 / PR #5551). No other global
   policy mutation landed.

## Inference

- Directional evidence does **not** justify promoting gated efficiency or
  repeat-failure policy changes from this campaign.
- Zero correctness on both arms means the skeleton measured transport and
  prompt packaging more than end-to-end repair quality. Treat the gate as a
  correct fail-closed signal, not as proof that ChaosEngine harms outcomes.
- Failover and cooling behavior are first-class operational risks for free
  OmniRoute campaigns; pin and document model identity per pair.
- Transparency still requires publishing the unfavorable result in the primary
  README (#5465), without reframing or suppressing dimensions.

## Rejected ideas

| Idea | Issue | Disposition | Reason |
| --- | --- | --- | --- |
| Repeat-failure reframing and root-owner selection guard | #5523 | Rejected / closed `not_planned` | #5522 gate **NO**; correctness did not beat control |
| Context, retrieval, and tool-call economy improvements | #5524 | Rejected / closed `not_planned` | Same gate; no efficiency win without regression |
| Global policy ADR for #5523/#5524 | (none opened) | Not required | Rejected slices shipped no policy change |
| Paid Harbor 120-trial / 95% CI power for this tracker | withdrawn under #5549 | Superseded | Free OmniRoute walking skeleton is the acceptance path |

Do not reopen #5523 or #5524 implementation from this report. A future campaign
needs a new evidence gate before those ideas are reconsidered.

## Global ADR disposition

**No new global policy ADR** for this delivery.

- Shipped elsewhere: process-owner / Scrum-master reference (#5550).
- Rejected slices (#5523, #5524) produced no accepted global rule, so an ADR
  would invent policy that was never adopted.

## Limitations (directional power)

- **n=12** paired free-model trials. This is a directional walking skeleton,
  not a Harbor-powered 95% confidence-interval pilot.
- Correctness stayed at 0.0 on both arms; the campaign cannot discriminate
  harness benefit on repair quality.
- Local OmniRoute `requestQueue.maxWaitMs=15000` and model cooling/failover
  bound external validity.
- Token and latency means are descriptive only; do not over-claim efficiency
  regressions beyond the recorded gate.
- Private prompts, transcripts, secrets, private paths, and provider routes
  are intentionally absent from committed evidence.

## Ranked backlog leftovers

1. Improve the walking-skeleton task loop so applyable patches (or an explicit
   non-patch success signal) can yield non-zero correctness under free
   OmniRoute constraints.
2. Capture comparable `cost_usd` / `external_run_minutes` when free transport
   exposes them; keep `UNAVAILABLE` until then.
3. Only after a future gate beats control: reconsider narrow, evidence-bound
   slices formerly tracked as #5523 / #5524 under a new issue.
4. Keep ChaosGauge Harbor methodology available as a later powered campaign;
   do not block README transparency on it (#5450 respec under #5549).
5. Refresh ChaosGauge harness digests whenever origin-only decision-quality
   artifacts change the `chaos-engine/` tree hash.

## Scorecard linkage (#5522)

| Artifact | Role |
| --- | --- |
| [`decision-quality-calibration.aggregate.json`](decision-quality-calibration.aggregate.json) | Machine-readable redacted aggregate (source of truth) |
| [`decision-quality-calibration.md`](decision-quality-calibration.md) | Human scorecard rendered from the aggregate |
| [`README.md`](README.md) OmniRoute evidence section | Adopter-facing transparency block generated from the aggregate |
| [`scripts/ci/chaos_gauge/omniroute_calibration.py`](../scripts/ci/chaos_gauge/omniroute_calibration.py) | Runner / validator / scorecard renderer |
| PR #5552 | Landed calibration evidence (`Closes #5522`) |

Recompute / verify:

```bash
python3 scripts/ci/chaos_gauge/omniroute_calibration.py validate \
  --evidence chaos-engine/decision-quality-calibration.aggregate.json
python3 scripts/ci/validate_chaos_engine_readme.py --write
python3 -m unittest tests.scripts.test_omniroute_calibration \
  tests.scripts.test_decision_quality_readme_evidence -q
```

## Rollback and observation window

**Rollback (documentation / evidence packaging only):**

1. Remove this report and the README evidence markers/section.
2. Leave #5522 aggregate/scorecard in place unless rolling back that PR as
   well; if removing calibration artifacts, also delete
   `scripts/ci/chaos_gauge/omniroute_calibration.py` and
   `tests/scripts/test_omniroute_calibration.py`.
3. Refresh ChaosGauge digests with
   `python3 scripts/ci/chaos_gauge/validate_experiment.py --write scripts/ci/chaos_gauge/experiment.json`.
4. Revert documentation allowlist / origin-only entries added for these files.

**Observation window:** watch adopter README readers and follow-on issues for
misreading the directional skeleton as a powered Harbor win/loss claim. If
that confusion appears, tighten the README label and link text; do not invent
metrics.

## Companion documentation disposition

No companion PR on `ShaftHQ/shafthq.github.io` for this slice.

Evidence: #5465 acceptance (2026-09-02 respec) publishes transparency in the
ChaosEngine primary README inside `ShaftHQ/SHAFT_ENGINE`. This delivery does
not change user-facing SHAFT product/adopter functional docs; methodology
publication formerly tracked as #5463 is superseded here as harness-origin
evidence, not site docs.
