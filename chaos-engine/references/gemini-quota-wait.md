---
name: gemini-quota-wait
description: Use when Gemini returns 429/quota or the UI greys out Create video / Deep Research; wait for the documented reset, then one-shot resume.
---

# Gemini quota-wait

## Rule

1. On API **429** / quota errors: read reset from `Retry-After` or the quota
   window; log the exact resume time; **wait that long** with a one-shot
   scheduled retry (no polling loop); then auto-resume the queued Gemini job.
2. Meanwhile work the next **non-Gemini** item.
3. Gemini **UI** exhaustion may show no error (Create video / Deep Research
   greyed out; tooltip "Bring ideas to life"). Detect the disabled control,
   use the documented ~5 h refresh from the last generation, and send sparse
   one-shot retry probes.

## Note

Full durable-job wiring is a follow-up feature. This card freezes the behaviour
contract so workers do not spin on quota.

## Sources

Owner via Cortex + Carl UI note 2026-10-30.
