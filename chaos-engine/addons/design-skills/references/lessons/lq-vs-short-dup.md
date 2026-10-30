---
name: lq-vs-short-dup
description: Use when scheduling a long-form recut that shares a source with Shorts or Q clips; dedupe titles and spans.
---

# L43 Long vs Short dedupe

## Use when

SIGN / schedule of any long chapter or LQ that shares a source with Shorts or Q items.

## Rules

1. **Title must differ** from every scheduled Short (normalized).
2. Same-source span overlap is allowed **only** when the long cut is a **proper superset** of the Short window **and** long duration ≥ **3×** Short duration.
3. Any other overlap → **fail closed**.
4. When a sheet still carries a full window as "Q", set dedupe start/end to the **hook** window used for long-form dedupe.

## Verify

- [ ] Dup gate inventory lists compared titles (normalized) and span math.
- [ ] Superset + ≥3× proof attached, or FAIL recorded.

## Sources

- Lesson index row deferred until after the Coach video sign-off PR merges.

- Field conflict where a long remux reused a Short title/span.
