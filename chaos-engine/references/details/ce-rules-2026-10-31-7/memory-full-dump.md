# Memory full-dump export

## Rule

Back up **all** team memories (shared + every bot's private) to `/workspace` on a
regular cadence, then abstract reusable rules into ChaosEngine. A dump is
**incomplete** until `exported_unique == total` (when the host reports total) and
no query is stuck on the top-N recall cap (default 50).

## Procedure

1. Seed many RecallMemory queries (role, date window, lesson ids, charter facets).
2. Collect hits; exact-dedupe; redact secrets / prices / personal identifiers.
3. Count-check via `memory_full_dump.merge_batches` — FAIL the dump receipt when
   `count_ok` is false.
4. If incomplete: `ReadTranscript` fallback for missing spans; merge; re-check.
5. Write `<bot>-full.md` + `<bot>-abstract.md` (generic rules only → CE).
6. After a CE release that lands new lessons, bots re-run the dump.

Module: [`memory_full_dump.py`](../../../rules/memory_full_dump.py).

## Check

- [ ] `count_ok=true` on the dump receipt
- [ ] No secrets / client / pricing / personal email in the dump tree
- [ ] Abstract rules folded into CE (or explicitly skipped as dup)

## Why

Recall APIs return a top-N page. Coach's 2026-10-31 dump exported 234 of a
355-hit pool because each query capped at 50 — silently incomplete without a
count gate.
