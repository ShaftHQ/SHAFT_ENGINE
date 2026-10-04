# Evolving playbook (helpful / harmful + deltas)

Phase-2 harness learning (issue 6532; ACE 2510.04618, Dynamic Cheatsheet
2504.07952). The heuristics store is the playbook: each item keeps
**helpful** / **harmful** counters and accepts **delta** text updates instead of
full-store rewrites. Provenance and quarantine from
[memory provenance](memory-provenance.md) still gate retrieve and promotion.

## Fields

| Field | Meaning |
| --- | --- |
| `helpful` | Non-negative count of outcomes where the item helped |
| `harmful` | Non-negative count of outcomes where the item hurt |
| `id` | Stable across delta text updates (identity is not rehashed) |

Legacy items missing counters read as `0` / `0`.

## Surfaces

| Piece | Path |
| --- | --- |
| Module | [`heuristics.py`](../heuristics.py) `record_feedback` / `apply_delta` / ranked `retrieve_top` |
| Entry-stamp verify | [`install.py`](../install.py) `is_generated_runtime_file` (ignore `runtime/`) |
| Doctor | `heuristics` summary `helpfulTotal` / `harmfulTotal` |
| CLI | `python3 chaos-engine/heuristics.py feedback\|delta\|retrieve\|summary` (repo-only) |
| Unittest | `python3 -m unittest tests.scripts.test_chaos_engine_evolving_playbook_6532 -v` (repo-only) |
| PR Gate check | `evolving-playbook-contract` (surface `evolving-playbook`) |

## Rules

1. Record feedback with `record_feedback(id, "helpful"|"harmful")` — one increment.
2. Patch text with `apply_delta(id, text=...)`; privacy gate still applies; counters and provenance stay.
3. `retrieve_top` ranks by `(helpful - harmful)` descending, then `at` recency, after the provenance filter.
4. Quarantined items never retrieve, even with high helpful counts.
5. `tool.py entry` may write `.chaos-engine/runtime/entry-stamp`; doctor must not treat that as ownership drift.

## Related

- Provenance floor: [memory-provenance](memory-provenance.md)
- Feedback and deltas feed the same Learning Session disposition path as other harness lessons
- Program epic: ChaosEngine program epic Phase 2
