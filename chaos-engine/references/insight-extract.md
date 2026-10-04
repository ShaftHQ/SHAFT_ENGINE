# Insight extraction (success / failure pairs)

Phase-2 harness learning (issue 6544; ExpeL 2308.10144, ReasoningBank
2509.25140). Agents record **experiences** (success or failure per task key),
then extract **insights** with ExpeL operators (ADD / EDIT / UPVOTE / DOWNVOTE)
and ReasoningBank **failure-distilled** strategies. No LLM inside this module:
the caller supplies privacy-safe insight text (Learning Session judgment).
Provenance and quarantine from [memory provenance](memory-provenance.md) still
gate retrieve and playbook promotion.

## Fields

| Field | Meaning |
| --- | --- |
| `importance` | ExpeL score: ADD starts at 2; UPVOTE/EDIT +1; DOWNVOTE −1; drop at 0 |
| `kind` | `pair` (success+failure), `failure-distilled`, or `success-chunk` |
| `taskKey` | Stable task identity for pairing experiences |
| `id` | Content hash; re-ADD of identical text increments importance |

## Surfaces

| Piece | Path |
| --- | --- |
| Module | [`insight_extract.py`](../insight_extract.py) |
| Playbook promote | [`heuristics.py`](../heuristics.py) via `promote_to_playbook` |
| CLI | `python3 chaos-engine/insight_extract.py experience\|pair\|distill-failure\|operator\|retrieve\|promote\|summary` (repo-only) |
| Unittest | `python3 -m unittest tests.scripts.test_chaos_engine_insight_extract_6544 -v` (repo-only) |
| PR Gate check | `insight-extract-contract` (surface `insight-extract`) |

## Rules

1. Record experiences with `record_experience(task_key, success|failure, summary)`.
2. Pair insights need at least one success **and** one failure for that task key.
3. Failure distill needs a failure experience; it stores preventative strategy text.
4. Operators follow ExpeL importance lifecycle; DOWNVOTE at zero removes the insight.
5. `retrieve_top` ranks by importance, then recency, after the provenance filter.
6. `promote_to_playbook` copies promotable insights into the evolving playbook.
7. Privacy gate matches heuristics (no secrets, paths, URLs, transcripts).

## Related

- Evolving playbook counters: [evolving-playbook](evolving-playbook.md)
- Provenance floor: [memory-provenance](memory-provenance.md)
- Program epic: ChaosEngine program epic Phase 2
