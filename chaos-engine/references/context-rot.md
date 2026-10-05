# Context-rot budget and compaction

Phase-3 harness check (issue 6518; Chroma Context Rot). `check` compares the
stored context to `BUDGET_CHARS` in [`context_rot.py`](../context_rot.py).
Over-budget context is `over` and `compactionNeeded`. `compact` keeps only
segments recorded with `--keep`, must shrink, and must finish inside the
budget. That write stays behind harness eval task `reg-context-rot-6518` and
lands in `.chaos-engine-state/context-rot/compacted.md`.

## Surfaces

| Piece | Path |
| --- | --- |
| Module | [`context_rot.py`](../context_rot.py) |
| CLI | `python3 chaos-engine/context_rot.py record\|check\|compact` (repo-only) |
| Unittest | `python3 -m unittest tests.scripts.test_chaos_engine_context_rot_6518 -v` (repo-only) |
| PR Gate check | `context-rot-contract` (surface `context-rot`) |
| Suite task | `reg-context-rot-6518` |

## Rules

1. `record` stores one segment. `--keep` marks text compaction must retain.
2. `check` reports `within` or `over` against the module budget.
3. `compact` refuses when the manifest drops this task, when nothing unmarked can be dropped, or when kept text is still over the budget.
4. Segment text is refused when it holds a secret, an absolute path, a URL, or a backtick.

## Related

- Archived replacements stay in [self-modify](self-modify.md).
- Scoring stays with [harness eval suite](harness-eval-suite.md).
