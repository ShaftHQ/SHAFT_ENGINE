# External-signal reflection

Phase-2 harness learning (issue 6518; Reflexion 2303.11366, Huang et al.
2310.01798). A reflection is stored only when its ground is an external
signal: **tests**, **CI**, or **doctor**. Self-report, model judgment, and
any other signal are refused. The module does not edit shipped skills.

## Fields

| Field | Meaning |
| --- | --- |
| `signal` | `tests`, `ci`, or `doctor` |
| `outcome` | `pass` or `fail` from that external run |
| `summary` | Privacy-safe note of what the signal showed |

## Surfaces

| Piece | Path |
| --- | --- |
| Module | [`external_reflection.py`](../external_reflection.py) |
| CLI | `python3 chaos-engine/external_reflection.py record\|retrieve\|summary` (repo-only) |
| Unittest | `python3 -m unittest tests.scripts.test_chaos_engine_external_reflection_6518 -v` (repo-only) |
| PR Gate check | `external-reflection-contract` (surface `external-reflection`) |

## Rules

1. `record_reflection` accepts only `tests`, `ci`, and `doctor`.
2. Outcome is `pass` or `fail` from that signal, not a model score.
3. Privacy rejects secrets, absolute paths, URLs, and backticks.
4. `retrieve` returns failures before passes, then id.
5. Stop-hook receipts stay in `hooks/reflection.py`. This store is separate.

## Related

- Workflow drafts: [workflow induction](workflow-induction.md).
- Eval scoring: [harness eval suite](harness-eval-suite.md).
