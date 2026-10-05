# Harness self-modification with an archive

Phase-3 harness learning (issue 6518; DGM 2505.22954, SICA 2504.15228).
A candidate body is stored only after the harness eval suite report passes
task `reg-self-modify-6518`. Applying a later body first copies the previous
body into `.chaos-engine-state/self-modify/archive/`. Rollback restores an
archived body and archives the body it replaces. Shipped `chaos-engine/`
files are not edited.

## Surfaces

| Piece | Path |
| --- | --- |
| Module | [`self_modify.py`](../self_modify.py) |
| CLI | `python3 chaos-engine/self_modify.py propose\|score\|apply\|rollback\|summary` (repo-only) |
| Unittest | `python3 -m unittest tests.scripts.test_chaos_engine_self_modify_6518 -v` (repo-only) |
| PR Gate check | `self-modify-contract` (surface `self-modify`) |
| Suite task | `reg-self-modify-6518` |

## Rules

1. `propose` keeps a slug label and privacy-safe text, unapplied.
2. `score` refuses a suite report that did not pass, or that omitted this task.
3. `apply` refuses an unscored candidate and a manifest that dropped this task.
4. `apply` and `rollback` append the previous applied body to the archive before writing.
5. The write target stays under `.chaos-engine-state/self-modify/applied/`.

## Related

- Suite-scored text candidates stay in [prompt evolution](prompt-evolution.md).
- The report is produced by [harness eval suite](harness-eval-suite.md).
