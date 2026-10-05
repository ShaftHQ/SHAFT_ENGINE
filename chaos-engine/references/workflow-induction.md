# Workflow induction (repeated successful steps)

Phase-2 harness learning (issue 6518; Agent Workflow Memory, AWM 2409.07429).
The module records privacy-safe trajectories and induces a workflow only when
the same contiguous step window appears in at least two **successful**
trajectories. Failures stay in the index and never raise support.

Publishing the induced steps as a skill file stays behind the harness eval
suite. `materialize` refuses unless
`chaos-engine/evals/harness-suite/manifest.json` still lists regression task
`reg-workflow-induction-6518` on module
`tests.scripts.test_chaos_engine_workflow_induction_6518`. Induced skills are
written under `.chaos-engine-state/workflows/skills/` and do not edit shipped
`SKILL.md` files.

## Fields

| Field | Meaning |
| --- | --- |
| `support` | Distinct successful trajectory ids that contain the window |
| Window | Contiguous steps, length 2 through 6 |
| `id` | Content hash of the window (workflows) or name plus steps (trajectories) |

## Surfaces

| Piece | Path |
| --- | --- |
| Module | [`workflow_induce.py`](../workflow_induce.py) |
| CLI | `python3 chaos-engine/workflow_induce.py trajectory\|induce\|retrieve\|materialize\|summary` (repo-only) |
| Unittest | `python3 -m unittest tests.scripts.test_chaos_engine_workflow_induction_6518 -v` (repo-only) |
| PR Gate check | `workflow-induction-contract` (surface `workflow-induction`) |

## Rules

1. Record a trajectory with `record_trajectory(name, steps, success=...)`.
2. Induce only windows that repeat across two or more successful trajectories.
3. A single trajectory, or a window that appears only on failures, produces no workflow.
4. Privacy rejects secrets, absolute paths, URLs, and backticks.
5. `retrieve` ranks matching workflows by support, then id.
6. `materialize_skill` writes one `SKILL.md` only while the eval-suite task is present.

## Related

- Pair insights stay in [insight extract](insight-extract.md).
- Eval scoring: [harness eval suite](harness-eval-suite.md).
