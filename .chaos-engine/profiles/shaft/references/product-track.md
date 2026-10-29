# Product-track self-improve (SHAFT + ChaosGauge)

Profile extension of the portable
[product-track playbook](../../../skills/self-improve/references/product-track.md).

## SHAFT filing cues

| Lesson class | Prefer |
| --- | --- |
| Engine / public API regression | Focused `shaft-engine` (or affected `shaft-*`) unittest + silent-verify |
| Doctor / CLI repair gap | `shaft-doctor` / `shaft-cli` path — not a new MCP essay surface |
| Agent effectiveness on product tasks | Link closest ChaosGauge public task under `scripts/ci/chaos_gauge/` |
| IntelliJ plugin | Focused plugin check; keep docs PR separate when docs-only |

## Routing

Product authoring still hands off to `shaft-developer` via
[shaft routing](routing.md). This playbook only covers Learning Session →
issue/eval encoding after delivery.

## Proof examples

```bash
py -3 -m unittest <focused_module> -v
python3 scripts/ci/chaos_gauge/validate_experiment.py scripts/ci/chaos_gauge/experiment.json
python3 scripts/ci/harness_pr_gate.py --base "$(git merge-base HEAD origin/main)" --head "$(git rev-parse HEAD)" --write-generated
```

Reject Task Observer; Delivery-Stop / Learning Session only.
