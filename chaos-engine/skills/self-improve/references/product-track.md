# Product-track self-improve playbook (SHAFT + ChaosGauge)

Dual-track Learning Session already queues **product** lessons via
`learning.py`. This playbook turns those lessons into durable SHAFT work —
issues, focused tests, and ChaosGauge linkage — at harness-track economics:
prefer **CLI / doctor / silent-verify** paths over MCP essay tickets.

Load on Learning Session product review or when filing a product follow-on.
**Reject Task Observer** — capture only on delivery Stop / explicit operator
request ([activation](activation.md)).

## When a product lesson is durable

Queue / file when the session produced a concrete product gap:

| Signal | Prefer |
| --- | --- |
| Broken public API / regression | Issue + focused unit/IT + silent-verify Check |
| Missing doctor / CLI repair path | Issue targeting `shaft-doctor` / CLI — not an MCP essay |
| Flaky or missing eval coverage | Issue linking ChaosGauge task identity or eval-parity fixture |
| Docs-only product truth drift | Separate docs PR (repo rule); still queue the lesson |
| Nothing durable | Report `product queued 0` / `nothing durable` — valid |

Privacy: use `learning.py queue` only — no secrets, absolute paths, raw
transcripts ([observation taxonomy](observation-taxonomy.md)).

## Issue template (product child)

```markdown
## Parent
Product follow-on from Learning Session (epic or delivery PR: #<n>).

## Problem
<one paragraph; privacy-safe>

## Acceptance
- [ ] Focused test or ChaosGauge/eval link proves the gap
- [ ] Prefer CLI/doctor/silent-verify path over new MCP surface
- [ ] Proof command listed (silent on success)

## Proof
\`\`\`bash
# example — replace with the real smallest check
py -3 -m unittest <focused_module> -v
# or: python3 scripts/ci/chaos_gauge/validate_experiment.py …
\`\`\`
```

File with `gh issue create` (CLI-over-MCP). Attach as sub-issue of the owning
epic when one exists. Delivery PRs `Fixes #<child>` — never the program epic.

## ChaosGauge linkage

When the lesson is about agent/harness **effectiveness on product tasks**
(diagnosis, repair, recovery, safety, delivery):

1. Name the closest public ChaosGauge task identity under
   `scripts/ci/chaos_gauge/` (or note “needs new task” without inventing digests).
2. Prefer refreshing pins via
   `python3 scripts/ci/harness_pr_gate.py --write-generated` after `chaos-engine/`
   tree changes — do not hand-edit digests.
3. For SHAFT product modules (engine, MCP, doctor, IntelliJ), prefer focused
   Maven/unittest proof first; ChaosGauge is the agent-effectiveness layer,
   not a substitute for module tests.

Profile routing for SHAFT product authoring remains
[shaft routing](../../../profiles/shaft/references/routing.md) /
`shaft-developer` — this playbook does not restate locator or test syntax.

## Economics (same as harness track)

| Prefer | Avoid |
| --- | --- |
| `gh`, doctor, `silent_verify.py`, focused unittest | MCP wrappers for the same job |
| One issue + one proof command | Essay tickets without acceptance/proof |
| Delivery-Stop Learning Session | Mid-turn product Observer |
| CLI `--fix-next-only` / catalog rows | Chat discovery of known repair paths |

See [zero-llm-catalog](../../../references/zero-llm-catalog.md) and
[silent-verify](../../../references/silent-verify.md).

## Learning Session report line

Always end product review with counts the root can paste:

`harness queued N / product queued N / nothing durable`
