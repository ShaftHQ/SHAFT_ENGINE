# Product-track self-improve playbook

Dual-track Learning Session already queues **product** lessons via
`learning.py`. This playbook turns those lessons into durable product work —
issues, focused tests, and product-eval linkage — at harness-track economics:
prefer **CLI / doctor / silent-verify** paths over MCP essay tickets.

Load on Learning Session product review or when filing a product follow-on.
**Reject Task Observer** — capture only on delivery Stop / explicit operator
request ([activation](activation.md)).

Adopter products may ship a profile-specific extension under
`profiles/<product>/references/product-track.md` with product-locked issue
templates and eval identities. This portable file stays free of product tokens.

## When a product lesson is durable

Queue / file when the session produced a concrete product gap:

| Signal | Prefer |
| --- | --- |
| Broken public API / regression | Issue + focused unit/IT + silent-verify Check |
| Missing doctor / CLI repair path | Issue targeting doctor / CLI — not an MCP essay |
| Flaky or missing eval coverage | Issue linking product eval / ChaosGauge task identity when applicable |
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
- [ ] Focused test or product-eval/ChaosGauge link proves the gap
- [ ] Prefer CLI/doctor/silent-verify path over new MCP surface
- [ ] Proof command listed (silent on success)

## Proof
\`\`\`bash
# example — replace with the real smallest check
python3 -m unittest <focused_module> -v
# or: python3 scripts/ci/chaos_gauge/validate_experiment.py …
\`\`\`
```

File with `gh issue create` (CLI-over-MCP). Attach as sub-issue of the owning
epic when one exists. Delivery PRs `Fixes #<child>` — never the program epic.

## ChaosGauge / product-eval linkage

When the lesson is about agent/harness **effectiveness on product tasks**
(diagnosis, repair, recovery, safety, delivery):

1. Name the closest public ChaosGauge task identity under the repo's ChaosGauge
   tree (or note “needs new task” without inventing digests).
2. Prefer refreshing pins via the repo's harness PR gate `--write-generated`
   after overlay tree changes — do not hand-edit digests.
3. Prefer focused module tests first; ChaosGauge is the agent-effectiveness
   layer, not a substitute for product unit/integration proof.

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
