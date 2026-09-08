# Context firewall (research / explore)

HumanLayer pattern: use an **isolated subagent** (or host Task / equivalent)
for research and multi-file explore so explore noise stays in the child and
the parent stays in the smart zone. **Not** Task Observer — spawn only when
the work needs isolation; never always-on observation.

## When to spawn

Spawn a research/explore subagent when **any** of:

- Multi-file or cross-module discovery before a decision
- Broad web / doc / codebase survey that would dump large tool output
- Parallel explore of competing approaches
- Host supports subagents / Task / local-coding-delegate for bounded labor

Stay on the parent thread when:

- One-file reversible read with a known path
- Mechanical inventory already specified by the orchestrator
- Host has **no** subagent/Task primitive (then apply the return contract
  yourself: distill in-place; do not paste transcripts forward)

## Return contract (parent receives only)

1. **Distillate** — what changed / what was proved / what remains (≤~½ page)
2. **Citations** — `path:line` (or URL + date for online) that support the claim
3. **Decision inputs** — steelman of rejected approach when comparing options
4. **Never** — raw tool transcripts, full file dumps, or multi-thousand-line logs

Spill large artifacts to disk; tell the parent the path + one discriminating fact
([context economy](context-economy.md)).

## Router rule (all hosts)

`research / multi-file explore → subagent|Task when host supports → return
filepath:line + distillate only`.

Encode lasting behavior in this Level-1 ref +
[chaos-engine skill](../skills/chaos-engine/SKILL.md) /
[delegation](delegation.md) — never Grok-Bot-only memory.

Optional local labor: [local-coding-delegate](../skills/local-coding-delegate/SKILL.md)
or [OmniRoute](../skills/omniroute/SKILL.md) when the decider chooses them;
missing optional transport never weakens this firewall.

## Token economics

| Surface | Cost |
| --- | --- |
| Parent | ↓ (no explore dumps) |
| Child | may ↑ — net quality win |
| SessionStart | unchanged (locator only) |
| Always-on Observer | **Reject** |

## Verify (zero-LLM / docs)

```bash
test -f chaos-engine/references/context-firewall.md
rg -n "context-firewall|filepath:line|Task Observer" chaos-engine/references/level-1-catalog.md chaos-engine/skills/chaos-engine/SKILL.md
```
