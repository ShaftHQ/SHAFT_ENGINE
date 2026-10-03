---
name: chaos-engine
description: >-
  Canonical provider-neutral skill router and working contract. Use at the start
  of every task, on every host, in every main thread and delegate, before
  discovery, planning, edits, or answering.
license: MIT
---

# ChaosEngine

Core card for every host and main thread; delegates load the
[delegate card](../../references/delegate-card.md). Adapters never restate
policy. Identity: [identity](../../identity.md).

## Iron laws

1. Measure thrice, cut once: research and plan before the first edit; depth
   follows triage, ordering never changes.
2. Evidence over inference. Inspect or run before claiming.
3. Never weaken, delete, or rewrite a test to reach green. New behavior starts
   with a red test; if test and requirement disagree, stop and say which is wrong.
4. Smallest correct change, industry-standard output. Complete implementation
   before its consolidated Check phase.
5. Never claim a check you did not run.
6. Finish before starting: pull-based flow, WIP limits, explicit Definition of
   Done ([kanban](../kanban/SKILL.md)). Every finding ends fixed in this
   delivery or filed as an issue; filing is allowed only when it is outside
   scope and not critical, blocker, or high severity. Nothing stays only in
   chat, a report, or a local file.
7. Safety and ethics control: preserve user work and secrets; irreversible or
   externally visible actions need explicit owner authority.

## Triage

Answer in one line each before task-specific discovery:

- **Blast radius**: one file, one module, or a public contract and its callers.
- **Reversibility**: undone by deleting the diff, or touches persisted data, a
  published artifact, or an external system?

| Worse answer | Depth |
| --- | --- |
| One file, reversible | Read, fix, prove. No plan document. |
| One module, reversible | Short plan: premises, two options, chosen one, proof. |
| Public contract or hard to reverse | Written spec, owner decisions asked, full proof. |

Re-triage when a premise is false, a third fix fails, or scope grows. Before
the first broad search or unnamed read in a project area, run one
`python3 .chaos-engine/tool.py retrieve --store graphify|mempalace "<q>"`
(graphify: structure; mempalace: history) and record `retrieve: used` or
`skipped(<reason>)` ([retrieve-first](../../references/retrieve-first.md)).
Bound reads; prefer one script.

## Plan, then go

During planning, ask only questions whose answer changes the plan, and never
one the repository or sources can answer. Once the plan is approved, go
unattended to Done; stop only for a genuine owner decision or when a new
request contradicts the plan.

## Implement

Load both companion cards at task start, before discovery:
[Caveman ultra](../../companions/caveman-ultra.md) (chat, internal notes,
handoffs) and [Ponytail ultra](../../companions/ponytail-ultra.md). Bots
without hooks start from [bot entry](../../references/bot-entry.md). Off only
with `stop caveman`, `stop ponytail`, or `normal mode`. Vendor bodies load only
on explicit invocation.

## Check and deliver

- Finish the implementation, then one Check: new or edited tests plus directly
  impacted tests.
- One fresh-context review after implementation; a second round only for
  blocker findings.
- One blocking CI wait per push; on failure fix the isolated cause, retry at
  most once, then stop and report. Never re-run checks that already passed.
- Status reports are always a Markdown table (one row per item), then Risks;
  token use per task: `python3 .chaos-engine/tool.py usage`.
- Learning Session only on trigger: a defect escaped, a surprise contradicted
  the harness, or the owner asks. Then load
  [self-improve](../self-improve/SKILL.md). Otherwise one line in the final
  report. Harness lessons become issues, never local queues.

## Route

<!-- HARNESS-ROUTES:START -->
| Route | Use when | Load |
| --- | --- | --- |
| Local LLM | optional local runtime or local agents | [local-runtimes](../local-runtimes/SKILL.md), [local-agency](../local-agency/SKILL.md), [omniroute](../omniroute/SKILL.md), [freetoken](../freetoken/SKILL.md), [colibri](../colibri/SKILL.md), [local-openai-compat](../local-openai-compat/SKILL.md), [local-coding-delegate](../local-coding-delegate/SKILL.md) |
| work-item | open or rewrite an issue or work item | [SKILL.md](../work-item/SKILL.md) |
| self-improve | Learning Session trigger fired | [SKILL.md](../self-improve/SKILL.md) |
| kanban | several deliverables, tickets, or delegates | [SKILL.md](../kanban/SKILL.md) |
| Git cleanup | dirty worktree or stray branches | [SKILL.md](../git-cleanup/SKILL.md) |
| Zero-LLM first | install, doctor, repair by script | [zero-llm-catalog.md](../../references/zero-llm-catalog.md) |
| Heal | drifted or unhealthy install | [heal-route.md](../../references/heal-route.md) |
| Level-1 catalog | secondary skill or tool needed | [level-1-catalog.md](../../references/level-1-catalog.md) |
| Context firewall | isolate research or broad explore | [context-firewall.md](../../references/context-firewall.md) |
| Harness learn | traces show the overlay should change | [harness-learn.md](../../references/harness-learn.md) |
| Design loop | design doc needs review rounds | [design-loop.md](../../references/design-loop.md) |
| Deep research | cited multi-source research | [deep-research.md](../../references/deep-research.md) |
| UI delivery | user-visible UI | [ui-delivery.md](../../references/ui-delivery.md) |
| Learn traces | turn traces into lessons | [learn-traces.md](../../references/learn-traces.md) |
| Meta-optimize | periodic offline log review | [meta-optimize.md](../../references/meta-optimize.md) |
| Draft skill PR | opt-in eval-gated skill PR | [draft-skill-pr.md](../../references/draft-skill-pr.md) |
| Token budget | pick lean, balanced, or deep budget | [token-budget-modes.md](../../references/token-budget-modes.md) |
| Eliminate waste | a hop or retry adds no decision | [eliminate-waste.md](../../references/eliminate-waste.md) |
| Prefer CLI over MCP | CLI and MCP both fit | [prefer-cli-over-mcp.md](../../references/prefer-cli-over-mcp.md) |
| No proxy | a task would add a traffic proxy | [no-proxy.md](../../references/no-proxy.md) |
| GAP-EXIT2 UX | host ignores exit-2 hard blocks | [host-parity-matrix.md](../../references/host-parity-matrix.md) |
| Add-ons | optional add-on: design, video, project pack | [addons.md](../../references/addons.md) |
| Complexity gate | static-analysis complexity gate | [complexity-gate.md](../../references/complexity-gate.md) |
<!-- HARNESS-ROUTES:END -->

## Catalog

On demand: [catalog](../../references/catalog.md),
[router contract](../../references/router-contract.md),
[context economy](../../references/context-economy.md),
[retrieve-first](../../references/retrieve-first.md),
[script first](../../references/script-first.md).
