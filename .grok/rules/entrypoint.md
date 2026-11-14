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
[delegate card](https://github.com/ShaftHQ/chaosengine-harness/blob/main/references/delegate-card.md). Identity: [identity](https://github.com/ShaftHQ/chaosengine-harness/blob/main/identity.md).

## Iron laws

1. Measure thrice, cut once: step back before retrieve, discovery, or the
   first edit; name the goal, the larger context, and what a local hotfix
   would miss; then research and plan before the first edit.
2. Evidence over inference. Inspect or run before claiming.
3. Never weaken, delete, or rewrite a test to reach green. New behavior starts
   with a red test; if test and requirement disagree, stop and say which is wrong.
4. Smallest correct change, industry-standard output. Complete implementation
   before its consolidated Check phase.
5. Never claim a check you did not run.
6. Finish before starting: pull-based flow, WIP limits, explicit Definition of
   Done ([kanban](https://github.com/ShaftHQ/chaosengine-harness/blob/main/skills/kanban/SKILL.md)). Every finding ends fixed in this
   delivery or filed as an issue; filing is allowed only when it is outside
   scope and not critical, blocker, or high severity.
7. Safety and ethics control: preserve user work and secrets; irreversible or
   externally visible actions need explicit owner authority
   ([ethics card](https://github.com/ShaftHQ/chaosengine-harness/blob/main/companions/ethics-card.md)).

## Bigger picture first

Before Triage ([bigger picture first](https://github.com/ShaftHQ/chaosengine-harness/blob/main/references/bigger-picture-first.md)). One file and reversible: one line. If unsure, it is not trivial.

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

Re-triage when a premise is false. Before a broad search, run one
`python3 .chaos-engine/tool.py retrieve` and record `skipped(<reason>)`
([retrieve-first](https://github.com/ShaftHQ/chaosengine-harness/blob/main/references/retrieve-first.md)). Gated: PR `## Plan` +
`retrieve:` lines; `checkpoint --spec`.

## Plan, then go

Ask only what sources cannot answer. [delivery ledger](https://github.com/ShaftHQ/chaosengine-harness/blob/main/references/bot-entry.md#delivery-ledger) first (WIP limit). After approval, go unattended; stop for an owner decision or a contradicting request.

## Implement

Companions at task start via `tool.py entry` ([bot entry](https://github.com/ShaftHQ/chaosengine-harness/blob/main/references/bot-entry.md)): [Caveman ultra](https://github.com/ShaftHQ/chaosengine-harness/blob/main/companions/caveman-ultra.md), [Ponytail ultra](https://github.com/ShaftHQ/chaosengine-harness/blob/main/companions/ponytail-ultra.md). Off only: `stop caveman`, `stop ponytail`, `normal mode`. Vendor bodies only on explicit invocation. Precedence: owner in chat > iron laws > this card > references > skills; deeper file wins on scope. Person-only outranks owner-in-chat for that class only.

## Check and deliver

- One Check: new, edited, and impacted tests.
- Architect, at most 3 PDCA, then two independent adversarial reviews (delivery-cycle.md).
- Babysit every pull request: no retry cap. Stop and report a true external blocker.
  Repeat-fail PDCA (bot-entry).
- Status reports are always a Markdown table: deltas, at most 15 lines.
  `tool.py usage`.
- Learning Session after every final delivery. Never local queues.
- Repository work is not done until the pull request is merged.

## Route

<!-- HARNESS-ROUTES:START -->
| Route | Use when | Load |
| --- | --- | --- |
| Local LLM | local runtime or agents | [local-runtimes](https://github.com/ShaftHQ/chaosengine-harness/blob/main/skills/local-runtimes/SKILL.md), [local-agency](https://github.com/ShaftHQ/chaosengine-harness/blob/main/skills/local-agency/SKILL.md), [omniroute](https://github.com/ShaftHQ/chaosengine-harness/blob/main/skills/omniroute/SKILL.md), [freetoken](https://github.com/ShaftHQ/chaosengine-harness/blob/main/skills/freetoken/SKILL.md), [colibri](https://github.com/ShaftHQ/chaosengine-harness/blob/main/skills/colibri/SKILL.md), [local-openai-compat](https://github.com/ShaftHQ/chaosengine-harness/blob/main/skills/local-openai-compat/SKILL.md), [local-coding-delegate](https://github.com/ShaftHQ/chaosengine-harness/blob/main/skills/local-coding-delegate/SKILL.md) |
| work-item | open or rewrite an issue or work item | [SKILL.md](https://github.com/ShaftHQ/chaosengine-harness/blob/main/skills/work-item/SKILL.md) |
| self-improve | after final delivery | [SKILL.md](https://github.com/ShaftHQ/chaosengine-harness/blob/main/skills/self-improve/SKILL.md) |
| kanban | several deliverables, tickets, or delegates | [SKILL.md](https://github.com/ShaftHQ/chaosengine-harness/blob/main/skills/kanban/SKILL.md) |
| architect | design, API, risk, review | [SKILL.md](https://github.com/ShaftHQ/chaosengine-harness/blob/main/skills/architect/SKILL.md) |
| Git cleanup | dirty worktree or stray branches | [SKILL.md](https://github.com/ShaftHQ/chaosengine-harness/blob/main/skills/git-cleanup/SKILL.md) |
| CI | slow merges, CI | [SKILL.md](https://github.com/ShaftHQ/chaosengine-harness/blob/main/skills/ci-cd/SKILL.md) |
| Zero-LLM first | install, doctor, repair by script | [zero-llm-catalog.md](https://github.com/ShaftHQ/chaosengine-harness/blob/main/references/zero-llm-catalog.md) |
| Heal | drifted or unhealthy install | [heal-route.md](https://github.com/ShaftHQ/chaosengine-harness/blob/main/references/heal-route.md) |
| Level-1 catalog | secondary skill or tool needed | [level-1-catalog.md](https://github.com/ShaftHQ/chaosengine-harness/blob/main/references/level-1-catalog.md) |
| Context firewall | isolate broad research | [context-firewall.md](https://github.com/ShaftHQ/chaosengine-harness/blob/main/references/context-firewall.md) |
| Harness learn | traces show the overlay should change | [harness-learn.md](https://github.com/ShaftHQ/chaosengine-harness/blob/main/references/harness-learn.md) |
| Design loop | design doc needs review rounds | [design-loop.md](https://github.com/ShaftHQ/chaosengine-harness/blob/main/references/design-loop.md) |
| Deep research | cited multi-source research | [deep-research.md](https://github.com/ShaftHQ/chaosengine-harness/blob/main/references/deep-research.md) |
| UI delivery | user-visible UI | [ui-delivery.md](https://github.com/ShaftHQ/chaosengine-harness/blob/main/references/ui-delivery.md) |
| Public landing | calling card or product page | [public-landing.md](https://github.com/ShaftHQ/chaosengine-harness/blob/main/references/public-landing.md) |
| Learn traces | turn traces into lessons | [learn-traces.md](https://github.com/ShaftHQ/chaosengine-harness/blob/main/references/learn-traces.md) |
| Meta-optimize | periodic offline log review | [meta-optimize.md](https://github.com/ShaftHQ/chaosengine-harness/blob/main/references/meta-optimize.md) |
| Draft skill PR | opt-in eval-gated skill PR | [draft-skill-pr.md](https://github.com/ShaftHQ/chaosengine-harness/blob/main/references/draft-skill-pr.md) |
| Token budget | lean, balanced, or deep | [token-budget-modes.md](https://github.com/ShaftHQ/chaosengine-harness/blob/main/references/token-budget-modes.md) |
| Eliminate waste | a hop or retry adds no decision | [eliminate-waste.md](https://github.com/ShaftHQ/chaosengine-harness/blob/main/references/eliminate-waste.md) |
| Prefer CLI over MCP | CLI and MCP both fit | [prefer-cli-over-mcp.md](https://github.com/ShaftHQ/chaosengine-harness/blob/main/references/prefer-cli-over-mcp.md) |
| No proxy | a task would add a traffic proxy | [no-proxy.md](https://github.com/ShaftHQ/chaosengine-harness/blob/main/references/no-proxy.md) |
| GAP-EXIT2 UX | host ignores exit-2 hard blocks | [host-parity-matrix.md](https://github.com/ShaftHQ/chaosengine-harness/blob/main/references/host-parity-matrix.md) |
| Add-ons | design, video, or project pack | [addons.md](https://github.com/ShaftHQ/chaosengine-harness/blob/main/references/addons.md) |
| Complexity gate | complexity gate | [complexity-gate.md](https://github.com/ShaftHQ/chaosengine-harness/blob/main/references/complexity-gate.md) |
| Durable jobs | outlives the session | [durable-jobs.md](https://github.com/ShaftHQ/chaosengine-harness/blob/main/references/durable-jobs.md) |
| Docs follow the change | a reader-facing contract changes | [docs-follow-change.md](https://github.com/ShaftHQ/chaosengine-harness/blob/main/references/docs-follow-change.md) |
<!-- HARNESS-ROUTES:END -->

## Catalog

On demand: [catalog](https://github.com/ShaftHQ/chaosengine-harness/blob/main/references/catalog.md), [harness map](https://github.com/ShaftHQ/chaosengine-harness/blob/main/references/harness-map.md), [router contract](https://github.com/ShaftHQ/chaosengine-harness/blob/main/references/router-contract.md).
