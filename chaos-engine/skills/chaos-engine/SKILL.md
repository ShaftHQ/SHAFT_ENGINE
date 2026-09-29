---
name: chaos-engine
description: >-
  Canonical provider-neutral skill router and working contract. Use at the start
  of every task, on every host, in every main thread and delegate, before
  discovery, planning, edits, or answering.
license: MIT
---

# ChaosEngine

Router core card for every host, main thread, and delegate. Host adapters
point here and never restate policy. Size the work, pick the one surface it
needs, load that surface, and work under the contract below.

## Iron laws

1. Research and plan before implementation. Complete the
   [research receipt](../../references/research-receipt.md) for every task;
   triage changes depth, never ordering.
2. Evidence over inference. Inspect or run before claiming.
3. Complete implementation before its consolidated Check phase. Never claim
   success before that Check runs.
4. Never weaken, delete, or rewrite a test to reach green. When a test and the
   requirement disagree, stop and report which one you believe is wrong.
5. Never claim a check you did not run.
6. Terminal adversarial review is on. Run at most two rounds only after
   complete implementation and CI fixes. No hook may force tests or reviews
   mid-implementation.
7. During planning, ask the open decisions. Skip those questions only when
   the owner explicitly asked for unattended planning.

## Triage

Before task-specific discovery, answer both in one line each:

- **Blast radius** — one file, one module, or a public contract and its callers.
- **Reversibility** — undone by deleting the diff, or does it touch persisted
  data, a published artifact, or an external system?

Take depth from the worse answer; every row loads
[consult-first](../../references/consult-first.md):

| Triage result | Depth |
| --- | --- |
| One file, reversible | Concise complete receipt. |
| One module, reversible | Normal full pass. |
| Public contract, many callers, or hard to reverse | Executable specification and full pass. |

Re-triage when a premise turns out false, the third fix for one symptom fails,
the blast radius grows, or the user adds scope.

Retrieve: graph and memory first, when it pays — one bounded retrieve per
project task area, never for harness files, script runs, or named files
([retrieve-first](../../references/retrieve-first.md)). Bound reads:
[context economy](../../references/context-economy.md); prefer one script over
a long tool chain: [script first](../../references/script-first.md).

## Contract

Load the [router contract](../../references/router-contract.md) for the
operating contract, implementation preflight, red flags, project profile,
task isolation, ethics (EC1-EC7, controlling), companions (Caveman +
Ponytail at ultra on the implement path), portability, validation scope,
roles, ownership, reflection, and the Learning Session. Delegates load the
[delegate card](../../references/delegate-card.md) instead. After confirmed
delivery run exactly one root-owned Learning Session immediately before the
final report. Untouched chaos-engine files are not a valid skip. Load
self-improve for that session. A ChaosEngine harness lesson, finding, or
potential enhancement is a GitHub issue only. Do not write it to a local queue or into chat. The [installer](../../install.py) and
[bootstrap](../../bootstrap.py) own install;
`tests/scripts/test_chaos_engine_bootstrap.py` runs the clean/update/failure
flow on Linux, macOS, and Windows.

## Route

Load one surface from the table, finish its deliverable, then return here.

<!-- HARNESS-ROUTES:START -->
| Route | Use when | Load |
| --- | --- | --- |
| Zero-LLM first | Use when install, doctor, or repair can run as a script before any host chat. Prefer these deterministic paths. | [zero-llm-catalog.md](../../references/zero-llm-catalog.md) |
| Heal | Use when a drifted install, wiped runtime, or unhealthy doctor must be repaired by file path. | [heal-route.md](../../references/heal-route.md) |
| Level-1 catalog | Use when a task needs a secondary skill or tool beyond the core router. The list stays short and sorted. | [level-1-catalog.md](../../references/level-1-catalog.md) |
| Context firewall | Use when research or a multi-file explore should run in an isolated subagent so the parent stays small. | [context-firewall.md](../../references/context-firewall.md) |
| Harness learn | Use when repeated session traces show the git-tracked overlay should change; tune the harness in the repo, never under `~/.grok/skills`. | [harness-learn.md](../../references/harness-learn.md) |
| Design loop | Use when a design document needs write-review-revise rounds until zero open review issues remain. | [design-loop.md](../../references/design-loop.md) |
| Deep research | Use when a question needs bounded parallel research with verification and a cited final report. | [deep-research.md](../../references/deep-research.md) |
| UI delivery | Use when a change touches user-visible UI, layout, styling, or themes: red-then-green e2e, measured geometry, viewport x theme matrix. | [ui-delivery.md](../../references/ui-delivery.md) |
| Learn traces | Use when session traces must be mapped, reduced, and verified into lessons without a host TUI runner. | [learn-traces.md](../../references/learn-traces.md) |
| Meta-optimize | Use when a periodic offline review of shared logs is due. This is not a continuous session hook. | [meta-optimize.md](../../references/meta-optimize.md) |
| Draft skill PR | Use when an opt-in eval-gated draft skill pull request is requested. The default is off. | [draft-skill-pr.md](../../references/draft-skill-pr.md) |
| Token budget | Use when triage or the environment selects an ultra-lean, balanced, or deep token budget. | [token-budget-modes.md](../../references/token-budget-modes.md) |
| Eliminate waste | Use when a hop, retry, or duplicate tool does not change the next decision and should be dropped. | [eliminate-waste.md](../../references/eliminate-waste.md) |
| Prefer CLI over MCP | Use when both a CLI and an MCP server can do the same job. Prefer the CLI, and gh when it is configured. | [prefer-cli-over-mcp.md](../../references/prefer-cli-over-mcp.md) |
| No proxy | Use when a task would install, pin, or wrap a traffic proxy. Never install one. | [no-proxy.md](../../references/no-proxy.md) |
| GAP-EXIT2 UX | Use when Grok or Copilot may not honor an exit-2 hard block and the compensating checklist applies. | [host-parity-matrix.md](../../references/host-parity-matrix.md) |
| Codacy Complexity | Use when a classifier or interaction change must treat Codacy Complexity ACTION_REQUIRED as a unit failure. | [codacy-complexity-gate.md](../../references/codacy-complexity-gate.md) |
| Git cleanup | Use when a git worktree is dirty or another local branch or worktree exists. Ask before cleanup unless the session is unattended. | [SKILL.md](../git-cleanup/SKILL.md) |
<!-- HARNESS-ROUTES:END -->

## Catalog

Skills, vendor companions, roles, and routes live in the generated
[catalog](../../references/catalog.md) from
[harness-index.json](../../harness-index.json). Load a body on demand.
