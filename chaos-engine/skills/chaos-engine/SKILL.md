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
final report. The [installer](../../install.py) and
[bootstrap](../../bootstrap.py) own install;
`tests/scripts/test_chaos_engine_bootstrap.py` runs the clean/update/failure
flow on Linux, macOS, and Windows.

## Route

The selected project profile maps the deliverable in front of you to the one
surface that owns it. The entrypoint makes that choice; callers do not bypass it
by invoking a playbook directly. Load one surface, finish its deliverable, then
return here for the next.

| Route | Use when | Load |
| --- | --- | --- |
| Zero-LLM first | Before chat discovery for install/doctor/repair | [zero-llm-catalog](../../references/zero-llm-catalog.md) |
| Heal | Drifted install, wiped runtime, unhealthy doctor | [heal-route](../../references/heal-route.md) (file path; no plugin required) |
| Level-1 catalog | Need a secondary skill/tool beyond this router | [level-1-catalog](../../references/level-1-catalog.md) |
| Context firewall | Research / multi-file explore needs isolation | [context-firewall](../../references/context-firewall.md) |
| Harness learn | Tune git-tracked harness from repeated traces | [harness-learn](../../references/harness-learn.md) |
| Design loop | Write-review-revise a design doc until 0 open issues | [design-loop](../../references/design-loop.md) |
| Deep research | Bounded parallel research with independent verify and cited report | [deep-research](../../references/deep-research.md) |
| Learn traces | Map-reduce-verify session traces; portable, no host TUI | [learn-traces](../../references/learn-traces.md) |
| Meta-optimize | Periodic offline shared-log review (not continuous) | [meta-optimize](../../references/meta-optimize.md) |
| Draft skill PR | Opt-in eval-gated draft skill PRs; default OFF | [draft-skill-pr](../../references/draft-skill-pr.md) |
| Token budget | Triage or env selects ultra-lean / balanced / deep | [token-budget-modes](../../references/token-budget-modes.md) |
| Eliminate waste | Token optimization: drop hops that do not change the next decision | [eliminate-waste](../../references/eliminate-waste.md) |
| Prefer CLI over MCP | CLI when both exist; `gh` when configured; never default GitHub MCP | [prefer-cli-over-mcp](../../references/prefer-cli-over-mcp.md) |
| No proxy | Never install a traffic proxy | [no-proxy](../../references/no-proxy.md) |
| GAP-EXIT2 UX | Grok/Copilot may not honor exit-2 hard blocks | [host-parity-matrix](../../references/host-parity-matrix.md) checklist |
| Codacy Complexity | Classifier / interaction PRs; Complexity ACTION_REQUIRED == unit red | [codacy-complexity-gate](../../references/codacy-complexity-gate.md) checklist |

## Catalog

Skills, vendor companions, roles, and routes live in the generated
[catalog](../../references/catalog.md) from
[harness-index.json](../../harness-index.json). Load a body on demand.
