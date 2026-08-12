# Routing

Match the deliverable in front of you to one row, load that surface, and finish
its deliverable before returning here. Do not load an adjacent surface
preemptively. If no row fits, say what is missing rather than approximating.

Never make the user name an implementation detail they could not reasonably
know. Infer the row from what they asked for.

## Always, before any row

Take the depth the entrypoint's triage already selected, loading
[consult-first](../../../references/consult-first.md) only when that triage sends you
there. Apply the entrypoint's Caveman, Ponytail, and TDD rules; use
[delegation](../../../references/delegation.md) before dispatch and [roles](../../../references/roles.md) for role
boundaries. When the change involves a mock, a method a production class gains
only for a test, or an argument for writing the code first, add
[TDD failure modes](../../../references/tdd-failure-modes.md).

## Knowledge before discovery

Each store answers a different question. Query the ones whose trigger fires
before broad manual discovery, not all of them by reflex.

| Question you actually have | Store | Query it when |
| --- | --- | --- |
| Has this constraint or gotcha already bitten us? | native Memory | Required before any non-trivial change, and before filing an issue. |
| What happened around this before, and what does it touch? | MemPalace | The change spans entities, sessions, or a history you were not part of. |
| What calls or depends on this? | [Graphify](../../../references/graphify.md) | Blast radius is unknown, or you are about to change a shared symbol. |
| What does the code do right now? | targeted `rg` and exact reads | Always. This is the only source that settles a disagreement. |

The native store is source-controlled, and its contract travels with it:
`.memory/config.json` declares the project and store version, and
`.memory/schema/object.schema.json`, `.memory/schema/relation.schema.json`,
`.memory/schema/event.schema.json`, `.memory/schema/patch.schema.json` and
`.memory/schema/config.schema.json` validate every write. The entries
themselves are data the CLI owns; reach them with `memory load`, `memory
search` and `memory inspect`. Standing constraints need no query at all — the
session-start hook injects them before your first tool call.

A retrieved claim is a lead, never a verdict: confirm it against the live file
before acting on it, and a stale index never outranks what is on disk. Your own
plan ranks here too, below every one of them: it is the oldest source you hold,
written before the reads.

If a store is unavailable, name the degraded mode in your report and continue
with the rest. Skipping a store whose trigger fired, without saying so, is the
failure this table exists to prevent.

## Repository engineering

| Deliverable in front of you | Load exactly this |
| --- | --- |
| Agent guidance, host adapters, hooks, budgets, retrieval setup | [agent guidance](playbooks/agent-guidance-boundary-guard.md) |
| Explicit PDCA, personas, repeated refinement | [PDCA](playbooks/agentic-pdca-loop.md) |
| Production Java under `src/main/java` | [framework source](playbooks/framework-source.md) |
| Test Java under `src/test/java` | [Java tests](playbooks/java-tests.md) |
| A red CI run, job, or scheduled suite | [CI failures](playbooks/ci-failure-investigator.md) |
| Inconsistent pass/fail under equivalent conditions | [flaky tests](playbooks/flaky-test-stabilizer.md) |
| Version, release, BOM, or dependency metadata | [release and dependencies](playbooks/release-dependency-guard.md) |
| MCP transport, tool contract, or client wiring | [MCP transport](playbooks/mcp-transport-contract-auditor.md) |
| Module boundary or cross-module dependency | [module boundaries](playbooks/modular-boundary-auditor.md) |
| Allure or Extent report generation and verdicts | [reports](playbooks/allure-extent-report-operator.md) |
| Externally documented behavior that changed | [public docs](playbooks/public-behavior-docs-synchronizer.md) |
| Any visible SHAFT interface, visual QA, UX copy, accessibility | [UI design](playbooks/shaft-ui-design.md) |
| Marketing or promotional material | [marketing](playbooks/shaft-marketing-ad-producer.md) |
| One issue through a merged PR | [GitHub playbook](../../../references/work-github-playbook.md) |
| Reviewing a diff for behavior that no check would catch | [verification-gap lens](../../../references/verification-gap-lens.md) |
| Holding main thread at session start | [orchestrator bootstrap](../../../references/orchestrator-bootstrap.md) |
| A repository CI script or guard under `scripts/`, outside agent guidance | [agent guidance](playbooks/agent-guidance-boundary-guard.md), then the guard's own test |
| In-repo Markdown for humans, such as README or CONTRIBUTING | [public docs](playbooks/public-behavior-docs-synchronizer.md) |
| The `.memory` store itself: entries, relations, or hygiene | [agent guidance](playbooks/agent-guidance-boundary-guard.md) |

## Deep SHAFT internals

Read the one chapter the task actually touches, and skip the rest. Each encodes
incident history that is expensive to re-derive.

| Task touches | Load exactly this |
| --- | --- |
| Recorder, preload scripts, network capture, browser lifecycle | [Selenium BiDi](shaft-mastery/selenium-bidi.md) |
| Report generation or patching, results JSON, verdict analysis | [Allure internals](shaft-mastery/allure-internals.md) |
| Mobile recording and replay, emulators, Appium or WinAppDriver CI | [Appium mobile](shaft-mastery/appium-mobile.md) |
| Versioning, Central publishing, BOM, dependency convergence | [Maven release](shaft-mastery/maven-release.md) |
| Listeners, forked JVMs, properties precedence, scoped runs | [TestNG lifecycle](shaft-mastery/testng-lifecycle.md) |
| The IntelliJ plugin, its desktop UI, tool windows, Gradle or JDK setup | [IntelliJ plugin](shaft-mastery/intellij-plugin.md) |
| MCP tools, stdio transport, workspace roots, clients | [MCP protocol](shaft-mastery/mcp-protocol.md) |
| Red CI runs, scheduled suites, workflow YAML, sharding | [CI forensics](shaft-mastery/ci-forensics.md) |
| Races, synchronization, deterministic reproduction | [Wait strategies](shaft-mastery/wait-strategies.md) |
| Locator choice, semantic selectors, healer or doctor | [Locator healing](shaft-mastery/locator-healing.md) |

## SHAFT product and test authoring

Writing, planning, recording, executing, diagnosing, or reporting SHAFT tests is
a different surface with its own router. Hand off to `shaft-developer` in the
distributed skills pack and let it select the single lifecycle, implementation,
or tool specialist. Do not duplicate its rows here, and do not guess SHAFT
syntax, tool names, or CLI flags from this file.

## External facts

- Past-cutoff external library API: context7 MCP; prefer a repo exemplar otherwise.
- Live Maven Central facts: Maven MCP; for in-tree dependency declarations,
  inspect POMs first.
- Never start unavailable infrastructure implicitly.

UI routes preserve native technology: desktop surfaces use IDE and JVM
inspection with renderer evidence; web surfaces use browser evidence and an
accessibility audit.
