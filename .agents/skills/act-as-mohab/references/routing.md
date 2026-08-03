# Routing

Match the deliverable in front of you to one row, load that surface, and finish
its deliverable before returning here. Do not load an adjacent surface
preemptively. If no row fits, say what is missing rather than approximating.

Never make the user name an implementation detail they could not reasonably
know. Infer the row from what they asked for.

## Always, before any row

Run [consult-first](../../consult-first/SKILL.md) at the depth its triage
selects. Apply the entrypoint's Caveman, Ponytail, and TDD rules; use
[delegation](delegation.md) before dispatch and [roles](roles.md) for role
boundaries.

## Knowledge before discovery

1. Native Memory for durable constraints and gotchas.
2. MemPalace for cross-session history, relations, and impact.
3. [Graphify](graphify.md) for the current structural map.
4. Targeted `rg` and exact reads to verify live files.

Use every applicable available source. If one fails, name degraded mode and
continue with the others. A stale index never outranks a live file.

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
| One issue through a merged PR | [GitHub playbook](work-github-playbook.md) |
| Deep SHAFT internals in one expert domain | [SHAFT mastery](shaft-mastery.md), then the one matching chapter |
| Reviewing a diff for behavior that no check would catch | [verification-gap lens](verification-gap-lens.md) |
| Holding main thread at session start | [orchestrator bootstrap](orchestrator-bootstrap.md) |

## SHAFT product and test authoring

Writing, planning, recording, executing, diagnosing, or reporting SHAFT tests is
a different surface with its own router. Hand off to `shaft-developer` in the
`shaft-skills` pack and let it select the single lifecycle, implementation, or
tool specialist. Do not duplicate its rows here, and do not guess SHAFT syntax,
tool names, or CLI flags from this file.

## External facts

- Past-cutoff external library API: context7 MCP; prefer a repo exemplar otherwise.
- Live Maven Central facts: Maven MCP; for in-tree dependency declarations,
  inspect POMs first.
- Never start unavailable infrastructure implicitly.

UI routes preserve native technology: desktop surfaces use IDE and JVM
inspection with renderer evidence; web surfaces use browser evidence and an
accessibility audit.
