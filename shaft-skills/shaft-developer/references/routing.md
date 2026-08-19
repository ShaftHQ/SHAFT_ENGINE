# SHAFT skill routing

Choose one row from the user's immediate requested output. Load no adjacent
skill preemptively. For a multi-stage request, finish and verify one output,
then return here and choose the next row. If no row fits, ask for the missing
intent instead of approximating a specialist.

## Lifecycle specialists

| Immediate output | Load exactly this skill |
| --- | --- |
| Testable requirements, acceptance criteria, gaps, or traceability | [`shaft-requirements-analysis`](../../shaft-requirements-analysis/SKILL.md) |
| Test strategy, scope, risk, estimates, schedule, entry, or exit criteria | [`shaft-test-planning`](../../shaft-test-planning/SKILL.md) |
| Manual or automatable test cases, scenarios, steps, or expected results | [`shaft-test-case-design`](../../shaft-test-case-design/SKILL.md) |
| Test datasets, factories, privacy, reset, or cleanup rules | [`shaft-test-data-design`](../../shaft-test-data-design/SKILL.md) |
| Browser, device, service, dependency, configuration, or environment readiness | [`shaft-test-environment`](../../shaft-test-environment/SKILL.md) |
| Run approved tests and capture trustworthy execution evidence | [`shaft-test-execution`](../../shaft-test-execution/SKILL.md) |
| Monitor progress, gates, coverage, duration, retries, or execution health | [`shaft-test-monitoring`](../../shaft-test-monitoring/SKILL.md) |
| Diagnose one consistent failed or broken result and identify root cause | [`shaft-failure-analysis`](../../shaft-failure-analysis/SKILL.md) |
| Diagnose inconsistent pass/fail outcomes under equivalent conditions | [`shaft-flaky-test-analysis`](../../shaft-flaky-test-analysis/SKILL.md) |
| File or improve a reproducible product defect report | [`shaft-defect-reporting`](../../shaft-defect-reporting/SKILL.md) |
| Produce an engineering-facing execution or failure report | [`shaft-execution-reporting`](../../shaft-execution-reporting/SKILL.md) |
| Produce a business-facing quality, risk, or release decision report | [`shaft-stakeholder-reporting`](../../shaft-stakeholder-reporting/SKILL.md) |
| Design or assess WCAG, keyboard, assistive-technology, or mobile accessibility tests | [`shaft-accessibility-testing`](../../shaft-accessibility-testing/SKILL.md) |
| Design performance, security, reliability, compatibility, recovery, or other quality tests | [`shaft-nonfunctional-test-design`](../../shaft-nonfunctional-test-design/SKILL.md) |

## Implementation specialists

These skills own implementation detail; lifecycle skills must hand off instead
of duplicating their guidance.

| Immediate output | Load exactly this skill |
| --- | --- |
| Implement or repair automated SHAFT test code | [`shaft-automated-test-authoring`](../../shaft-automated-test-authoring/SKILL.md) |
| Design or refactor page/component objects | [`shaft-page-objects`](../../shaft-page-objects/SKILL.md) |
| Compose or correct SHAFT fluent chains | [`shaft-fluent-api`](../../shaft-fluent-api/SKILL.md) |
| Identify, rank, verify, or repair element locators | [`shaft-locator-design`](../../shaft-locator-design/SKILL.md) |
| Use SHAFT browser or element actions | [`shaft-web-actions`](../../shaft-web-actions/SKILL.md) |
| Use SHAFT mobile or touch actions | [`shaft-mobile-actions`](../../shaft-mobile-actions/SKILL.md) |
| Implement SHAFT API tests | [`shaft-api-testing`](../../shaft-api-testing/SKILL.md) |
| Implement SHAFT database tests | [`shaft-database-testing`](../../shaft-database-testing/SKILL.md) |
| Implement element, browser, API, or other SHAFT assertions | [`shaft-assertions`](../../shaft-assertions/SKILL.md) |
| Search and ground syntax in the current official SHAFT guide | [`shaft-guide-search`](../../shaft-guide-search/SKILL.md) |
| Record a live web or mobile flow | [`shaft-test-recording`](../../shaft-test-recording/SKILL.md) |
| Generate test code from a persisted recording | [`shaft-recording-codegen`](../../shaft-recording-codegen/SKILL.md) |
| Preview, guardrail-check, apply, or verify a generated change | [`shaft-change-verification`](../../shaft-change-verification/SKILL.md) |

## Tool specialists

| Immediate output | Load exactly this skill |
| --- | --- |
| Select or call an interactive, stateful, exploratory SHAFT MCP capability | [`shaft-mcp`](../../shaft-mcp/SKILL.md) |
| Select or run a deterministic one-shot SHAFT CLI command | [`shaft-cli`](../../shaft-cli/SKILL.md) |

When the requested output is generated test code from a natural-language web,
mobile, or API scenario and no persisted recording exists, load
[`shaft-test-recording`](../../shaft-test-recording/SKILL.md) first. After the
recording is persisted and replay-proven, return here and load
[`shaft-recording-codegen`](../../shaft-recording-codegen/SKILL.md). Do not send
that request to
[`shaft-automated-test-authoring`](../../shaft-automated-test-authoring/SKILL.md).

Do not infer exact MCP names, parameters, or CLI flags from this map. The two
tool specialists resolve them from the generated canonical catalogs.
