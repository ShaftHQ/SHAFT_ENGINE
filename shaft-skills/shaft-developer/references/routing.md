# SHAFT skill routing

Choose one row from the user's immediate requested output. Load no adjacent
skill preemptively. For a multi-stage request, finish and verify one output,
then return here and choose the next row. If no row fits, ask for the missing
intent instead of approximating a specialist.

## Product stages (Design, Automation, Reporting)

SHAFT's IntelliJ, CLI, MCP, and agentic installer share three product stages
(#5943). Design uses MCP `design_*` / CLI `shaft design`. Automation live record
is a client of MCP `capture_*` plus CLI `shaft capture` / offline `shaft codegen`
— never invent a plugin-only recording protocol. Reporting uses MCP `report_*` /
CLI `shaft report` plus `shaft doctor`. Healer and Doctor flake analytics stay
in Reporting (Stage 3); do not fold heal into record or codegen. ChaosEngine's
installer is a separate product and must not publish `shaft-mcp` into CE
`.mcp.json`.

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
`shaft-test-recording` first. After the recording is persisted and replay-proven,
return here and load `shaft-recording-codegen`. Do not send that request to
`shaft-automated-test-authoring`.

Do not infer exact MCP names, parameters, or CLI flags from this map. The two
tool specialists resolve them from the generated canonical catalogs.


## Design stage tools (#5943)

Curated MCP `design_*` tools share CLI `shaft design` aliases (same dispatcher as the plugin):

- `design_ingest` / `shaft design ingest` — story and AC ingest
- `design_analyze` / `shaft design analyze` — testability analysis
- `design_gherkin_draft` / `shaft design gherkin` — declarative Gherkin draft
- `design_examples` / `shaft design examples` — Scenario Outline examples
- `design_lexicon` / `shaft design lexicon` — ubiquitous-language catalog
- `design_coverage` / `shaft design coverage` — requirement-to-scenario coverage
- `design_lint` / `shaft design lint` — Gherkin quality lint
- `design_gap_map` / `shaft design gap-map` — fluent API gap map
- `design_readiness` / `shaft design readiness` — Ready / Needs questions gate
- `design_handoff` / `shaft design handoff` — approved pack into Automation

Missing MCP for live design analyze fails with the same recovery as other CLI commands. Gherkin is a review artifact; Java remains SoT.

## Reporting history (S3-01)

Use MCP `report_history` / CLI `shaft report history` for cross-run Allure history in the IDE Reporting canvas. Never confuse with HealingHistoryStore (#3315). Honor `allure.accumulateHistory`; never replace the allure-results root.

Use MCP `report_flake` / CLI `shaft report flake` for dual flake definitions (retry-hidden vs transitions). Never collapse into one score; unknown history is explicit; CI commit metadata is optional (#5968).

Use MCP `report_clusters` / CLI `shaft report clusters` for unique-error clustering by Doctor historical-signature keys (error → impacted tests). No cloud ML; empty results are empty-state (#5969).
Use MCP `report_heal` / CLI `shaft report heal` for HealingDecision status counts with persist-on-pass review gate (RECOVERED+pass → reviewable diff; AMBIGUOUS no apply-to-source; NO_CANDIDATES shown; never auto-land). Reuse `healer_run_failed_test` (#5972).
Use MCP `doctor_cause_label` / CLI `shaft doctor cause-label` to confirm or suggest persisted Doctor defect labels (PRODUCT/TEST/ENVIRONMENT/LOCATOR/TIMING) keyed by signature. Local gitignored store; no cloud ML (#5971).


## Reporting canvas (S3-10)

- `report_open` / `report_summary` — unified Reporting stage open/summary parity with IDE Overview.
- `report_smart_tags` / `shaft report tags` — New / Always-failing / Flaky / Regressed / Fixed (#5975).
- `report_mute` / `shaft report mute|unmute|mutes` — local flake mute with recover lifecycle (#5974).
- Exact tool names for CI shard merge stay on `shaft call report_merge_shards` (no curated alias).
