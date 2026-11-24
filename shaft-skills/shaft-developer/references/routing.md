# SHAFT skill routing

Choose one row from the user's immediate requested output. Load no adjacent
skill preemptively. For a multi-stage request, finish and verify one output,
then return here and choose the next row. If no row fits, the result is no skill. Do not load the nearest skill.

| Request with no row | Result |
| --- | --- |
| Desktop window, desktop application, or desktop automation | no skill. Do not load `shaft-web-actions`. |
| CLI of a product under test | no skill. `shaft-cli` is the SHAFT CLI, not that product. |

## Product stages (Design, Automation, Reporting)

SHAFT's IntelliJ, CLI, MCP, and agentic installer share three product stages
(#5943). Design uses MCP `design_*` / CLI `shaft-cli design`. Automation live record
is a client of MCP `capture_*` plus CLI `shaft-cli capture` / offline `shaft-cli codegen`
— never invent a plugin-only recording protocol. Reporting uses MCP `report_*` /
CLI `shaft-cli report` plus `shaft-cli doctor`. Healer and Doctor flake analytics stay
in Reporting (Stage 3); do not fold heal into record or codegen. ChaosEngine's
installer is a separate product and must not publish `shaft-mcp` into CE
`.mcp.json`.

## Lifecycle specialists

| Immediate output | Load exactly this reference |
| --- | --- |
| Testable requirements, acceptance criteria, gaps, or traceability | [`shaft-requirements-analysis`](specialists/requirements-analysis.md) |
| Test strategy, scope, risk, estimates, schedule, entry, or exit criteria | [`shaft-test-planning`](specialists/test-planning.md) |
| Manual or automatable test cases, scenarios, steps, or expected results | [`shaft-test-case-design`](specialists/test-case-design.md) |
| Test datasets, factories, privacy, reset, or cleanup rules | [`shaft-test-data-design`](specialists/test-data-design.md) |
| Browser, device, service, dependency, configuration, or environment readiness | [`shaft-test-environment`](specialists/test-environment.md) |
| Run approved tests and capture trustworthy execution evidence | [`shaft-test-execution`](specialists/test-execution.md) |
| Monitor progress, gates, coverage, duration, retries, or execution health | [`shaft-test-monitoring`](specialists/test-monitoring.md) |
| Close a cycle: exit criteria, residual risk, archive, testware retirement, handover, or lessons | [`shaft-test-completion`](specialists/test-completion.md) |
| Allure failure analysis of one consistent failed or broken result and its root cause | [`shaft-failure-analysis`](specialists/failure-analysis.md) |
| Diagnose inconsistent pass/fail outcomes under equivalent conditions | [`shaft-flaky-test-analysis`](specialists/flaky-test-analysis.md) |
| File or improve a reproducible product defect report | [`shaft-defect-reporting`](specialists/defect-reporting.md) |
| Produce an engineering-facing execution or failure report | [`shaft-execution-reporting`](specialists/execution-reporting.md) |
| Produce a business-facing quality, risk, or release decision report | [`shaft-stakeholder-reporting`](specialists/stakeholder-reporting.md) |
| Design or assess WCAG, keyboard, assistive-technology, or mobile accessibility tests | [`shaft-accessibility-testing`](specialists/accessibility-testing.md) |
| Design performance, security, reliability, compatibility, recovery, or other quality tests | [`shaft-nonfunctional-test-design`](specialists/nonfunctional-test-design.md) |

## Implementation specialists

These skills own implementation detail; lifecycle skills must hand off instead
of duplicating their guidance.

| Immediate output | Load exactly this reference |
| --- | --- |
| Implement or repair automated SHAFT test code | [`shaft-automated-test-authoring`](specialists/automated-test-authoring.md) |
| Design or refactor page/component objects | [`shaft-page-objects`](specialists/page-objects.md) |
| Compose or correct SHAFT fluent chains | [`shaft-fluent-api`](specialists/web-actions.md#fluent-chains) |
| Identify, rank, verify, or repair element locators | [`shaft-locator-design`](specialists/locator-design.md) |
| Use SHAFT browser or element actions | [`shaft-web-actions`](specialists/web-actions.md) |
| Use SHAFT mobile or touch actions | [`shaft-mobile-actions`](specialists/mobile-actions.md) |
| Set up or repair local mobile JDK/Appium/SDK/emulator | [`shaft-mobile-env`](specialists/mobile-runtime.md#local-environment) |
| Configure BrowserStack App Automate native runs | [`shaft-browserstack-native`](specialists/mobile-runtime.md#browserstack-app-automate) |
| Gate an iOS IPA before cloud upload | [`shaft-ios-ipa-acceptance`](specialists/mobile-runtime.md#ios-ipa-acceptance) |
| Choose universal vs ABI-split Android APK | [`shaft-android-apk-select`](specialists/mobile-runtime.md#android-apk-selection) |
| Keep plan/journey/issue docs vocabulary and hours consistent | [`shaft-docs-hygiene`](specialists/shaft-docs-hygiene.md) |
| Triage mobile retest map/picker/bytecode/journal failures | [`shaft-mobile-retest-hygiene`](specialists/mobile-retest-hygiene.md) |
| Implement SHAFT API tests | [`shaft-api-testing`](specialists/api-testing.md) |
| Implement SHAFT database tests | [`shaft-database-testing`](specialists/database-testing.md) |
| Implement element, browser, API, or other SHAFT assertions | [`shaft-assertions`](specialists/assertions.md) |
| Search and ground syntax in the current official SHAFT guide | [`shaft-guide-search`](specialists/cli.md#guide-search) ([command forms](shaft-cli-commands.md)) |
| Record and playback of a live web or mobile flow | [`shaft-test-recording`](specialists/test-recording.md) |
| Code generation from a persisted recording | [`shaft-recording-codegen`](specialists/recording-codegen.md) |
| Preview, guardrail-check, apply, or verify a generated change | [`shaft-change-verification`](specialists/change-verification.md) |

## Tool specialists

| Immediate output | Load exactly this reference |
| --- | --- |
| Select or call an interactive, stateful, exploratory SHAFT MCP capability | [`shaft-mcp`](specialists/mcp.md) |
| Select or run a deterministic one-shot SHAFT CLI command | [`shaft-cli`](specialists/cli.md) |

When the requested output is generated test code from a natural-language web,
mobile, or API scenario and no persisted recording exists, load
[`shaft-test-recording`](specialists/test-recording.md) first. After the recording is persisted and replay-proven,
return here and load [`shaft-recording-codegen`](specialists/recording-codegen.md). Do not send that request to
`shaft-automated-test-authoring`.

Do not infer exact MCP names, parameters, or CLI flags from this map. Resolve
them from [shaft-mcp-tools.md](shaft-mcp-tools.md), [mcp-cache.md](mcp-cache.md),
[cli-cache.md](cli-cache.md), and [shaft-cli-commands.md](shaft-cli-commands.md).
`shaft` is an accepted alias of the `shaft-cli` launcher.


## Design stage tools (#5943)

Curated MCP `design_*` tools share CLI `shaft-cli design` aliases (same dispatcher as the plugin):

- `design_ingest` / `shaft-cli design ingest` — story and AC ingest
- `design_analyze` / `shaft-cli design analyze` — testability analysis
- `design_gherkin_draft` / `shaft-cli design gherkin` — declarative Gherkin draft
- `design_examples` / `shaft-cli design examples` — Scenario Outline examples
- `design_lexicon` / `shaft-cli design lexicon` — ubiquitous-language catalog
- `design_coverage` / `shaft-cli design coverage` — requirement-to-scenario coverage
- `design_lint` / `shaft-cli design lint` — Gherkin quality lint
- `design_gap_map` / `shaft-cli design gap-map` — fluent API gap map
- `design_readiness` / `shaft-cli design readiness` — Ready / Needs questions gate
- `design_handoff` / `shaft-cli design handoff` — approved pack into Automation

Missing MCP for live design analyze fails with the same recovery as other CLI commands. Gherkin is a review artifact; Java remains SoT.

## Reporting history (S3-01)

Use MCP `report_history` / CLI `shaft-cli report history` for cross-run Allure history in the IDE Reporting canvas. Never confuse with HealingHistoryStore (#3315). Honor `allure.accumulateHistory`; never replace the allure-results root.

Use MCP `report_flake` / CLI `shaft-cli report flake` for dual flake definitions (retry-hidden vs transitions). Never collapse into one score; unknown history is explicit; CI commit metadata is optional (#5968).

Use MCP `report_clusters` / CLI `shaft-cli report clusters` for unique-error clustering by Doctor historical-signature keys (error → impacted tests). No cloud ML; empty results are empty-state (#5969).
Use MCP `report_heal` / CLI `shaft-cli report heal` for HealingDecision status counts with persist-on-pass review gate (RECOVERED+pass → reviewable diff; AMBIGUOUS no apply-to-source; NO_CANDIDATES shown; never auto-land). Reuse `healer_run_failed_test` (#5972).
Use MCP `doctor_cause_label` / CLI `shaft-cli doctor cause-label` to confirm or suggest persisted Doctor defect labels (PRODUCT/TEST/ENVIRONMENT/LOCATOR/TIMING) keyed by signature. Local gitignored store; no cloud ML (#5971).


## Reporting canvas (S3-10)

- `report_open` / `report_summary` — unified Reporting stage open/summary parity with IDE Overview.
- `report_smart_tags` / `shaft-cli report tags` — New / Always-failing / Flaky / Regressed / Fixed (#5975).
- `report_mute` / `shaft-cli report mute|unmute|mutes` — local flake mute with recover lifecycle (#5974).
- Exact tool names for CI shard merge stay on `shaft-cli call report_merge_shards` (no curated alias).

## Token use and completeness (#6909)

These efficiency rules are embedded directly in this router and its linked specialist references. A consumer install does not open a separate efficiency-contract file.

- Load one specialist SKILL.md and its playbook; do not open a second skill in the same turn.
- Do not paste MCP JSON schemas into the prompt; call the tool by its exact name.
- Return a bounded result summary, not raw page source, accessibility XML, or screenshot bytes.
- Do not load ce-pack mastery chapters unless the selected playbook names that chapter.
- Keep the skill description inside the context budget; do not duplicate a playbook inside SKILL.md.
- Stop reading a playbook once the matching practice is found.
- A completed mobile or web action names the SHAFT API, the evidence that proves it, and the failure to report.
- Assertion text names the element and the expected state.
- Do not load every shaft-skills playbook for completeness.
- Never copy session auth tokens from a bug report into logs, commits, or skills.
- When the first unique locator tier matches, stop; do not add a coordinate fallback.
