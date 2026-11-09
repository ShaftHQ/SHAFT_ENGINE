# SHAFT skills pack

**Humans:** this folder is the published pack that teaches an agent to use SHAFT. The generated inventory is [INVENTORY.md](INVENTORY.md). The scorecard is [QUALITY.md](QUALITY.md). Evaluation prompts are [evaluation-prompts.md](evaluation-prompts.md).

**Agents:** for product test work, load `shaft-skills/shaft-developer/SKILL.md` and follow that router. For work on this repository, load `AGENTS.md` at the repository root and follow ChaosEngine. This file is the human map and an index. Do not treat it, `INVENTORY.md`, or `QUALITY.md` as policy. `INVENTORY.md` and `QUALITY.md` are generated; do not edit them by hand.

# SHAFT skills mindmap

Human map of the pack. An agent loads `shaft-developer` and that entrypoint selects exactly one skill. The other skills stay unloaded until that choice.

```text
shaft-developer
├── Lifecycle
│   ├── shaft-requirements-analysis — testable requirements, gaps, traceability
│   ├── shaft-test-planning — strategy, scope, risk, schedule, entry and exit
│   ├── shaft-test-case-design — scenarios, steps, expected results
│   ├── shaft-test-data-design — datasets, factories, privacy, cleanup
│   ├── shaft-test-environment — browser, device, service, configuration readiness
│   ├── shaft-test-execution — run approved tests and keep the evidence
│   ├── shaft-test-monitoring — progress, gates, duration, retries
│   ├── shaft-test-completion — exit criteria, residual risk, archive, handover
│   ├── shaft-failure-analysis — Allure failure analysis of one consistent failure
│   ├── shaft-flaky-test-analysis — inconsistent pass/fail under the same conditions
│   ├── shaft-defect-reporting — a reproducible product defect
│   ├── shaft-execution-reporting — engineering execution report
│   ├── shaft-stakeholder-reporting — business quality or release decision
│   ├── shaft-accessibility-testing — WCAG, keyboard, assistive technology
│   └── shaft-nonfunctional-test-design — performance, security, reliability, recovery
├── Implementation
│   ├── shaft-automated-test-authoring — executable SHAFT test code
│   ├── shaft-page-objects — page and component objects
│   ├── shaft-fluent-api — fluent chains
│   ├── shaft-locator-design — rank and repair locators
│   ├── shaft-web-actions — browser and element actions
│   ├── shaft-mobile-actions — native and touch actions
│   ├── shaft-mobile-env — local JDK, Appium, SDK, emulator
│   ├── shaft-android-apk-select — universal or ABI-split APK
│   ├── shaft-ios-ipa-acceptance — IPA gate before cloud upload
│   ├── shaft-browserstack-native — BrowserStack App Automate
│   ├── shaft-mobile-retest-hygiene — mobile retest map, picker, bytecode, journal
│   ├── shaft-api-testing — HTTP API tests
│   ├── shaft-database-testing — persisted-state checks
│   ├── shaft-assertions — hard assertions and soft verifications
│   ├── shaft-guide-search — current official SHAFT guide
│   ├── shaft-test-recording — Record and playback of a live flow
│   ├── shaft-recording-codegen — Code generation from a persisted recording
│   ├── shaft-change-verification — preview, guardrail, apply, verify
│   └── shaft-docs-hygiene — plan, journey, and issue vocabulary
└── Tool caches (load only when selected)
    ├── shaft-mcp — compact MCP names and arguments; defer JSON schemas
    └── shaft-cli — compact CLI forms and arguments; do not use --help for discovery
```

## How they relate

- `shaft-developer` is the only decision surface. Finish one skill before returning for a different deliverable.
- Allure failure analysis is `shaft-failure-analysis`. The sibling for results that pass and fail under the same conditions is `shaft-flaky-test-analysis`.
- Record and playback is `shaft-test-recording`. Code generation from that persisted recording is `shaft-recording-codegen`. A generated edit then goes to `shaft-change-verification`.
- `shaft-automated-test-authoring` owns test code that is not produced from a recording. It hands locator, page-object, fluent, web, mobile, API, database, and assertion detail to those specialists.
- Local mobile setup starts at `shaft-mobile-env`, then `shaft-android-apk-select` or `shaft-ios-ipa-acceptance`, and uses `shaft-browserstack-native` for a cloud device.
- `shaft-mcp` and `shaft-cli` are the two tool caches. Load one of them only when the entrypoint selects that tool path. Neither cache is part of the ChaosEngine entry.

## Next

- Repository orientation is `README.md` at the repository root.
- [Skill inventory](INVENTORY.md)
