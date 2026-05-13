# SHAFT MCP Server Community Proposal

## Recommendation

Build SHAFT MCP as a high-level QA automation MCP server, not as a replacement
for Playwright MCP.

- **Playwright MCP** should remain the preferred tool for direct browser
  operation: navigate, click, type, inspect accessibility snapshots, and capture
  screenshots.
- **SHAFT MCP** should focus on SHAFT workflows: run Maven/TestNG suites,
  inspect Allure and SHAFT reports, summarize failures, and guide agents through
  web, API, mobile, database, CLI, and reporting evidence.

In short:

```text
Playwright MCP = browser control
SHAFT MCP      = QA framework orchestration and reporting
```

## MVP

The starter server in `tools/shaft-mcp-server/` exposes:

- `shaft_project_info`
- `shaft_run_validation`
- `shaft_extract_allure_failures`
- `shaft_list_report_artifacts`
- `shaft_read_report_artifact`
- `shaft://guidance/agents`
- `shaft://project/pom`

This is deliberately small. It gives agents enough capability to validate a
SHAFT checkout and reason over results without taking over the whole framework.

## Full Capability Map

| Capability | MVP Tooling | Requirement Before It Is Truly End-to-End |
| --- | --- | --- |
| Workspace onboarding | `shaft_project_info`, `shaft://guidance/agents`, `shaft://project/pom` | Add dependency/workflow inventory and environment diagnostics |
| Java/Maven setup validation | `shaft_run_validation` with `validate`, `compile`, `test-compile` | Add JDK/Maven discovery and maintainer-approved self-provisioning if desired |
| Existing unit tests | `shaft_run_validation` with `specific-test` and `unit-tests-regex` | Add suite discovery and safer profile selection |
| Allure result analysis | `shaft_extract_allure_failures` | Add history/trend comparison and broken-test clustering |
| Surefire/report artifact reading | `shaft_list_report_artifacts`, `shaft_read_report_artifact` | Add `shaft_compare_surefire_allure` for oracle mismatch detection |
| Web browser tests | Existing Maven tests can run if browser/grid is already configured | Add Selenium Grid lifecycle tools and structured WebDriver snapshots |
| API tests | Existing Maven tests can run if the target API is available | Add sanitized request/response summaries and API environment checks |
| Android tests | Existing Maven tests can run if device/emulator/Appium are ready | Add Android device listing, emulator readiness, and Appium lifecycle tools |
| iOS tests | Existing Maven tests can run if macOS/device/Appium are ready | Add iOS device listing and Appium lifecycle tools |
| Database tests | Existing Maven tests can run if DB is available | Add DB readiness checks that never expose credentials |
| CLI tests | Existing Maven tests can run | Add narrow SHAFT CLI helpers; avoid generic shell execution |
| Cloud execution | Existing Maven tests can run if credentials are configured | Add BrowserStack/LambdaTest wrappers with strict redaction |
| Desktop UI | Not a SHAFT MCP responsibility today | Use a dedicated desktop MCP or define an Appium desktop path separately |
| Maintainer reports | Agent can draft reports from tool output | Add optional GitHub issue/advisory helpers only after redaction policy is accepted |

## Why This Shape

Direct browser-control MCP tools need a strong structured snapshot model. SHAFT
currently wraps Selenium/Appium and focuses on fluent test automation and rich
reporting. A browser-control MCP layer would need to design:

- stable element references
- accessibility or DOM snapshots
- session lifecycle management
- screenshot and artifact streaming
- safe cleanup for WebDriver/Appium sessions

Until that exists, duplicating Playwright MCP's browser surface would likely be
less reliable than using Playwright MCP directly.

The SHAFT-specific value is higher at the framework/reporting layer:

- run existing SHAFT tests
- inspect generated Allure result JSON
- detect Maven/Surefire/Allure mismatches
- summarize failed tests for maintainers
- preserve SHAFT's batteries-included workflow

## Security Requirements

SHAFT MCP should never expose secrets through tool output.

Required defaults:

- no generic shell execution tool
- no raw environment dump
- no credential printing
- no unrestricted filesystem read
- output redaction before returning logs/artifacts to the agent
- allow-listed report directories
- explicit workspace root

Security-sensitive future tools, such as GitHub advisory reporting or cloud
provider debugging, should be added only after the project has a central
redaction utility and clear maintainer approval.

## Future Tool Candidates

After the MVP stabilizes, useful additions would be:

| Tool | Purpose |
| --- | --- |
| `shaft_run_test_class` | Dedicated wrapper for one TestNG/JUnit class |
| `shaft_compare_surefire_allure` | Detect report-oracle mismatches |
| `shaft_generate_failure_brief` | Produce a maintainer-ready failure summary |
| `shaft_start_web_session` | Start a SHAFT WebDriver session |
| `shaft_snapshot_web_session` | Return a compact structured page snapshot |
| `shaft_stop_session` | Cleanup WebDriver/Appium sessions |
| `shaft_api_request_from_config` | Run a safe SHAFT API request from sanitized config |

## Maintainer Handoff

The current starter is a reviewable contribution seed:

1. It is standalone under `tools/shaft-mcp-server/`.
2. It does not change SHAFT's published Maven artifact.
3. It uses the official MCP TypeScript SDK.
4. It includes a smoke test that starts the MCP server, lists tools, and calls
   `shaft_project_info`.
5. It can evolve into a separate package or be moved into another repository if
   maintainers prefer to keep MCP tooling outside the engine repository.

## Client Compatibility

The server uses stdio MCP transport. It should work with any MCP client that can
launch a local command and pass environment variables.

Confirmed by smoke test:

```bash
cd tools/shaft-mcp-server
npm ci
npm run smoke
```

Configuration examples are included for:

- Codex: `tools/shaft-mcp-server/examples/codex-config.example.toml`
- Claude Desktop: `tools/shaft-mcp-server/examples/claude-desktop-config.example.json`
- Generic MCP clients: `tools/shaft-mcp-server/examples/mcp-client-config.example.json`
