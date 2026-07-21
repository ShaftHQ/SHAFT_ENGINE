---
name: analyzing-shaft-failures
description: Use when analyzing SHAFT Allure results, Doctor reports, trace evidence, healer output, flaky locator/wait/assertion failures, retries, or test-fix recommendations.
---

# Analyzing SHAFT Failures

## Overview

Analyze populated evidence before changing tests. Separate product defects, test defects, and infrastructure problems; treat Doctor and healer patches as review-only until applied by the calling agent or user.

## Evidence Workflow

1. Count populated Allure `*-result.json` files before trusting status, summaries, or screenshots.
2. Preserve the `allure-results` root; delete contents only when cleanup is explicitly needed.
3. Use `shaft-mcp:doctor_analyze_failed_allure` for WebDriver/Selenium SHAFT failures.
4. Use `shaft-mcp:playwright_doctor_analyze_failed_allure` for SHAFT Playwright failures.
5. Use `shaft-mcp:doctor_suggest_fix` or `shaft-mcp:playwright_doctor_suggest_fix` only after reviewing the Doctor report.
6. Prefer `shaft-mcp:trace_latest`, `shaft-mcp:trace_summarize`, and `shaft-mcp:doctor_analyze_trace` when structured trace evidence exists.
7. Search the guide with `shaft-mcp:shaft_guide_search` before recommending SHAFT syntax changes.
8. Call `shaft-mcp:shaft_coding_partner_plan` with the failed source path, selected failing code, and evidence paths before adding or moving repair code.
9. Run `shaft-mcp:test_code_guardrails_check` on any suggested Java patch.

## Diagnosis Categories

| Symptom | First check |
| --- | --- |
| Locator not found or duplicate | Current DOM/tree, smart locator, app-owned attributes |
| Stale/hidden/covered/interactable | Page state, frame/window context, synchronized SHAFT action |
| Assertion mismatch | Expected behavior, test data, response/body state |
| Timeout/flaky retry | Deterministic wait condition, environment, network trace |
| Empty report | Test did not run or result path is wrong; do not infer pass/fail |
| Product behavior changed | Report suspected product bug, do not silently weaken assertions |

## Healer Rules

- `shaft-mcp:healer_run_failed_test` and `shaft-mcp:playwright_healer_run_failed_test` may rerun and propose fixes, but they do not own source edits.
- Require a headless, bounded Maven command and workspace-local evidence paths.
- Do not run cloud/external suites, publish PRs, or use provider advisories unless explicitly approved.
- Use `shaft_coding_partner_plan.reuseMatches` to keep repairs in the existing page/test owner instead of creating duplicate helper classes.
- Validate applied fixes with `shaft-mcp:verify_run_focused` using the smallest affected test or compile check; see `verifying-and-applying-shaft-changes`.

## Tool Catalog

Exact tool names live in `../references/shaft-mcp-tools.md` (read it — it
covers client prefixes and batched schema loading). Prefer `shaft-cli call
<tool>` (`../references/shaft-cli-commands.md`) when installed, else
`shaft-mcp:<tool>`.

## Example calls

`doctor_analyze_failed_allure` — request (empty `allureResultPaths` analyzes
the newest evidence found in the workspace):

```json
{
  "allureResultPaths": [],
  "historicalBundlePaths": [],
  "outputDirectory": "",
  "includeScreenshots": false,
  "includePageSnapshots": false,
  "minimumAllureResults": 1,
  "repositoryRoot": "",
  "allowedSourcePaths": [],
  "useAi": false,
  "allowLocalAi": false,
  "allowRemoteAi": false,
  "driverVariableName": "driver",
  "backend": ""
}
```

response (`McpAnalysisReport`, recorded live and truncated — note the real
shape: `actions[]` entries carry `title`/`action`/`status`, not `kind`/
`description`, and `codeBlocks[]` are objects with `title`/`language`/`code`/
`copyPasteReady`, not bare strings):

```json
{
  "schemaVersion": "1.0",
  "status": "DETERMINISTIC",
  "bundleId": "bundle-21218384673999cd887b",
  "primaryCause": "LOCATOR",
  "confidence": "HIGH",
  "summary": "Locator did not resolve an element.",
  "actions": [
    {
      "id": "r-locator-not-found",
      "title": "Locator did not resolve an element",
      "category": "LOCATOR",
      "action": "Inspect the cited locator against the failing page state and update it only after confirming the intended element.",
      "codeBlockIds": ["locator-review-locator", "explicit-wait-locator", "fix-prompt-locator"],
      "status": "SUGGESTED"
    }
  ],
  "codeBlocks": [
    {
      "id": "locator-review-locator",
      "title": "LOCATOR (trust 90%): Review and replace the failing locator",
      "kind": "LOCATOR",
      "language": "java",
      "code": "// Replace the placeholder with evidence-backed attributes from the current page.\nprivate static final String TARGET_ELEMENT_TEXT = \"REPLACE_WITH_STABLE_VISIBLE_TEXT\";",
      "copyPasteReady": false
    }
  ],
  "bundlePath": "target/shaft-doctor/doctor-evidence.json",
  "jsonReportPath": "target/shaft-doctor/doctor-report.json",
  "markdownReportPath": "target/shaft-doctor/doctor-report.md",
  "warnings": []
}
```

`doctor_suggest_fix` — request (same report, backend defaults to `web`):

```json
{
  "jsonReportPath": "target/shaft-doctor/doctor-report.json",
  "repositoryRoot": "",
  "allowedSourcePaths": [],
  "useAi": false,
  "allowLocalAi": false,
  "allowRemoteAi": false,
  "driverVariableName": "driver",
  "backend": ""
}
```

Response is the same `McpAnalysisReport` shape shown above, re-rendered from
the same report path (recorded live: byte-for-byte the same analysis) —
review `codeBlocks` before any of it is applied anywhere.

## Official Guide Routes

- Doctor: `https://shafthq.github.io/docs/agentic/doctor`
- MCP: `https://shafthq.github.io/docs/agentic/mcp`
- Flakiness: `https://shafthq.github.io/docs/testing/flakiness`
- Web locator strategy: `https://shafthq.github.io/docs/testing/web#locator-strategy`
- Element validations: `https://shafthq.github.io/docs/reference/actions/GUI/Element_Validations`

## Common Mistakes

| Mistake | Fix |
| --- | --- |
| Trusting empty Allure output | Count result JSON first |
| Fixing only the named test | Check shared page/API helper callers |
| Weakening assertion to pass | Confirm expected behavior or report product bug |
| Applying healer patch blindly | Review evidence and guardrails first |
| Replacing `allure-results` directory | Preserve root and clean contents only |
