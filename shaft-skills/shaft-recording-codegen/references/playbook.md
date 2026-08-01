# Recording-codegen playbook

## Ten practices

1. Require a persisted recording path, identified backend, intended repository target, and reviewed recording readiness before generation. [`SHAFT-MCP`, `ISTQB-CTFL`]
2. Prefer `shaft-cli codegen --session <path>` for deterministic scriptable generation; use direct MCP when structured code blocks, compilation, replay, or interactive follow-up is needed. [`SHAFT-MCP`]
3. Use `capture_generate_replay` for generated, compiled, optionally replayed evidence; treat `capture_code_blocks` as a faster unproven draft. [`SHAFT-MCP`]
4. Select `web`, `playwright`, or `mobile` explicitly when active engine state cannot determine the intended backend; use `capture_api_generate` for recorded API traffic. [`SHAFT-MCP`, `APPIUM`]
5. Run `shaft_coding_partner_plan` and `capture_target_candidates` before creating files, reusing existing tests, page objects, components, and locators. [`SHAFT-MCP`, `SELENIUM-PRACTICES`]
6. Use `capture_record_at_target_code_blocks` for focused insertion at an existing source anchor; generate a full class only when no valid owner exists. [`SHAFT-MCP`]
7. Review every generated locator, assertion, required-data placeholder, secret redaction, action order, and cleanup step against the recording and current UI/API. [`SHAFT-REPORTING`, `ISTQB-CTFL`]
8. Keep Playwright evidence as evidence and generate the project's requested SHAFT Java backend; never paste sidecar TypeScript into a Java test suite. [`SHAFT-GUIDE`, `SELENIUM-PRACTICES`]
9. Run `test_code_guardrails_check`, preview with `shaft_coding_partner_diff`, and apply only the missing reviewed code under granted authority. [`SHAFT-MCP`]
10. Compile and run the smallest replay or focused test, preserving the recording, generated blocks, diff, and verdict as traceable evidence. [`SHAFT-MCP`, `ALLURE-RESULTS`]

## Valid examples

- Generate WebDriver Java with `shaft-cli codegen --session recordings/checkout.json --backend webdriver`, then integrate only the missing page method.
- Call `capture_generate_replay` for a persisted mobile recording with backend `mobile` and review compilation/replay evidence.
- Call `capture_record_at_target_code_blocks` against an existing `LoginPage` anchor after the coding-partner plan identifies it.
- Generate SHAFT.API blocks with `capture_api_generate` after reviewing sensitive and volatile response leaves.

## Boundary

- Do not generate from an active but unsaved flow or publish raw output; route live capture to `shaft-test-recording` and the diff/apply/test gate to `shaft-change-verification`.

Sources: [shared authoritative bibliography](../../shaft-developer/references/sources.md).
