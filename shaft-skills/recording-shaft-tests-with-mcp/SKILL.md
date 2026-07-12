---
name: recording-shaft-tests-with-mcp
description: Use when recording browser, Playwright, mobile, Appium Inspector, or user-performed flows into SHAFT tests with MCP Capture, replay, code blocks, or codegen insertion.
---

# Recording SHAFT Tests With MCP

## Overview

Record first, then generate reviewed SHAFT code. Treat recordings as evidence and snippets as drafts to integrate into the existing page/test structure.

## WebDriver Capture Flow

1. Call `shaft-mcp:shaft_guide_search` for Capture and the target test area.
2. Start with `shaft-mcp:capture_start` or `shaft-mcp:capture_start_codegen`.
3. Use the visible browser or MCP element tools to perform the flow.
4. Add checkpoints with `shaft-mcp:capture_checkpoint` for assertions, page transitions, or named flow blocks.
5. Check readiness with `shaft-mcp:capture_status`; resolve `BLOCKED` or important `RISKY` findings.
6. Stop with `shaft-mcp:capture_stop`.
7. Call `shaft-mcp:shaft_coding_partner_plan` with the repository path, intent, current source path, selected text, and recording/report artifacts to find existing targets and missing code.
8. Generate snippets with `shaft-mcp:capture_code_blocks`; use `shaft-mcp:capture_record_at_target_code_blocks` for insertion into an existing Java source anchor from the plan.
9. Run `shaft-mcp:test_code_guardrails_check` before inserting or returning code.
10. Preview insertion with `shaft-mcp:shaft_coding_partner_diff`, apply under explicit
    approval in IntelliJ, then verify with `shaft-mcp:verify_run_focused`. See
    `verifying-and-applying-shaft-changes`.

## Playwright And Mobile

| Target | Tools |
| --- | --- |
| SHAFT Playwright direct record | `shaft-mcp:playwright_initialize`, `shaft-mcp:playwright_record_start`, `shaft-mcp:playwright_record_status`, `shaft-mcp:playwright_record_stop`, `shaft-mcp:playwright_recording_code_blocks`, `shaft-mcp:playwright_replay_recording` |
| Playwright Capture generation | `shaft-mcp:playwright_capture_generate_replay`, `shaft-mcp:playwright_capture_code_blocks` |
| Mobile MCP recording | `shaft-mcp:mobile_record_start`, `shaft-mcp:mobile_record_stop`, `shaft-mcp:mobile_recording_code_blocks` |
| Appium Inspector wrapper | `shaft-mcp:mobile_inspector_record_prepare`, `shaft-mcp:mobile_inspector_record_start`, `shaft-mcp:mobile_inspector_record_control`, `shaft-mcp:mobile_inspector_record_stop` |
| Repo-aware insertion plan | `shaft-mcp:shaft_coding_partner_plan`, then target-specific code blocks |

## Official Playwright Sidecar

Use official Playwright CLI or Playwright MCP as a delegated browser-exploration sidecar when the user asks for it, when installed Playwright skills are already active, or when the task needs token-efficient snapshots, network/storage inspection, console output, tracing, video, PDF, or Playwright Test Agent planning.

Bring only evidence back into SHAFT: command transcript, page snapshot, locator notes, screenshots, HAR/storage paths, traces, and the exact user steps. Then call `shaft-mcp:shaft_coding_partner_plan`, `shaft-mcp:capture_start_codegen`, `shaft-mcp:capture_codegen_features`, and WebDriver or Playwright code-block tools according to the target Java backend.

## Integration Rules

- Ask for the exact target URL when the user names a site without one.
- Keep secrets redacted. Use generated required-data placeholders instead of captured typed secrets.
- Prefer recorded assertion mode for generated assertions; checkpoints should name review intent, and generated verification code must use SHAFT assertion builders only.
- Move stable locators/actions into page objects; do not paste a generic generated class when the repo has existing structure.
- Treat `shaft_coding_partner_plan.reuseMatches` as the insertion shortlist and `missingCodeItems` as the only code that still needs to be created.
- Record the complete user flow before codegen when requested actions or locators are missing, then insert only the missing locators/actions into the existing source anchor.
- Do not generate `SHAFT.GUI.Locator.xpath(...)`; use Smart Locators, ARIA locators, the SHAFT locator builder, or native `By.xpath(...)` only as a last fallback.
- Keep recording artifacts as evidence, not as source.
- Do not paste Playwright TypeScript tests into Java projects; translate the proven behavior into SHAFT syntax and the existing Java design pattern.
- Use native Playwright locators only as a last fallback in SHAFT Playwright-specific code.
- Treat Playwright `browser_run_code_unsafe`, `playwright-cli run-code`, and `eval` as trusted-client-only evidence gathering.
- Do not add sleeps, absolute XPath, raw Selenium calls, or coordinate-only actions when locator candidates exist.

## Tool Catalog

Every shaft-mcp tool name and description is cached in
`../references/shaft-mcp-tools.md`. Read it to pick exact tool names instead of
listing tools at runtime, and load only the schemas you need — on clients that
defer tool schemas, batch the load in one lookup. When a `shaft-cli` launcher
is installed, prefer running the same tools as shell commands per
`../references/shaft-cli-commands.md` (`shaft-cli call <tool>`), falling back
to `shaft-mcp:<tool>` MCP calls otherwise.

## Official Guide Routes

- Capture: `https://shafthq.github.io/docs/agentic/capture`
- MCP: `https://shafthq.github.io/docs/agentic/mcp`
- Web testing: `https://shafthq.github.io/docs/testing/web`
- Playwright backend: `https://shafthq.github.io/docs/reference/actions/GUI/Playwright_Backend`
- Mobile testing: `https://shafthq.github.io/docs/testing/mobile`

## Common Mistakes

| Mistake | Fix |
| --- | --- |
| Starting capture after the flow | Restart and record from the first meaningful action |
| Ignoring readiness warnings | Resolve missing assertions, risky locators, or required data first |
| Returning raw generated class | Integrate snippets into current page/test files |
| Creating duplicate page object from a recording | Use `shaft_coding_partner_plan` and `capture_record_at_target_code_blocks` against the existing source |
| Publishing unverified locator | Replay or perform the action before final code |
| Capturing secret values | Replace with safe placeholders or config-backed test data |
