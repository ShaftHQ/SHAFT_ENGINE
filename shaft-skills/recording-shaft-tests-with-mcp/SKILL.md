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

## Playwright And Mobile

| Target | Tools |
| --- | --- |
| SHAFT Playwright direct record | `shaft-mcp:playwright_initialize`, `shaft-mcp:playwright_record_start`, `shaft-mcp:playwright_record_stop`, `shaft-mcp:playwright_recording_code_blocks` |
| Playwright Capture generation | `shaft-mcp:playwright_capture_code_blocks` |
| Mobile MCP recording | `shaft-mcp:mobile_record_start`, `shaft-mcp:mobile_record_stop`, `shaft-mcp:mobile_recording_code_blocks` |
| Appium Inspector wrapper | `shaft-mcp:mobile_inspector_record_prepare`, `shaft-mcp:mobile_inspector_record_start`, `shaft-mcp:mobile_inspector_record_control`, `shaft-mcp:mobile_inspector_record_stop` |
| Repo-aware insertion plan | `shaft-mcp:shaft_coding_partner_plan`, then target-specific code blocks |

## Integration Rules

- Ask for the exact target URL when the user names a site without one.
- Keep secrets redacted. Use generated required-data placeholders instead of captured typed secrets.
- Prefer recorded assertion mode or checkpoints so replay has meaningful verification.
- Move stable locators/actions into page objects; do not paste a generic generated class when the repo has existing structure.
- Treat `shaft_coding_partner_plan.reuseMatches` as the insertion shortlist and `missingCodeItems` as the only code that still needs to be created.
- Keep recording artifacts as evidence, not as source.
- Do not add sleeps, absolute XPath, raw Selenium calls, or coordinate-only actions when locator candidates exist.

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
