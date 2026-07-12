---
name: verifying-and-applying-shaft-changes
description: Use when reviewing, previewing, applying, guardrail-checking, or verifying generated SHAFT Java before or after inserting it into a repository, especially the coding-partner diff/apply/verify loop from IntelliJ.
---

# Verifying And Applying SHAFT Changes

## Overview

Generated SHAFT code is a draft until it is reviewed, previewed as a diff,
applied under explicit approval, guardrail-checked, and verified. Never write
files from MCP; MCP produces previews and evidence, and the IntelliJ user (or
calling agent) applies edits and runs verification.

## Apply And Verify Loop

1. Get a reuse plan first: `shaft-mcp:shaft_coding_partner_plan` returns the
   recommended target source path, insertion anchor, reuse matches, and the
   smallest useful verification command.
2. Turn reviewed code blocks into a preview with
   `shaft-mcp:shaft_coding_partner_diff` (repositoryPath, targetSourcePath,
   codeBlocks, insertionAnchor). It returns a unified diff and never writes files.
3. Read the diff. Confirm it inserts into the planned owner/anchor, reuses
   existing locator fields and actions, and adds only missing code.
4. Run `shaft-mcp:test_code_guardrails_check` on the inserted code. Resolve every
   ERROR (sleeps, absolute XPath, `SHAFT.GUI.Locator.xpath`, raw Selenium,
   hard-coded secrets) before applying.
5. Apply only under explicit user approval, inside IntelliJ (never via MCP file
   writes). In the plugin this is the plan card's Preview patch -> Apply action.
6. Verify with `shaft-mcp:verify_run_focused` using the plan's
   `verificationCommand` (for example `mvn -q test-compile`, then the smallest
   affected test). It runs headlessly with an allowlisted Maven goal and returns
   a bounded pass/fail summary.
7. On failure, route the evidence back through `analyzing-shaft-failures` and the
   coding-partner plan before changing shared page/test code.

## Guardrail Gate

- Applied Java must be SHAFT syntax only: `SHAFT.GUI.WebDriver`,
  `driver.browser()`, `driver.element()`, `SHAFT.GUI.Locator`, SHAFT assertions.
- Reject and regenerate if the diff contains raw Selenium (`driver.findElement`,
  `ChromeDriver`, `driver.get(...)`), `Thread.sleep`, `@FindBy`/`PageFactory`,
  absolute XPath, or `SHAFT.GUI.Locator.xpath(...)`.
- Treat a cloud provider `guardrailStatus` other than `PASSED` as a blocker.

## Verification Rules

- Use the smallest non-redundant check first: `test-compile`, then the single
  affected test. Escalate to `package`/`verify` only when needed.
- Verification stays headless and offline unless network validation is approved;
  release and deploy goals are rejected by the runner.
- Do not mark a change done while verification fails or was skipped; report the
  bounded output and the next step.

## Tool Catalog

Every shaft-mcp tool name and description is cached in
`../references/shaft-mcp-tools.md`. Read it to pick exact tool names instead of
listing tools at runtime, and load only the schemas you need — on clients that
defer tool schemas, batch the load in one lookup.

## Official Guide Routes

- MCP: `https://shafthq.github.io/docs/agentic/mcp`
- Capture: `https://shafthq.github.io/docs/agentic/capture`
- Doctor: `https://shafthq.github.io/docs/agentic/doctor`
- Web testing: `https://shafthq.github.io/docs/testing/web`

## Common Mistakes

| Mistake | Fix |
| --- | --- |
| Pasting generated code without a diff | Preview with `shaft_coding_partner_diff` first |
| Applying before guardrails pass | Run `test_code_guardrails_check` and clear ERRORs |
| Writing files from MCP | Apply inside IntelliJ under explicit approval only |
| Reporting done without running the check | Run `verify_run_focused` and read the result |
| Full-suite run to validate one change | Use the plan's focused verification command |
