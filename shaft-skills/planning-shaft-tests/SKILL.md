---
name: planning-shaft-tests
description: Use when planning SHAFT test coverage by exploring an app, generating test flow specifications, and preparing automation scenarios.
---

# Planning SHAFT Tests

## Overview

Explore a target application to discover testable flows, generate annotated test plans, and prepare automation scenarios before recording or writing test code. Treat generated plans as specifications for human review and codegen input.

## Exploration and Planning Flow

1. Initialize a browser session with `shaft-mcp:driver_initialize`.
2. Call `shaft-mcp:test_plan_explore` with the target URL, business goal, and exploration depth/page limits to breadth-first crawl the app and generate one Markdown test plan per discovered flow into `specs/`.
3. Review the generated plans in `specs/` and edit them for clarity, scope, and priority (select which flows to automate, refine steps, add seed data, verify candidate locators).
4. For UI flows: call `shaft-mcp:capture_start_codegen` to record and generate SHAFT test code, or call `shaft-mcp:shaft_coding_partner_plan` with the repository path and a plan to find insertion targets.
5. For API flows: call `shaft-mcp:capture_api_start` followed by `shaft-mcp:capture_api_generate` to record and generate SHAFT API test code.
6. When planning codegen insertion into an existing project, use `shaft-mcp:shaft_coding_partner_diff` to preview the integration before applying in IntelliJ.
7. Run `shaft-mcp:verify_run_focused` to execute the newly generated or edited test scoped and headless.
8. Call `shaft-mcp:shaft_guide_search` whenever SHAFT API or assertion questions arise during planning or code review.

## Integration Rules

- Never exceed `maxDepth: 3` or `maxPages: 50` when calling `test_plan_explore`; deeper crawls risk timeouts and token limits.
- Always review generated plans in `specs/` with the user before proceeding to codegen or recording.
- Treat `specs/` files as specifications and inputs to codegen, never as test code itself.
- Keep test runs scoped and headless unless the user explicitly approves headed/full-suite execution.
- Replace candidate locators in plans with verified Smart Locators or ARIA locators before codegen.
- Use `test_plan_explore` to discover flows; do not manually compose plans when automated discovery is available.
- Redirect codegen requests to `capture_start_codegen` (UI) or `capture_api_start`/`capture_api_generate` (API) after plan approval.

## Tool Catalog

Exact tool names live in `../references/shaft-mcp-tools.md` (read it — it
covers client prefixes and batched schema loading). Prefer `shaft-cli call
<tool>` (`../references/shaft-cli-commands.md`) when installed, else
`shaft-mcp:<tool>`.

## Example calls

`test_plan_explore` — request (requires an active browser session from
`driver_initialize` first; `maxDepth`/`maxPages` must stay within the
Integration Rules bounds):

```json
{
  "targetUrl": "https://demo.example.com",
  "goal": "guest checkout",
  "maxDepth": 2,
  "maxPages": 20
}
```

response (`McpTestPlanExploreResult`, recorded live -- field names/shape
confirmed accurate):

```json
{
  "schemaVersion": "1.0",
  "pagesVisited": 14,
  "plansWritten": [
    "specs/01-guest-checkout-add-to-cart.md",
    "specs/02-guest-checkout-payment.md"
  ],
  "warnings": ["1 page skipped: response status 404."]
}
```

## Official Guide Routes

- Agentic overview: `https://shafthq.github.io/docs/agentic/overview`
- Web testing: `https://shafthq.github.io/docs/testing/web`
- API testing: `https://shafthq.github.io/docs/testing/api`
- Capture: `https://shafthq.github.io/docs/agentic/capture`
- MCP: `https://shafthq.github.io/docs/agentic/mcp`

## Common Mistakes

| Mistake | Fix |
| --- | --- |
| Skipping plan review before codegen | Always review and edit `specs/` files with the user first |
| Exploring beyond maxDepth 3 | Trim discovery scope; rerun with smaller bounds if needed |
| Treating specs/ as test code | Specs are input to codegen; do not run them as tests |
| Missing candidate locators in plans | Use page inspection or Playwright sidecar to populate Smart Locator hints |
| Forgetting to verify insertion before apply | Preview with `shaft_coding_partner_diff` and apply only under user approval |
| Running full test suite on new tests | Use `verify_run_focused` with headless scope instead |
