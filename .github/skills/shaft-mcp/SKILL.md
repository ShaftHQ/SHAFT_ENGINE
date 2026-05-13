---
name: shaft-mcp
description: Use SHAFT MCP tools to inspect a SHAFT_ENGINE workspace, run safe validation, summarize Allure/Surefire reports, and prepare maintainer-ready QA findings.
metadata:
  homepage: https://github.com/ShaftHQ/SHAFT_ENGINE/tree/main/tools/shaft-mcp-server
---

# SHAFT MCP Automation Skill

## Purpose

Use this skill when an AI agent has access to the SHAFT MCP server. Prefer it
for QA automation workflows where the agent needs SHAFT project context,
validation evidence, and report summaries.

Use Playwright MCP for direct browser exploration. Use SHAFT MCP for framework
orchestration and report analysis.

## Default Workflow

1. Call `shaft_project_info` first.
   - Confirm the workspace is SHAFT_ENGINE.
   - Check Java/Maven metadata and report artifact inventory.

2. Choose the narrowest safe validation.
   - Use `shaft_run_validation` with `validate` for a cheap setup check.
   - Use `compile` or `test-compile` before source/test changes.
   - Use `specific-test` for focused test verification.
   - Use `unit-tests-regex` only when a broad unit slice is justified.

3. After tests run, call `shaft_extract_allure_failures`.
   - Count total Allure result JSON files before drawing conclusions.
   - If the result count is zero or unexpectedly low, treat the run as invalid.

4. Call `shaft_list_report_artifacts` and `shaft_read_report_artifact` when
   supporting evidence is needed.

5. Prepare findings with:
   - affected files and lines
   - exact commands/tools used
   - expected versus actual behavior
   - impact
   - minimal safe fix direction
   - remaining uncertainty

## Installation Awareness

If the SHAFT MCP server is not available as tools, tell the user to install or
configure it first. The starter server uses stdio transport and lives at:

```text
tools/shaft-mcp-server/src/index.js
```

The MCP client must set:

```text
SHAFT_WORKSPACE=/absolute/path/to/SHAFT_ENGINE
```

Useful config examples are in:

- `tools/shaft-mcp-server/examples/codex-config.example.toml`
- `tools/shaft-mcp-server/examples/claude-desktop-config.example.json`
- `tools/shaft-mcp-server/examples/mcp-client-config.example.json`

## Safety Rules

- Do not ask the MCP server to expose raw secrets.
- Do not paste literal credential values into reports or public issues.
- Do not rely on Maven exit code alone; SHAFT may intentionally continue after
  failures so reporting can finish.
- Treat Allure result JSON as the primary SHAFT test oracle only after verifying
  the result count is populated.
- Cross-check Surefire reports when Allure and Maven disagree.

## Suggested Prompts

### Failure Triage

```text
Use SHAFT MCP to inspect the workspace, run the narrowest relevant validation,
summarize Allure failures, and produce a maintainer-ready issue draft with
commands, file references, expected behavior, actual behavior, impact, and
minimal fix direction.
```

### Release Confidence

```text
Use SHAFT MCP to run validate, compile, test-compile, and the relevant test
selector. Then inspect Allure and Surefire artifacts and report whether the
release evidence is complete, incomplete, or blocked.
```

### Reporting Consistency

```text
Use SHAFT MCP to compare Allure result counts and failure summaries against
Surefire report artifacts. Flag any mismatch where the authoritative report can
hide a failing test.
```
