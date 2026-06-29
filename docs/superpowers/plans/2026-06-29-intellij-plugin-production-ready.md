# IntelliJ Plugin Production-Ready Execution Plan

## Objective

Make the SHAFT IntelliJ IDEA plugin a production-ready assistant for test automation work: guided recording, locator discovery, code generation, failure triage, dynamic MCP tooling, accessible UI, and reliable stdio transport.

## Workstreams

1. Preserve user work and develop from an isolated branch/worktree based on `origin/main`.
2. Harden MCP stdio result handling so tool output is preserved for object, array, string, primitive, and null JSON-RPC results.
3. Add dynamic `tools/list` parsing and merge discovered tools into the curated IntelliJ catalog with static fallback.
4. Expand Assistant slash commands for recording, locator inspection, Allure triage, failed-test fixing, and test generation.
5. Add guided workflow panels for recorder, locator, guardrail, Doctor, Trace, Healer, and locator proposal requests.
6. Keep focused workflow tabs stable while allowing the Advanced Tools tab to refresh its MCP catalog dynamically.
7. Polish accessibility metadata and narrow-pane behavior for the expanded tab surface.
8. Add focused regression tests for command routing, tool catalog parsing, guided payload contracts, MCP transport, and screenshot rendering.
9. Generate updated IntelliJ plugin screenshot evidence for the new surfaces.
10. Run targeted plugin tests first, then broader Gradle plugin checks before PR preparation.

## Delegation

Spark agents were split across MCP transport, dynamic tool discovery, assistant commands, and UI accessibility/status work. The controller owns integration review, contract alignment, test coverage, screenshots, and final validation.
