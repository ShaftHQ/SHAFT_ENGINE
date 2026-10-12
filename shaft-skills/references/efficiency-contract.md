# Shaft-skills efficiency contract (#6909)

Each row is one shipped enhancement (E) or closed gap (G). The sentence is the
normative text in the mapped skill file.

E01 || shaft-developer/references/routing.md || Load one specialist SKILL.md and its playbook; do not open a second skill in the same turn.
E02 || shaft-developer/references/routing.md || Do not paste MCP JSON schemas into the prompt; call the tool by its exact name.
E03 || shaft-cli/references/playbook.md || Prefer `shaft-cli call` for a stateless tool so the MCP schema is not loaded.
E04 || shaft-cli/references/playbook.md || Ask `shaft-cli tools --cached` before a live tool listing, and do not paste the live listing into the chat.
E05 || shaft-developer/references/routing.md || Return a bounded result summary, not raw page source, accessibility XML, or screenshot bytes.
E06 || shaft-developer/references/routing.md || Do not load ce-pack mastery chapters unless the selected playbook names that chapter.
E07 || shaft-developer/references/routing.md || Keep the skill description inside the context budget; do not duplicate a playbook inside SKILL.md.
E08 || shaft-developer/references/routing.md || Stop reading a playbook once the matching practice is found.
E09 || shaft-developer/references/routing.md || A completed mobile or web action names the SHAFT API, the evidence that proves it, and the failure to report.
E10 || shaft-mobile-actions/references/playbook.md || Tap with `driver.touch().tap(locator)`.
E11 || shaft-mobile-actions/references/playbook.md || File silent no-ops upstream.
E12 || shaft-mobile-actions/references/playbook.md || Coordinate taps are forbidden.
E13 || shaft-mobile-actions/references/playbook.md || Record XCUITest or UiAutomator2 before choosing a gesture.
E14 || shaft-developer/references/routing.md || Assertion text names the element and the expected state.
E15 || shaft-mcp/references/playbook.md || Use locator tap or `element_click` for mobile controls; do not call `mobile_tap_coordinates` to finish a flow.
E16 || shaft-mcp/references/playbook.md || Initialize `mobile_native` before mobile tools on a native session.
E17 || shaft-mcp/references/playbook.md || Read `isError` and warnings before the next MCP call.
E18 || shaft-mcp/references/playbook.md || Keep one MCP client for the whole stateful sequence.
E19 || shaft-mcp/references/playbook.md || Redact tokens from tool output before repeating it.
E20 || shaft-mcp/references/playbook.md || Close with `driver_quit` when the task owns the driver.
G01 || shaft-mobile-actions/references/playbook.md || A non-hittable iOS element is not a passed tap.
G02 || shaft-mobile-actions/references/playbook.md || `driver.element().click(locator)` and `driver.touch().tap(locator)` share one hittable outcome.
G03 || shaft-mobile-actions/references/playbook.md || Do not report pass when the page source is unchanged after a non-hittable tap.
G04 || shaft-cli/references/playbook.md || Generated mobile Java taps with `driver.touch().tap(locator)` and never `tapByCoordinates`.
G05 || shaft-defect-reporting/references/playbook.md || Strip `auth_token` and other query credentials from defect evidence before filing.
G06 || shaft-failure-analysis/references/playbook.md || A later assertion timeout can hide a no-op tap; inspect the tap step before widening the wait.
G07 || shaft-web-actions/references/playbook.md || Native iOS and Android taps belong to shaft-mobile-actions, not this web playbook.
G08 || shaft-recording-codegen/references/playbook.md || Do not generate `tapByCoordinates` or `mobile_tap_coordinates` from a recording.
G09 || shaft-developer/references/routing.md || Do not load every shaft-skills playbook for completeness.
G10 || shaft-cli/references/playbook.md || Prefer the checked-in catalog over dumping `shaft-cli tools` live output into the chat.
G11 || shaft-defect-reporting/references/playbook.md || A coordinate tap that works on a device is diagnosis only, not the fix.
G12 || shaft-locator-design/references/playbook.md || Mobile identity still follows the three locator tiers; coordinates are not a tier.
G13 || shaft-mobile-actions/SKILL.md || On iOS XCUITest, `hittable=false` means the tap did not land.
G14 || shaft-mcp/references/playbook.md || `mobile_tap_coordinates` is not a success path.
G15 || shaft-cli/references/playbook.md || Inside a `shaft-cli session`, mobile taps still use a locator, not viewport coordinates.
G16 || shaft-failure-analysis/references/playbook.md || Do not replace a silent no-op with a coordinate workaround.
G17 || shaft-web-actions/references/playbook.md || An action that returns without an exception is not proof the control fired.
G18 || shaft-recording-codegen/references/playbook.md || Replay-proven code is required before a mobile tap snippet is treated as done.
G19 || shaft-developer/references/routing.md || Never copy session auth tokens from a bug report into logs, commits, or skills.
G20 || shaft-developer/references/routing.md || When the first unique locator tier matches, stop; do not add a coordinate fallback.
