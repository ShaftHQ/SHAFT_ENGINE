# Shaft-skills efficiency contract (#6909)

Each row is one shipped enhancement (E) or closed gap (G). The sentence is the
normative text in the mapped skill file.

E01 || shaft-developer/references/routing.md || Load one specialist SKILL.md and its playbook; do not open a second skill in the same turn.
E02 || shaft-developer/references/routing.md || Do not paste MCP JSON schemas into the prompt; call the tool by its exact name.
E03 || shaft-developer/references/specialists/cli.md || Prefer `shaft-cli call` for a stateless tool so the MCP schema is not loaded.
E04 || shaft-developer/references/specialists/cli.md || Ask `shaft-cli tools --cached` before a live tool listing, and do not paste the live listing into the chat.
E05 || shaft-developer/references/routing.md || Return a bounded result summary, not raw page source, accessibility XML, or screenshot bytes.
E06 || shaft-developer/references/routing.md || Do not load ce-pack mastery chapters unless the selected playbook names that chapter.
E07 || shaft-developer/references/routing.md || Keep the skill description inside the context budget; do not duplicate a playbook inside SKILL.md.
E08 || shaft-developer/references/routing.md || Stop reading a playbook once the matching practice is found.
E09 || shaft-developer/references/routing.md || A completed mobile or web action names the SHAFT API, the evidence that proves it, and the failure to report.
E10 || shaft-developer/references/specialists/mobile-actions.md || Tap with `driver.touch().tap(locator)`.
E11 || shaft-developer/references/specialists/mobile-actions.md || File silent no-ops upstream.
E12 || shaft-developer/references/specialists/mobile-actions.md || Coordinate taps are forbidden.
E13 || shaft-developer/references/specialists/mobile-actions.md || Record XCUITest or UiAutomator2 before choosing a gesture.
E14 || shaft-developer/references/routing.md || Assertion text names the element and the expected state.
E15 || shaft-developer/references/specialists/mcp.md || Use locator tap or `element_click` for mobile controls; do not call `mobile_tap_coordinates` to finish a flow.
E16 || shaft-developer/references/specialists/mcp.md || Initialize `mobile_native` before mobile tools on a native session.
E17 || shaft-developer/references/specialists/mcp.md || Read `isError` and warnings before the next MCP call.
E18 || shaft-developer/references/specialists/mcp.md || Keep one MCP client for the whole stateful sequence.
E19 || shaft-developer/references/specialists/mcp.md || Redact tokens from tool output before repeating it.
E20 || shaft-developer/references/specialists/mcp.md || Close with `driver_quit` when the task owns the driver.
G01 || shaft-developer/references/specialists/mobile-actions.md || A non-hittable iOS element is not a passed tap.
G02 || shaft-developer/references/specialists/mobile-actions.md || `driver.element().click(locator)` and `driver.touch().tap(locator)` share one hittable outcome.
G03 || shaft-developer/references/specialists/mobile-actions.md || Do not report pass when the page source is unchanged after a non-hittable tap.
G04 || shaft-developer/references/specialists/cli.md || Generated mobile Java taps with `driver.touch().tap(locator)` and never `tapByCoordinates`.
G05 || shaft-developer/references/specialists/defect-reporting.md || Strip `auth_token` and other query credentials from defect evidence before filing.
G06 || shaft-developer/references/specialists/failure-analysis.md || A later assertion timeout can hide a no-op tap; inspect the tap step before widening the wait.
G07 || shaft-developer/references/specialists/web-actions.md || Native iOS and Android taps belong to shaft-mobile-actions, not this web playbook.
G08 || shaft-developer/references/specialists/recording-codegen.md || Do not generate `tapByCoordinates` or `mobile_tap_coordinates` from a recording.
G09 || shaft-developer/references/routing.md || Do not load every shaft-skills playbook for completeness.
G10 || shaft-developer/references/specialists/cli.md || Prefer the checked-in catalog over dumping `shaft-cli tools` live output into the chat.
G11 || shaft-developer/references/specialists/defect-reporting.md || A coordinate tap that works on a device is diagnosis only, not the fix.
G12 || shaft-developer/references/specialists/locator-design.md || Mobile identity still follows the three locator tiers; coordinates are not a tier.
G13 || shaft-developer/references/specialists/mobile-actions.md || On iOS XCUITest, `hittable=false` means the tap did not land.
G14 || shaft-developer/references/specialists/mcp.md || `mobile_tap_coordinates` is not a success path.
G15 || shaft-developer/references/specialists/cli.md || Inside a `shaft-cli session`, mobile taps still use a locator, not viewport coordinates.
G16 || shaft-developer/references/specialists/failure-analysis.md || Do not replace a silent no-op with a coordinate workaround.
G17 || shaft-developer/references/specialists/web-actions.md || An action that returns without an exception is not proof the control fired.
G18 || shaft-developer/references/specialists/recording-codegen.md || Replay-proven code is required before a mobile tap snippet is treated as done.
G19 || shaft-developer/references/routing.md || Never copy session auth tokens from a bug report into logs, commits, or skills.
G20 || shaft-developer/references/routing.md || When the first unique locator tier matches, stop; do not add a coordinate fallback.
