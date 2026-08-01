# Mobile-actions playbook

## Ten practices

1. Declare platform, OS version, device/emulator, app/build, orientation, locale, permissions, and native/mobile-web scope before implementation. [`APPIUM`, `ANDROID-TESTING`]
2. Follow the project's SHAFT mobile driver setup; for MCP, initialize `mobile_native` or `mobile_web` with `driver_initialize` before mobile actions or recording. [`SHAFT-GUIDE`, `SHAFT-MCP`]
3. Inspect available contexts with `mobile_get_contexts` and switch explicitly with `mobile_switch_context`; do not apply native locators to a webview or vice versa. [`APPIUM`]
4. Prefer accessibility IDs, roles, labels, and verified element locators; use `mobile_tap_coordinates` only after locator-based `element_click` cannot work. [`APPIUM`, `W3C-WCAG22`]
5. Use the smallest gesture matching intent through `driver.touch()` or exact MCP tools; bound swipes by element, container, direction, or text where possible. [`SHAFT-GUIDE`, `APPIUM`]
6. Treat rotation, keyboard actions, backgrounding, activation, permissions, and app state as explicit test steps with observable outcomes. [`ANDROID-TESTING`, `ISTQB-CTFL`]
7. Avoid sleeps; wait on visible, enabled, selected, context, or application-state evidence through SHAFT actions and assertions. [`SHAFT-GUIDE`, `APPIUM`]
8. Reset app/account/device state between tests, avoid shared emulator residue, and keep credentials and capabilities outside source. [`ANDROID-TESTING`, `ISTQB-CTFL`]
9. Capture device metadata, accessibility tree, screenshot, context, and logs needed to reproduce failures without exposing private user data. [`SHAFT-REPORTING`, `APPIUM`]
10. Verify the smallest focused test on the declared device class, then expand only to the risk-based compatibility matrix. [`ANDROID-TESTING`, `ISTQB-TM`]

## Valid examples

- Initialize `mobile_native`, inspect the accessibility tree, tap an accessibility-ID login control, and assert the native success view.
- Switch from `NATIVE_APP` to a discovered `WEBVIEW_*` context before using web locators, then switch back for a system dialog.
- Use `driver.touch().swipeElementIntoView(target, TouchActions.SwipeDirection.DOWN).tap(target)` and assert the selected item.
- Rotate to landscape and back only when orientation behavior is the explicit acceptance criterion.

## Boundary

- Do not open emulators, Inspector, devices, or visible sessions without approval; route recording to `shaft-test-recording` and locator ranking to `shaft-locator-design`.

Sources: [shared authoritative bibliography](../../shaft-developer/references/sources.md).
