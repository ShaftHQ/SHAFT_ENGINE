# Mobile-actions playbook

## Ten practices

1. Declare platform, OS version, device/emulator, app/build, orientation, locale, permissions, and native/mobile-web scope before implementation. [`APPIUM`, `ANDROID-TESTING`]
2. Follow the project's SHAFT mobile driver setup; for MCP, initialize `mobile_native` or `mobile_web` with `driver_initialize` before mobile actions or recording. [`SHAFT-GUIDE`, `SHAFT-MCP`]
3. Inspect available contexts with `mobile_get_contexts` and switch explicitly with `mobile_switch_context`; do not apply native locators to a webview or vice versa. [`APPIUM`]
4. Tap with `driver.touch().tap(locator)`. Coordinate taps are forbidden. File silent no-ops upstream. Prefer accessibility IDs and the SHAFT locator builder. [`APPIUM`, `W3C-WCAG22`]
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
- Test another language with `SHAFT.Properties.mobile.set().language("ar").locale("EG")` before the driver starts (`appium:language`, `appium:locale`; also works on BrowserStack). An `adb` app-locale change is lost when Appium resets the app, and raw `System.setProperty` calls trip the test-code guardrails.
- In a native session assert state with `isEnabled()`, `isDisabled()`, `isVisible()` and `isHidden()`; SHAFT reads them from the element, not from web-only DOM attributes. SHAFT never sends browser-only commands (Get Title, Get URL, tag-name iframe probes, DOM scripts, Close Window) to a native session.

## Boundary

- Do not open emulators, Inspector, devices, or visible sessions without approval; route recording to `shaft-test-recording` and locator ranking to `shaft-locator-design`.

## Hittable taps (#6909)

- Tap with `driver.touch().tap(locator)`.
- Coordinate taps are forbidden.
- File silent no-ops upstream.
- Record XCUITest or UiAutomator2 before choosing a gesture.
- A non-hittable iOS element is not a passed tap.
- `driver.element().click(locator)` and `driver.touch().tap(locator)` share one hittable outcome.
- Do not report pass when the page source is unchanged after a non-hittable tap.

## Execution class

- A missing requested accessibility id is not a skip while the tree shows the control. Content description counts. XPath `!=` on an unset attribute drops the node. `not(...)` keeps it, for example `not(@content-desc='marker' or @name='marker')`.
- Do not tap Done or Save when the oracle is an in-memory count. That tap writes device state for later tests in the same install. Tapping it is allowed when save is the scenario.
- Do not work around tracked scroll bug #6940 with a coordinate tap, a null container, or a reversed direction. #6942 closed it; prove a nested mobile list on a device before relying on that fix. Do not claim that it did without that evidence. #6908 stays the example of a successful tap that does nothing.
- Android language proof reads visible `text`, not `content-desc`. `content-desc` may be the automation tag. A locale change is a new process: `SHAFT.Properties.mobile.set().language(...).locale(...)` before the driver starts. Do not cold-boot or wipe between language cohorts when the emulator is healthy. Cold-boot belongs to the pressure-or-wedge cadence when the emulator is wedged. Do not copy an iOS label into an Android accessibility id.
- Keyboard dismissal is not an unguarded system Back. Use `hideKeyboard`. It can no-op. A no-op is not permission to press Back. After the hide, the control that must stay is still visible. A locale-only miss asks for a screenshot before a product-defect conclusion. A wrapping locale can cover a control with the keyboard.
- Optional screens use `isPresent(By, Duration)` or `waitUntilAnyPresent(Duration, By...)`. Do not use `getElementsCount` and do not catch a click. Absence is a branch, not a failure. A once-per-launch prompt is one check per session unless a measurement shows it returns on each test. A mandatory screen stays an assertion. Do not invent a second presence API (#6842).
- A screen that an asynchronous prompt can precede (OS permission, notifications, app rating after sign-in) is never checked once. Poll for the target screen with a deadline, dismiss any known prompt on every iteration, and assert the target only after the loop. Each look stays bounded (`isPresent(By, Duration)`); this rule is about when to look (#7067).
- Isolate tests inside one Appium session: `terminateApp`, `mobile: clearApp`, `activateApp`. Do not restart the session, the Appium server, or the emulator for isolation. iOS real devices: `removeApp`, `installApp`, `activateApp`; `mobile: clearApp` is simulator-only (#7071).
- Device and app control goes through Appium only, never `adb`, so local and cloud (BrowserStack, LambdaTest) runs match. Permissions come from capabilities (`appium:autoGrantPermissions`, `appium:permissions`) or Appium-driven dialogs (#7071).
