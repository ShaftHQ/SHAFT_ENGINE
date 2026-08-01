# Locator-design playbook

## Ten practices

1. Inspect the current DOM, ARIA snapshot, or mobile accessibility tree before choosing or repairing a locator; old reports are supporting evidence only. [`SHAFT-GUIDE`, `W3C-ACT`, `APPIUM`]
2. Reuse an existing verified locator owned by the current page/component before adding another expression for the same element. [`SELENIUM-PRACTICES`]
3. For generated web code, first use a unique, stable, author-written ID through `SHAFT.GUI.Locator.hasAnyTagName().hasId(...).build()`. [`SHAFT-GUIDE`]
4. If no eligible ID exists, use the SHAFT locator builder with an ARIA role and accessible text/attributes, adding container, frame, or shadow context until unique. [`SHAFT-GUIDE`, `W3C-WCAG22`]
5. Use native relative `By.xpath(...)` only when neither a stable author ID nor usable role exists; never use absolute XPath or `SHAFT.GUI.Locator.xpath(...)`. [`SHAFT-GUIDE`, `SELENIUM-PRACTICES`]
6. Reject framework-generated IDs, positional selectors, layout classes, volatile text, and coordinates as primary identity. [`SELENIUM-PRACTICES`]
7. Keep Smart Locators only for disposable human exploration; generated or repository code must resolve to a deterministic verified locator. [`SHAFT-GUIDE`]
8. For mobile, prefer accessibility ID and platform accessibility semantics; inspect context before choosing native XML or web DOM evidence. [`APPIUM`, `ANDROID-TESTING`]
9. Prove uniqueness and actionability with `browser_open_intent`, `browser_get_page_dom`, `browser_aria_snapshot`, or `mobile_get_accessibility_tree`, then perform the intended action. [`SHAFT-MCP`, `W3C-ACT`]
10. Run `test_code_guardrails_check` and the nearest affected test; a locator is accepted only when current behavior and the resulting source both pass. [`SHAFT-MCP`, `ISTQB-CTFL`]

## Valid examples

- Build `SHAFT.GUI.Locator.hasAnyTagName().hasId("checkout-submit").build()` after proving the author-written ID is unique.
- Build `SHAFT.GUI.Locator.hasRole(Role.BUTTON).hasText("Create Account").build()` after checking the ARIA tree and action.
- Use an Appium accessibility ID for a native `Sign in` control after confirming `NATIVE_APP` context.
- Repair a stale positional XPath by inspecting the live DOM and moving the verified locator into its existing page component.

## Boundary

- This skill identifies and verifies elements; route page ownership to `shaft-page-objects`, action composition to `shaft-web-actions` or `shaft-mobile-actions`, and code insertion to `shaft-change-verification`.

Sources: [shared authoritative bibliography](../../shaft-developer/references/sources.md).
