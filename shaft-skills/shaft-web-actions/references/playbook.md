# Web-actions playbook

## Ten practices

1. Confirm whether the project uses `SHAFT.GUI.WebDriver` or `SHAFT.GUI.Playwright`; follow its existing backend and never mix backend-specific waits or locators. [`SHAFT-GUIDE`, `SELENIUM-PRACTICES`]
2. Use `driver.browser()` for navigation, windows, cookies, screenshots, and browser state; use `driver.element()` for element interaction and state. [`SHAFT-GUIDE`]
3. Verify or reuse the locator before acting, and express user intent through page/component methods when reuse exists. [`SELENIUM-PRACTICES`, `SHAFT-GUIDE`]
4. Rely on SHAFT synchronized actions and observable state; replace sleeps and timing guesses with the next meaningful assertion. [`SHAFT-GUIDE`, `SELENIUM-PRACTICES`]
5. Navigate only to an exact configured URL, keep credentials outside source, and isolate browser state between tests. [`SELENIUM-PRACTICES`]
6. Use the narrowest action that matches intent: type, click, hover, clear, upload, drag-and-drop, or browser operation; avoid raw JavaScript or raw driver escape hatches. [`SHAFT-GUIDE`]
7. Treat new windows, cookies, storage, network routes, and uploads as owned state with explicit setup and cleanup. [`SELENIUM-PRACTICES`, `ISTQB-CTFL`]
8. Assert the user-visible effect after state-changing actions; an action completing without exception is not the business oracle. [`ISTQB-CTFL`, `SHAFT-REPORTING`]
9. Capture screenshots, DOM, network, or trace evidence only when it improves diagnosis, and redact sensitive values. [`SHAFT-REPORTING`, `ALLURE-RESULTS`]
10. Guardrail-check generated Java and run the smallest headless focused test across the intended browser/backend before completion. [`SHAFT-MCP`, `ISTQB-CTFL`]

## Valid examples

- Navigate with `driver.browser().navigateToURL(baseUrl)` and assert the expected URL or page marker.
- Type into two verified fields with `driver.element().type(email, user).and().type(password, pass).and().click(signIn)` and assert the signed-in state.
- Upload a fixture through the supported element action, assert the displayed filename, and clean the created server-side record.
- Use `browser_route` and `browser_unroute` within one approved MCP session to test a deterministic error response.

## Boundary

- Do not use this skill to invent locators, redesign page objects, or choose business assertions; obtain those specialist outputs first.

Sources: [shared authoritative bibliography](../../shaft-developer/references/sources.md).
