# Web actions

Use when implementing SHAFT WebDriver or Playwright navigation, browser state, element interaction, upload, drag-and-drop, window, cookie, or screenshot behavior, or when composing or repairing SHAFT fluent browser, element, assertion, verification, or report-message chains. Touch chains belong to [mobile actions](mobile-actions.md).

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

Native iOS and Android taps belong to shaft-mobile-actions, not this web playbook. Follow [mobile actions](mobile-actions.md).
An action that returns without an exception is not proof the control fired.

## Valid examples

- Navigate with `driver.browser().navigateToURL(baseUrl)` and assert the expected URL or page marker.
- Type into two verified fields with `driver.element().type(email, user).and().type(password, pass).and().click(signIn)` and assert the signed-in state.
- Upload a fixture through the supported element action, assert the displayed filename, and clean the created server-side record.
- Use `browser_route` and `browser_unroute` within one approved MCP session to test a deterministic error response.

## Boundary

- Do not use this skill to invent locators, redesign page objects, or choose business assertions; obtain those specialist outputs first.

## Fluent chains

## Ten practices

1. Confirm the exact chain in the current guide or repository before editing; fluent return types differ by action family and version. [`SHAFT-GUIDE`]
2. Start from the correct facade and surface: `driver.browser()`, `driver.element()`, `driver.touch()`, `driver.assertThat()`, `driver.verifyThat()`, or `SHAFT.Validations`. [`SHAFT-GUIDE`]
3. Use `.and()` only to move through supported SHAFT fluent contexts; do not invent transitions because they read naturally. [`SHAFT-GUIDE`]
4. Keep a chain in one coherent user action or oracle; split it when intermediate values, branches, recovery, or diagnostics matter. [`ISTQB-CTFL`]
5. Preserve ordered side effects and stop after the first state-changing failure rather than masking it with later steps. [`ISTQB-CTFL`, `SHAFT-REPORTING`]
6. End validation chains with a terminal assertion method; SHAFT evaluates that method immediately, while an unfinished builder is not an assertion. [`SHAFT-GUIDE`]
7. Choose `assertThat()` for a blocking invariant and `verifyThat()` only when collecting further evidence after a noncritical mismatch is intentional. [`SHAFT-GUIDE`, `ISTQB-CTFL`]
8. Attach `withCustomReportMessage(...)` to the smallest meaningful action or validation, using domain intent rather than restating syntax. A non-blank custom report message is part of the thrown `AssertionError` and of the accumulated soft-assert text, together with the expected and actual values. An id passed only there is visible on the throwable. A blank message adds no empty prefix. `forceFail` already uses that string as the actual value, so it is not prefixed again. A passing validation that sets the message does not throw. A raw driver exception still needs the defect id on that throwable, not only in the report string. [`SHAFT-REPORTING`]
9. Do not mix raw Selenium/Appium/REST-assured calls or third-party assertions into a SHAFT chain. [`SHAFT-GUIDE`, `SELENIUM-PRACTICES`]
10. Compile the exact chain and run the nearest test; use `test_code_guardrails_check` before accepting generated code. [`SHAFT-MCP`, `ISTQB-CTFL`]

## Valid examples

- Navigate with `driver.browser().navigateToURL(url)`, then assert visibility with `driver.element().assertThat(title).isVisible()`.
- Chain form actions through `driver.element().type(email, user).and().type(password, pass).and().click(submit)`.
- Replace an unfinished browser assertion builder with `driver.assertThat().browser().url().contains("/dashboard")`.

## Boundary

- Fluent composition does not choose the locator, page-object owner, or business oracle; route locator, page-object, and oracle decisions to [locator design](locator-design.md), [page objects](page-objects.md), and [assertions](assertions.md) before polishing the chain. Touch chains: [mobile actions](mobile-actions.md).
