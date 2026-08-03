# Fluent-API playbook

## Ten practices

1. Confirm the exact chain in the current guide or repository before editing; fluent return types differ by action family and version. [`SHAFT-GUIDE`]
2. Start from the correct facade and surface: `driver.browser()`, `driver.element()`, `driver.touch()`, `driver.assertThat()`, `driver.verifyThat()`, or `SHAFT.Validations`. [`SHAFT-GUIDE`]
3. Use `.and()` only to move through supported SHAFT fluent contexts; do not invent transitions because they read naturally. [`SHAFT-GUIDE`]
4. Keep a chain in one coherent user action or oracle; split it when intermediate values, branches, recovery, or diagnostics matter. [`ISTQB-CTFL`]
5. Preserve ordered side effects and stop after the first state-changing failure rather than masking it with later steps. [`ISTQB-CTFL`, `SHAFT-REPORTING`]
6. End validation chains with a terminal assertion method; SHAFT evaluates that method immediately, while an unfinished builder is not an assertion. [`SHAFT-GUIDE`]
7. Choose `assertThat()` for a blocking invariant and `verifyThat()` only when collecting further evidence after a noncritical mismatch is intentional. [`SHAFT-GUIDE`, `ISTQB-CTFL`]
8. Attach `withCustomReportMessage(...)` to the smallest meaningful action or validation, using domain intent rather than restating syntax. [`SHAFT-REPORTING`]
9. Do not mix raw Selenium/Appium/REST-assured calls or third-party assertions into a SHAFT chain. [`SHAFT-GUIDE`, `SELENIUM-PRACTICES`]
10. Compile the exact chain and run the nearest test; use `test_code_guardrails_check` before accepting generated code. [`SHAFT-MCP`, `ISTQB-CTFL`]

## Valid examples

- Navigate with `driver.browser().navigateToURL(url)`, then assert visibility with `driver.element().assertThat(title).isVisible()`.
- Chain form actions through `driver.element().type(email, user).and().type(password, pass).and().click(submit)`.
- Replace an unfinished browser assertion builder with `driver.assertThat().browser().url().contains("/dashboard")`.

## Boundary

- Fluent composition does not choose the locator, page-object owner, or business oracle; route those decisions to their specialists before polishing the chain.

Sources: [shared authoritative bibliography](../../shaft-developer/references/sources.md).
