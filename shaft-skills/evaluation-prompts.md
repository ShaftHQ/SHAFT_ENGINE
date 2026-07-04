# SHAFT Skill Evaluation Prompts

Use these prompts through IntelliJ Assistant `/partner` or Guided `Plan coding partner` first. Fresh agent sessions are fallback coverage when subagent or model-evaluation access is approved. They are not a substitute for the repo validation checks; they pressure-test whether agents apply the skills under realistic drift.

## `writing-shaft-tests`

1. Use `$writing-shaft-tests` to write a TestNG SHAFT web login test for an existing project. The page has Email and Password fields and a Sign in button. Include one hard assertion and no sleeps.
2. Use `$writing-shaft-tests` to review this Java snippet and list only the changes required before it can be accepted: it uses `Thread.sleep`, `driver.findElement`, `System.getProperty`, and a hard-coded token.
3. Use `$writing-shaft-tests` to sketch a SHAFT API negative test from an OpenAPI contract where a missing required field should return 400. Keep reusable request/validation code outside the test.
4. Use `$writing-shaft-tests` for a repo that already has `LoginPage.loginAs(...)` and locator fields. Show that you call `shaft_coding_partner_plan` and extend the existing target instead of creating `GeneratedLoginTest`.

## `choosing-shaft-locators`

1. Use `$choosing-shaft-locators` to replace `By.xpath("/html/body/div[2]/form/input[1]")` for an email field whose visible label is Email and whose placeholder is Work email.
2. Use `$choosing-shaft-locators` to choose locators for a checkout button with accessible name Checkout, a generated id, and a stable `data-testid=checkout-submit`.
3. Use `$choosing-shaft-locators` to diagnose a flaky mobile locator where XPath finds two Login buttons but one has accessibility id `loginButton`.
4. Use `$choosing-shaft-locators` when `shaft_coding_partner_plan` reports an existing `emailInput` locator and one missing confirmation-banner locator.

## `recording-shaft-tests-with-mcp`

1. Use `$recording-shaft-tests-with-mcp` to describe the MCP tool sequence for recording a WebDriver checkout flow and integrating it into existing page objects.
2. Use `$recording-shaft-tests-with-mcp` to handle a Capture result with readiness `RISKY`, one positional CSS warning, and no post-submit assertion.
3. Use `$recording-shaft-tests-with-mcp` to turn an Appium Inspector recording into reusable SHAFT mobile test code without leaking typed passwords.
4. Use `$recording-shaft-tests-with-mcp` after a successful recording when the repo already has `CheckoutPage`. Include `shaft_coding_partner_plan`, target-specific code blocks, guardrails, and focused verification.

## `analyzing-shaft-failures`

1. Use `$analyzing-shaft-failures` to analyze an `allure-results` directory that contains zero `*-result.json` files but a stale `summary.json` says passed.
2. Use `$analyzing-shaft-failures` to triage a failed SHAFT web test with `NoSuchElementException` after a retry and a Doctor suggestion to use a new locator.
3. Use `$analyzing-shaft-failures` to decide whether to apply a healer-proposed assertion change when the trace shows the product now returns a different validation message.
4. Use `$analyzing-shaft-failures` to route a failed login repair through `shaft_coding_partner_plan` so the fix updates the existing page object and not a new helper.
