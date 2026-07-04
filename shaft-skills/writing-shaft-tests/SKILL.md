---
name: writing-shaft-tests
description: Use when writing, reviewing, or repairing SHAFT Java tests, page objects, API tests, mobile tests, CLI/DB tests, assertions, waits, or TestNG/JUnit/Cucumber scenarios.
---

# Writing SHAFT Tests

## Overview

Write SHAFT tests from official guide evidence and current repo patterns. Prefer SHAFT facades, fluent actions, SHAFT assertions, and the smallest maintainable test shape that proves the behavior.

## Required Workflow

1. Inspect the project first: runner, existing page/API objects, package layout, properties, and test style.
2. Call `shaft-mcp:shaft_guide_search` before writing SHAFT code. Cite the returned guide URLs in the final answer.
3. For repo-aware GUI or codegen work, call `shaft-mcp:shaft_coding_partner_plan` with the repository path, user intent, current source path, selected text, and evidence paths before creating new classes.
4. For broad or unclear automation requests, call `shaft-mcp:test_automation_scenarios` with the closest area (`web`, `api`, `mobile`, `cli`, `db`, `capture`, `doctor`, or `ci`).
5. Reuse the plan's existing page objects, tests, locators, and actions before adding missing code.
6. Write code using SHAFT syntax only; do not invent APIs from memory.
7. Run `shaft-mcp:test_code_guardrails_check` on generated Java before finalizing.
8. Validate with the smallest affected test or compile check available.

## SHAFT Defaults

| Need | Use |
| --- | --- |
| Web UI test | `SHAFT.GUI.WebDriver`, `driver.browser()`, `driver.element()`, page objects |
| Playwright project | `SHAFT.GUI.Playwright`, not mixed WebDriver waits |
| Mobile native/web | `SHAFT.GUI.Locator.*`, touch actions, `driver.element().assertThat(...)` |
| API | `SHAFT.API`, reusable request builders/validators, `perform()` |
| Assertions | `driver.assertThat(...)`, `driver.verifyThat(...)`, or `SHAFT.Validations` |
| Guide evidence | `shaft-mcp:shaft_guide_search` |
| Repo reuse plan | `shaft-mcp:shaft_coding_partner_plan` |
| Guardrails | `shaft-mcp:test_code_guardrails_check` |

## Hard Rules

- Do not use `Thread.sleep`; use SHAFT waits, actions, assertions, or condition-based waiting from the guide.
- Do not use raw `driver.findElement`, Selenium `PageFactory`, `@FindBy`, implicit waits, headed setup, or TestNG/JUnit assertions in generated tests.
- Generate GUI assertions and checkpoint follow-ups through SHAFT assertion builders such as `driver.element().assertThat(...)`, `driver.browser().assertThat()`, or `driver.verifyThat(...)`.
- Do not hard-code secrets, credentials, tokens, target URLs, or environment-specific paths.
- Do not infer a target URL from a site/product name. Ask for the exact URL when it is missing.
- Keep browser sessions fresh per test and always quit/clean up following the repo's test framework style.
- Keep reusable page/API/mobile objects outside test classes only when they are actually reused.
- Do not create duplicate page objects, generic generated test classes, or locator fields when the coding partner plan found a reusable target.

## Example Shape

```java
private final By email = SHAFT.GUI.Locator.inputField("Email");
private final By password = SHAFT.GUI.Locator.inputField("Password");
private final By signIn = SHAFT.GUI.Locator.clickableField("Sign in");

@Test
public void userCanSignIn() {
    driver.browser().navigateToURL(baseUrl);

    driver.element()
            .type(email, username)
            .and().type(password, passwordValue)
            .and().click(signIn);

    driver.assertThat().browser().url().contains("/dashboard");
}
```

## Official Guide Routes

Use `shaft-mcp:shaft_guide_search` first, then fall back to these public pages when MCP is unavailable:

- Web testing: `https://shafthq.github.io/docs/testing/web`
- API testing: `https://shafthq.github.io/docs/testing/api`
- Mobile testing: `https://shafthq.github.io/docs/testing/mobile`
- Element validations: `https://shafthq.github.io/docs/reference/actions/GUI/Element_Validations`
- Validation overview: `https://shafthq.github.io/docs/reference/actions/Validations/Overview`
- MCP: `https://shafthq.github.io/docs/agentic/mcp`

## Common Mistakes

| Mistake | Fix |
| --- | --- |
| Native Selenium snippet | Rewrite through SHAFT driver/page/API facades |
| `Thread.sleep` after navigation | Assert URL/page state or use SHAFT synchronized action |
| Assertions only in comments | Add real `assertThat`/`verifyThat` calls |
| Huge generated test class | Move only reused flows into page/API objects |
| Duplicate page object from codegen | Run `shaft_coding_partner_plan`, then insert reviewed methods into the existing target |
| Guardrail check skipped | Run `shaft-mcp:test_code_guardrails_check` before final answer |
