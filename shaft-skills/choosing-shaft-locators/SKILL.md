---
name: choosing-shaft-locators
description: Use when creating, reviewing, refactoring, repairing, or generating SHAFT web/mobile locators, smart locators, ARIA locators, XPath/CSS replacements, or codegen element identifiers.
---

# Choosing SHAFT Locators

## Overview

Choose locators that express user intent first and DOM mechanics last. A locator is not ready for generated code until it has been checked against the current page, app tree, or official guide pattern.

## Locator Ladder

Stop at the first rung that uniquely identifies the element:

1. Smart locator: `SHAFT.GUI.Locator.inputField("Email")`, `clickableField("Sign in")`.
2. Semantic/ARIA locator: `SHAFT.GUI.Locator.hasRole(Role.BUTTON).hasText("Submit").build()`.
3. Stable product-owned attribute: `data-testid`, stable `id`, `name`, or mobile `accessibilityId`.
4. Composed `SHAFT.GUI.Locator` builder with tag, text, attributes, parent/shadow/iframe context.
5. Stable CSS only when the app exposes no semantic signal.
6. Native `By.xpath(...)` only when required, never absolute XPath. Do not generate `SHAFT.GUI.Locator.xpath(...)`.

## MCP Checks

- Call `shaft-mcp:shaft_guide_search` for `Smart Locators`, `SHAFT Locator Builder`, or `web locator strategy`.
- For live web work, use `shaft-mcp:browser_open_intent`, `shaft-mcp:browser_get_page_dom`, and screenshots when needed.
- For Playwright projects, use the matching `shaft-mcp:playwright_*` DOM and element tools.
- For mobile, use `shaft-mcp:mobile_get_accessibility_tree` and prefer accessibility IDs before XPath.
- For repo insertion, call `shaft-mcp:shaft_coding_partner_plan` to see existing locator fields and page methods before adding a new locator.
- Run `shaft-mcp:test_code_guardrails_check` on final Java snippets.

## Codegen Rules

- Verify login, form, and navigation locators with real MCP actions before publishing them.
- Keep generated `SHAFT.GUI.Locator.*` locators inline only for throwaway snippets; move stable locators into page objects for repo insertion.
- Reuse locator summaries returned by `shaft_coding_partner_plan` and add only missing fields that the current DOM proves are needed.
- Preserve user-provided locator choices from Capture when the recorder marks them as intentional.
- For complex XPath, first try Smart Locators, ARIA, and the SHAFT locator builder; use a native Selenium `By` object only when those fail.
- For SHAFT Playwright code, use native Playwright locators only as the same last fallback.
- Do not use coordinate-only actions while a locator candidate exists.
- Do not paste raw DOM snapshots into source code.

## Examples

```java
By email = SHAFT.GUI.Locator.inputField("Email");
By submit = SHAFT.GUI.Locator.clickableField("Create Account");
By alert = SHAFT.GUI.Locator.hasRole(Role.ALERT).containsText("error").build();
By checkout = SHAFT.GUI.Locator.hasAnyTagName()
        .hasAttribute("data-testid", "checkout")
        .build();
```

## Official Guide Routes

- Locator strategy: `https://shafthq.github.io/docs/testing/web#locator-strategy`
- Smart Locators: `https://shafthq.github.io/docs/reference/actions/GUI/didYouKnow/Smart_Locators`
- Locator Builder: `https://shafthq.github.io/docs/reference/actions/GUI/didYouKnow/Shaft_Locator_Builder`
- Element identification: `https://shafthq.github.io/docs/reference/actions/GUI/Element_Identification`
- Mobile testing: `https://shafthq.github.io/docs/testing/mobile`

## Common Mistakes

| Mistake | Fix |
| --- | --- |
| `By.xpath("/html/body/...")` | Use Smart Locator, ARIA, or builder context |
| Generated ID chosen blindly | Prefer visible label or app-owned test attribute |
| Multiple Smart Locator matches | Add parent/container context with `SHAFT.GUI.Locator` |
| Locator repaired from old report only | Inspect current DOM/tree before changing source |
| Selenium `@FindBy` | Use `By` fields and SHAFT page object methods |
