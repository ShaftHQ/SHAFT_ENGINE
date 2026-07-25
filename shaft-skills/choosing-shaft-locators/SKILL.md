---
name: choosing-shaft-locators
description: Use when creating, reviewing, refactoring, repairing, or generating SHAFT web/mobile locators, smart locators, ARIA locators, XPath/CSS replacements, or codegen element identifiers.
---

# Choosing SHAFT Locators

## Overview

Choose locators that express user intent first and DOM mechanics last. A locator is not ready for generated code until it has been checked against the current page, app tree, or official guide pattern.

## Locator Ladder

Generated/codegen output has exactly two legal rungs. Stop at the first one that uniquely identifies the element:

1. ARIA-role-powered XPath via the SHAFT locator builder: `SHAFT.GUI.Locator.hasRole(Role.BUTTON).hasText("Submit").build()`. Add `hasAttribute(...)`, `containsText(...)`, tag, or parent/shadow/iframe context to the same chain when role + text alone doesn't uniquely match — do not drop out of the builder to reach for a stable attribute or CSS separately.
2. Native `By.xpath(...)` — the only permitted fallback, and only when the target element exposes no usable ARIA role. Never absolute XPath; do not generate `SHAFT.GUI.Locator.xpath(...)`.

**Smart locator is excluded from generated/codegen output.** `SHAFT.GUI.Locator.inputField("Email")` / `clickableField("Sign in")` OR-tries dozens of XPath strategies non-deterministically — you can't tell which one matched, and `test_code_guardrails_check` already flags it (`SMART_LOCATOR`, `WARNING`). Its one legitimate use is a human's own throwaway, DOM-unexplored exploration snippet that is never published or inserted into a repo; move to rung 1 the moment the DOM is inspected.

## MCP Checks

- Call `shaft-mcp:shaft_guide_search` for `SHAFT Locator Builder`, `ARIA roles`, or `web locator strategy` (search `Smart Locators` only for the narrow throwaway-snippet case above).
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
- Build every generated locator through the SHAFT locator builder's ARIA role (`hasRole(...)`); use a native Selenium `By.xpath(...)` object only when the target element exposes no ARIA role.
- For SHAFT Playwright code, use native Playwright locators only as the same last fallback.
- Do not use coordinate-only actions while a locator candidate exists.
- Do not paste raw DOM snapshots into source code.

## Examples

```java
By submit = SHAFT.GUI.Locator.hasRole(Role.BUTTON).hasText("Create Account").build();
By email = SHAFT.GUI.Locator.hasRole(Role.TEXTBOX).hasAttribute("aria-label", "Email").build();
By alert = SHAFT.GUI.Locator.hasAnyTagName().hasAttribute("role", "alert").containsText("error").build();
By checkout = SHAFT.GUI.Locator.hasRole(Role.BUTTON).hasAttribute("data-testid", "checkout").build();
```

## Tool Catalog

Exact tool names live in `../references/shaft-mcp-tools.md` (read it — it
covers client prefixes and batched schema loading). Prefer `shaft-cli call
<tool>` (`../references/shaft-cli-commands.md`) when installed, else
`shaft-mcp:<tool>`.

## Example calls

`browser_open_intent` — request:

```json
{
  "targetUrl": "https://demo.example.com/checkout",
  "userIntent": "click the Checkout button",
  "maxCharacters": 4000,
  "maxElements": 20
}
```

response (truncated bounded-DOM map, real keys from `BrowserService.orientPage`):

```json
{
  "schemaVersion": "1.0",
  "currentUrl": "https://demo.example.com/checkout",
  "title": "Checkout",
  "userIntent": "click the Checkout button",
  "dom": "<button data-testid=\"checkout-submit\">Checkout</button> ...",
  "characterCount": 812,
  "truncated": false,
  "elements": [
    {"tag": "button", "text": "Checkout", "attributes": {"data-testid": "checkout-submit"}}
  ],
  "nextTools": ["capture_pick_locator", "element_click"],
  "warnings": []
}
```

`shaft_guide_search` — request:

```json
{"query": "Smart Locators inputField clickableField", "maxResults": 2}
```

response (`McpGuideSearchResult`, truncated):

```json
{
  "schemaVersion": "1.0",
  "query": "Smart Locators inputField clickableField",
  "sourceIndexUrl": "https://shafthq.github.io/docs/search-index.json",
  "matches": [
    {
      "title": "Smart Locators",
      "section": "inputField / clickableField",
      "url": "https://shafthq.github.io/docs/reference/actions/GUI/didYouKnow/Smart_Locators",
      "score": 0.88,
      "excerpt": "SHAFT.GUI.Locator.inputField(\"Email\") matches by label, placeholder, or name.",
      "codeBlocks": ["By email = SHAFT.GUI.Locator.inputField(\"Email\");"]
    }
  ],
  "guidanceRules": ["Stop at the first locator rung that uniquely matches."],
  "warnings": []
}
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
| `By.xpath("/html/body/...")` | Use the SHAFT locator builder's ARIA role, not an absolute path |
| Generated ID chosen blindly | Prefer ARIA role + visible text over a guessed `id` |
| Smart Locator in generated/repo code | Replace with an ARIA-role builder locator; Smart Locator is throwaway-snippet only |
| Multiple builder-XPath matches | Add parent/container context or another attribute filter to the same builder chain |
| Locator repaired from old report only | Inspect current DOM/tree before changing source |
| Selenium `@FindBy` | Use `By` fields and SHAFT page object methods |
