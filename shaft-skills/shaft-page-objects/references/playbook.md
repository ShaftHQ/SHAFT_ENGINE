# Page-object playbook

## Ten practices

1. Search for the existing page or component owner before creating one; extend the narrowest reusable owner. [`SELENIUM-PRACTICES`]
2. Model user-visible services and components, not every page element or test step. [`SELENIUM-PRACTICES`, `SHAFT-GUIDE`]
3. Keep locators private and stable: unique author-written `hasAnyTagName().hasId(...)` first, `hasRole(...)` second, native relative `By.xpath(...)` only when neither exists. Never the SHAFT Locator xpath factory or raw id/name/cssSelector/className/tagName factories. [`SHAFT-GUIDE`]
4. Inject or receive the project-standard SHAFT driver; do not create hidden sessions inside page methods. [`SELENIUM-PRACTICES`, `SHAFT-GUIDE`]
5. Name methods by intent such as `signInAs` or `addProductToCart`, and keep low-level mechanics behind that contract. [`SELENIUM-PRACTICES`]
6. Return a new page/component or `this` only when navigation or meaningful fluent composition requires it; avoid decorative chaining. [`SELENIUM-PRACTICES`]
7. Keep business assertions in tests; page objects may verify only load/readiness invariants needed to make their service reliable. [`SELENIUM-PRACTICES`, `ISTQB-CTFL`]
8. Separate reusable components when they have independent behavior and multiple consumers; prefer composition over page-object inheritance. [`SELENIUM-PRACTICES`]
9. Keep test data and environment configuration outside page objects, passing only values the user action needs. [`ISTQB-CTFL`]
10. Verify every refactor with current callers, a focused compile/test, and guardrail review; preserve public methods unless migration is explicit. [`ISTQB-CTFL`, `SHAFT-MCP`]

## Valid examples

- Add `signInAs(user, password)` to an existing `LoginPage` that owns the verified email, password, and submit locators.
- Extract a reused product-card component that exposes `addToCart()` and `name()` across listing and search pages.
- Split an oversized checkout page into shipping, payment, and order-summary components while keeping the test's business assertion outside them.

## Boundary

- Keep a one-off interaction in the test until reuse or complexity earns an object; route locator ranking to `shaft-locator-design` and executable test assembly to `shaft-automated-test-authoring`.

Sources: [shared authoritative bibliography](../../shaft-developer/references/sources.md).
