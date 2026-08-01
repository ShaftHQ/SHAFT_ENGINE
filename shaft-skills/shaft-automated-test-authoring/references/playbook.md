# Automated-test-authoring playbook

## Ten practices

1. Start from reviewed requirements, risks, and test cases; encode one observable behavior and oracle per test. [`ISTQB-CTFL`]
2. Search current repository tests, fixtures, page/API objects, properties, and runner conventions before adding files or dependencies. [`SELENIUM-PRACTICES`, `SHAFT-GUIDE`]
3. Ground unfamiliar SHAFT syntax with `shaft_guide_search` and current repository exemplars; never write an API from memory. [`SHAFT-GUIDE`, `SHAFT-MCP`]
4. Use the public SHAFT facade for the target surface: `SHAFT.GUI`, `SHAFT.API`, `SHAFT.DB`, `SHAFT.Validations`, or supported CLI actions. [`SHAFT-GUIDE`]
5. Keep setup, action, oracle, and cleanup visible; make tests independent, deterministic, parallel-safe, and explicit about required external infrastructure. [`ISTQB-CTFL`, `SELENIUM-PRACTICES`]
6. Put stable configuration and secrets outside source, generate only necessary data, and restore mutated state through reliable cleanup. [`ISTQB-CTFL`, `SHAFT-REPORTING`]
7. Rely on SHAFT synchronization and state assertions; never add `Thread.sleep`, raw driver calls, headed defaults, or brittle absolute locators. [`SHAFT-GUIDE`, `SELENIUM-PRACTICES`]
8. Extract page, component, API, or data helpers only when reuse is proven; one test does not justify a new abstraction. [`SELENIUM-PRACTICES`]
9. Run `test_code_guardrails_check`, review the real diff, and use the smallest focused compile or test that proves the new behavior. [`SHAFT-MCP`, `ISTQB-CTFL`]
10. Preserve test identity, traceability, reports, attachments, and diagnostic messages so failures remain attributable and actionable. [`ALLURE-RESULTS`, `SHAFT-REPORTING`]

## Valid examples

- Implement a TestNG login test that reuses an existing `LoginPage`, drives `SHAFT.GUI.WebDriver`, and asserts the dashboard URL.
- Implement a JUnit service contract using `SHAFT.API`, a target status code, and response-body assertions.
- Implement a database-backed integration test using `SHAFT.DB`, isolated data, explicit cleanup, and SHAFT validations.
- Repair a Cucumber step that uses raw Selenium by reusing the project's SHAFT driver and page method.

## Boundary

- If the requested deliverable is still requirements, test cases, locators, page-object design, or failure diagnosis, finish that specialist output first; do not hide an unresolved design decision inside code.

Sources: [shared authoritative bibliography](../../shaft-developer/references/sources.md).
