# Assertions playbook

## Ten practices

1. Derive each oracle from an acceptance criterion, invariant, contract, or risk; never assert an implementation detail merely because it is easy to read. [`ISTQB-CTFL`]
2. Assert the smallest stable observable state that proves the behavior, close to the action that produced it. [`ISTQB-CTFL`, `SELENIUM-PRACTICES`]
3. Use `assertThat()` for a blocking invariant and `verifyThat()` only when continued execution intentionally collects useful independent evidence. [`SHAFT-GUIDE`]
4. Select the typed SHAFT surface that owns the actual value: browser, element, API response, object, number, file, or image. [`SHAFT-GUIDE`]
5. Terminal validation methods execute immediately and return an executor only for optional report-message customization; creating a builder alone provides no verdict. [`SHAFT-GUIDE`]
6. Compare exact values only when exactness is contractual; otherwise use contains, regex, range, presence, state, or schema assertions that express the real tolerance. [`ISTQB-CTFL`]
7. Keep expected and actual orientation clear, normalize only contractually irrelevant variation, and never weaken an assertion to make a flaky result green. [`ISTQB-CTFL`, `ALLURE-STABILITY`]
8. Add a concise custom report message when domain intent would otherwise be unclear, naming expected behavior and relevant identity without secrets. [`SHAFT-REPORTING`]
9. Attach or preserve the smallest diagnostic evidence needed to explain failure; do not substitute screenshots or logs for an executable oracle. [`ALLURE-RESULTS`, `SHAFT-REPORTING`]
10. Prove negative and failure paths with focused tests, and confirm hard/soft behavior matches the runner and report semantics. [`ISTQB-CTFL`, `ALLURE-RESULTS`]

## Valid examples

- Assert `driver.assertThat().browser().url().contains("/dashboard")` after successful sign-in.
- Assert `driver.element().assertThat(status).text().isEqualTo("Paid")` after payment capture.
- Assert `api.assertThatResponse().extractedJsonValue("$.id").isEqualTo(expectedId)` for a service contract.
- Use `SHAFT.Validations.verifyThat().object(warning).contains("deprecated")` only when later independent checks remain valuable.

## Boundary

- Assertions do not decide scenario coverage, locator strategy, or failure root cause; route those deliverables to `shaft-test-case-design`, `shaft-locator-design`, or `shaft-failure-analysis`.

Sources: [shared authoritative bibliography](../../shaft-developer/references/sources.md).
