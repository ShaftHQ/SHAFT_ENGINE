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

## Execution class

- Android language proof reads visible `text`, not `content-desc`. A locale change is a new process, with `language()` and `locale()` set before the driver starts. Do not cold-boot or wipe between language cohorts when the emulator is healthy.
- A static element uses `matchesReferenceImage`. A surface that is still changing uses `matchesReferenceImageWhenStable` (#6823). Do not call `matchesReferenceImage` on that surface. Take two screenshots a few seconds apart before adding the check. If they differ, use the stable API or do not add the check. The first run stores the baseline and passes. Commit that file. It matches one resolution. Another resolution is not a comparison against that baseline.
- Optional screens use `isPresent(By, Duration)` or `waitUntilAnyPresent`. Do not use `getElementsCount` and do not catch a click. Absence is a branch. A once-per-launch prompt is one check per session unless measured. Do not invent a new presence method (#6842).
- A shipped assertion has a stated way to fail. Reject `exists()` on an id that is also on the wrong screen, a non-empty check that passes on whitespace (trim first; whitespace-only text is not non-empty), a number label with no digit, an enabled-state change that did not record the before state, and a check that is true in every language used as proof of one language.

Sources: [shared authoritative bibliography](../../shaft-developer/references/sources.md).
