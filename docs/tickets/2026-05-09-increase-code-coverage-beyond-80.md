# GitHub issue: Increase code coverage beyond 80%

## Metadata

- **Suggested title:** Increase code coverage beyond 80%
- **Suggested labels:** `testing`, `coverage`, `ci`, `e2e`, `technical debt`
- **Suggested type:** Feature / quality improvement

## Problem statement

SHAFT_ENGINE should consistently report code coverage above 80% while keeping the E2E suite meaningful, reliable, and regularly executed. Current coverage work should not only add tests for uncovered branches, but also close gaps in the existing E2E workflows by fixing failing tests and ensuring valuable tests are not left outside scheduled CI execution.

## Scope

This issue covers the following work:

1. **Establish the coverage baseline**
   - Generate the current JaCoCo coverage report from the existing Maven test suite.
   - Record the current line, branch, method, and class coverage numbers.
   - Identify the packages/classes with the largest uncovered areas and prioritize coverage improvements that provide meaningful behavioral validation.

2. **Explore and fix tests currently failing in E2E workflows**
   - Review failures from the active E2E workflows:
     - `.github/workflows/e2eTests.yml`
     - `.github/workflows/e2eLocalTests.yml`
     - `.github/workflows/e2eLambdaTestTests.yml`
   - Use workflow summaries, Surefire reports, Allure results, and available artifacts to identify failing or broken test methods.
   - Fix tests that are flaky, outdated, environment-coupled, or asserting incorrect behavior.
   - Prefer stabilizing tests through better setup, teardown, waits, deterministic data, and explicit environment assumptions rather than weakening assertions.

3. **Explore and fix tests not executed by any E2E workflow**
   - Inventory test classes and methods under `src/test/java/`.
   - Compare the inventory against the `-Dtest=...` selectors in the E2E workflows.
   - Identify tests that are never selected by any workflow.
   - Fix, rewrite, or classify these tests so every valuable test has a clear execution path.

4. **Guarantee regular execution for new or fixed tests**
   - Add every newly created, fixed, or verified test to at least one E2E workflow.
   - Choose the workflow that matches the test's dependency profile:
     - mocked/unit-style tests in fast API/utility/local jobs where possible;
     - browser tests in local browser, Selenium Grid, BrowserStack, or LambdaTest jobs as appropriate;
     - mobile tests in the matching cloud/mobile workflow;
     - database tests in database-specific jobs;
     - API tests in API-specific jobs.
   - Keep workflow selectors explicit enough that future contributors can understand why each test is covered.

5. **Remove unfixable tests**
   - If a test cannot be fixed or rewritten in a maintainable way, delete it rather than leaving dead, skipped, or permanently failing test code.
   - Document the reason for deletion in the pull request that removes the test.
   - Avoid deleting tests solely to increase coverage or hide legitimate product defects.

6. **Prefer mocked and uncoupled tests for new coverage**
   - For new tests, prefer mocked, isolated, deterministic test methods.
   - Avoid unnecessary coupling to browsers, external services, mobile devices, remote grids, cloud providers, file-system state, wall-clock timing, or public internet endpoints.
   - Use test data files and local fixtures instead of hardcoded large payloads where practical.

7. **Allow realistic E2E scenarios when coupling is necessary**
   - When coverage requires validating integrated behavior, create realistic E2E scenarios that may:
     - launch a browser;
     - exercise Selenium Grid, BrowserStack, LambdaTest, or local browser execution;
     - perform requests against a real endpoint;
     - interact with a test database;
     - use local HTML pages or local API/database fixtures.
   - Any required local HTML pages, API servers, databases, containers, or seed data must include setup and teardown in the related workflow or test lifecycle.
   - Do not depend on mutable third-party data unless the test is explicitly scoped as an integration test and has clear failure diagnostics.

## Proposed implementation plan

1. Run the current coverage report and save the baseline metrics in the pull request description.
2. Build a test-execution inventory by comparing `src/test/java/` classes to the workflow `-Dtest` selectors.
3. Triage existing E2E failures first, because failing jobs can mask coverage progress.
4. Add or repair high-value mocked tests for uncovered framework logic.
5. Add realistic E2E tests only when mocked coverage cannot validate the behavior.
6. Update the relevant E2E workflow selectors for every new, fixed, or verified test.
7. Remove tests that cannot be repaired or rewritten without creating brittle CI behavior.
8. Re-run the narrowest affected jobs locally where possible.
9. Confirm the full CI coverage report exceeds 80% and that E2E workflow summaries show no failed or broken tests.

## Acceptance criteria

- [ ] The JaCoCo report shows total code coverage above 80% for the agreed project-level metric.
- [ ] Existing E2E workflow failures investigated during this work are fixed, rewritten, or documented with follow-up details.
- [ ] Tests under `src/test/java/` that were not selected by any E2E workflow are either added to an appropriate workflow, rewritten into a covered test, or deleted with justification.
- [ ] Every new, fixed, or verified test is included in at least one E2E workflow selector.
- [ ] New tests prefer mocked, uncoupled, deterministic methods unless true E2E coverage is necessary.
- [ ] Any new local HTML page, API server, database, container, or fixture has explicit setup and teardown in the test lifecycle or related workflow.
- [ ] Unfixable tests are deleted rather than left skipped, dead, or permanently failing.
- [ ] CI artifacts include coverage and Allure/Surefire evidence that the updated tests execute regularly.

## Suggested validation commands

```bash
mvn clean test -Dgpg.skip
mvn jacoco:report -Dgpg.skip
mvn -e test "-Dtest=<affected-test-selector>" "-DgenerateAllureReportArchive=true"
```

## Notes

- `mvn test` may continue after test failures because the project allows post-test reporting to complete; inspect Surefire, Allure, and workflow summaries before declaring a run successful.
- Browser, mobile, cloud, and database tests may require credentials or environment setup that is only available in CI.
- Do not run Maven Central deployment, JavaDoc publishing, or credentialed cloud workflows as part of this issue unless maintainers explicitly request it.
