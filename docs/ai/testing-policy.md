# Testing Policy

## Frameworks Found
- TestNG is the primary framework and Maven's default active test profile.
- JUnit Jupiter is supported through the `junit` Maven profile and listener code.
- Cucumber 7 uses Java, TestNG, PicoContainer, feature files, and Allure integration.
- Mockito supports focused unit/coverage tests.
- Selenium/Appium tests cover browser, mobile, local, Grid, BrowserStack, and LambdaTest paths.
- REST Assured and JDBC tests cover API and database behavior.
- JaCoCo, Codecov/Codacy readiness scripts, CodeQL, dependency review, and link checks provide additional gates.

## Test Locations
- `src/test/java/com/shaft/`: package-aligned framework tests.
- `src/test/java/testPackage/unitTests/`: broad focused unit/coverage suite.
- `src/test/java/testPackage/`: integration/E2E and examples, including cloud/mobile/properties/legacy coverage.
- `src/test/java/junitTestPackage/`: JUnit examples/tests.
- `src/test/java/cucumberTestRunner/` and `src/test/resources/*Features/`: Cucumber tests.
- `src/test/resources/TestSuites/`: TestNG XML suites.
- `src/test/resources/testDataFiles/`: JSON, CSV, YAML, HTML, image, PDF, API schema/body, and app fixtures.
- `tests/scripts/`: Python unit tests for repository automation scripts.

## Commands
### Compile/typecheck/package
```bash
mvn clean install -DskipTests -Dgpg.skip
```
Requires JDK 25 and Maven 3.9.0 or newer. This is the mandatory compile check for Java code changes.

### Focused TestNG test
```bash
mvn test -Dtest=TestClassName
```
Use comma-separated classes or quoted `%regex[...]` patterns when needed.

### Full Maven tests
```bash
mvn test
```
This can be slow and environment-dependent. Surefire is configured with `testFailureIgnore=true` so report generation can finish.

### JUnit profile
```bash
mvn test -Pjunit -Dtest=FullyQualifiedTestClass
```

### Cucumber / suites
Use the existing runners or suite selection that matches the affected area, for example:
```bash
mvn test -Dtest=cucumberTestRunner.CucumberTests
```
Some CI jobs pass browser, execution-address, or suite properties; copy the relevant workflow invocation rather than guessing.

### Python repository-tool tests
```bash
python -m unittest discover -s tests/scripts -p 'test_*.py'
```

### JavaDocs
```bash
mvn javadoc:javadoc
```
Run when public API documentation changes.

### Lint/format
Not found in current codebase scan: a dedicated local Java formatter or lint command. Preserve neighboring style. CodeQL and Codacy run in CI; Maven compilation and JavaDocs are the local static checks.

### E2E/UI infrastructure
Selenium Grid can be started with:
```bash
docker compose -f src/main/resources/docker-compose/selenium4.yml up --scale chrome=1 --scale edge=0 --scale firefox=0 -d
```
Stop and clean it after use. Cloud/mobile/browser E2E tests require browsers, Docker, Appium assets, devices, endpoints, or credentials and may only be runnable in CI.

## Validation Strategy
Use the cheapest check that can falsify the change first, then broaden only as risk requires:
1. **Static scope check:** inspect `git diff`, changed paths, configuration impact, and generated/untracked files.
2. **Focused check:** run the smallest affected unit test, documentation validator, or script test.
3. **Compile/type check:** run the mandatory Maven compile for Java changes.
4. **Integration/E2E check:** run only the affected browser/API/DB/mobile/Cucumber path when behavior crosses that boundary.
5. **Release check:** use the blocking build and user-guide smoke test defined in `docs/ai/release-policy.md` only for release readiness.

Validation rules:
- Run checks after the final edit, not only before it.
- A command that exits successfully but executes zero relevant tests is not a pass.
- Never treat skipped, ignored, empty, stale, or unexpectedly small results as evidence of correctness.
- Separate failures caused by the patch from environment limitations; do not relabel an agent error as an environment warning.
- Record exact commands and outcomes. Do not claim checks that were not executed.
- If a required check cannot run, perform the nearest meaningful lower-level check and name the exact remaining command/scenario and owner.

## When to Add Tests
- Every behavior change, bug fix, public overload, property, lifecycle change, parser, integration behavior, and concurrency fix needs focused coverage.
- Bug fixes should first reproduce the failure in a regression test when practical.
- Public GUI/API/DB changes should test fluent behavior and failure/reporting paths, not only the happy path.
- Property changes should cover defaults, setters/overrides, and thread-local cleanup.
- Dashboard changes should add/update nearby realtime reporter tests and manually verify rendering when feasible.
- Documentation-only changes do not require Maven tests unless they alter executable snippets or build/release instructions.

## Test Design Rules
### TestNG structure and naming
- Use `@BeforeClass` to initialize class-level `SHAFT.TestData.JSON` when needed.
- Use `@BeforeMethod` to create a fresh `SHAFT.GUI.WebDriver` for each browser test.
- Use `@AfterMethod(alwaysRun = true)` to call `driver.quit()` and clear mutable state even after failures.
- Use descriptive `@Test` methods; class names use `PascalCase` ending in `Test` or `Tests`, and methods use scenario-describing `camelCase`.
- For JUnit 5, use the equivalent `@BeforeAll`/`@BeforeEach`/`@AfterEach` lifecycle.
- Include only imports needed by the test; common SHAFT browser tests use `SHAFT`, `Locator`, Selenium `By`, and the relevant TestNG annotations.

### Assertions, locators, and data
- Use SHAFT fluent assertions such as `driver.assertThat()...perform()` where an equivalent exists; do not mix raw TestNG/JUnit assertions into framework-behavior tests without a concrete reason.
- Prefer the `Locator` builder for expressive selectors; stable `By.id`, `By.cssSelector`, and `By.xpath` remain acceptable.
- Store reusable test data under `src/test/resources/testDataFiles/` and load JSON through `SHAFT.TestData.JSON`; do not hardcode environment/customer data.

### Performance-mode restriction
- Do not enable `maximumPerformanceMode` for framework validation, regression, smoke, or release-gate tests. It intentionally disables diagnostics and evidence that tests rely on.

- Use `ThreadLocal<SHAFT.GUI.WebDriver>` for classes intended to run methods in parallel.
- Global property-dependent tests must set prerequisites inside each test and clear per-thread state during teardown.
- Classes that mutate engine-global flags should use `@Test(singleThreaded = true)` for within-class serialization and restore defaults in `@AfterMethod(alwaysRun = true)`; this does not protect against other classes mutating the same global state.
- During parallel runs, clear the contents of `allure-results` without replacing the live root directory.

## Allure as Test Oracle
1. Count Allure result JSON files/executed tests before analyzing status.
2. If the count is zero or unexpectedly low, report the run as invalid/empty and fix generation or rerun.
3. Use Allure status as authoritative; use `target/surefire-reports` as supporting diagnostics.
4. For CI failures, follow `docs/CI_FAILURE_INVESTIGATION.md` and inspect embedded Allure artifact results.

## If Tests Cannot Run
- State the exact command attempted and the concrete limitation (JDK, browser, Docker, device, credentials, network, or service).
- Run the nearest lower-level check that does not bypass safety, such as documentation structure checks, focused unit tests, compilation, or JavaDocs.
- Do not claim passing behavior from code inspection alone.
- List the exact CI or maintainer-run command still required.

## Minimum Validation Checklist
- [ ] Diff contains only intended files and no generated artifacts/secrets.
- [ ] Focused affected tests pass, or the limitation is documented.
- [ ] `mvn clean install -DskipTests -Dgpg.skip` passes for Java changes.
- [ ] Allure results are populated and checked for Maven test runs.
- [ ] Public API JavaDocs build when changed.
- [ ] Parallel-state cleanup and resource cleanup are covered.
- [ ] Relevant browser/API/DB/cloud/Cucumber path is validated based on risk.
- [ ] Final response lists tests run, tests not run, risks, and rollback.
- [ ] For Java source or test changes, capture screenshots of the successful test results and retain the populated Allure evidence required by repository review.
