Contribution is easy!

Just join our Slack channel linked in the readme.md, tell us your idea, and we'll help you implement it yourself.
The more you contribute, the more you'll get the hang of how things are done.

## Requirements

For fixes, provide focused evidence that reproduces the problem and proves the
result on the affected path. Add tests for behavior changes and JavaDocs for
new public APIs.

Nothing fancy... Just keep it clear and simple.

## Code Quality Standards

### JavaDocs
- Every `public` class and method **must** have a JavaDoc comment.
- Include `@param`, `@return`, and `@throws` tags where applicable.
- Add `@see` links to related classes and the [SHAFT User Guide](https://shaftengine.netlify.app/docs/start/overview) where appropriate.
- Use `{@code ...}` for inline code references and `{@link ...}` for type references.
- Validate JavaDoc output locally with `mvn -pl shaft-engine javadoc:javadoc` before opening a PR when JavaDocs were changed.

### Logging
- Use Log4j (`LogManager.getLogger(ClassName.class)`) or SHAFT's `ReportManager` for all logging.
- **Never** use `System.out.println` or `System.err.println` in production code.
- Use `ReportManager.log()` for user-visible report steps and `ReportManager.logDiscrete()` for diagnostic messages.

### Testing
- All tests should use SHAFT's fluent assertion API (`driver.assertThat()`, `Validations.assertThat()`).
- Use `@BeforeMethod` / `@AfterMethod(alwaysRun = true)` for driver lifecycle.
- Follow the naming convention: `[Feature]Tests` for classes, `shouldExpectedBehaviorWhenCondition` for methods.
- Store test data in JSON files under `shaft-engine/src/test/resources/testDataFiles/`.

### Complexity
- Keep methods short and focused (ideally under 30 lines).
- Prefer extracting helper methods over deep nesting.
- Use meaningful, descriptive names for variables and methods.
- Prefer direct boolean expressions over multi-branch temporary flags when readability is improved.

## Validation By Risk

- **Documentation or agent guidance:** run the relevant deterministic validator
  plus `python3 scripts/ci/validate_documentation_boundaries.py` and
  `git diff --check`; no Maven build is needed.
- **Localized code:** run the affected tests, then compile/package once before
  finalizing the change.
- **Shared API, concurrency, build, or release changes:** run targeted tests,
  relevant module checks, and the full compile/package command.
- **Visual behavior:** include image or browser evidence. Console output and
  test reports are sufficient for non-visual changes.
- **External/cloud E2E:** run only when the required infrastructure is
  available and the result is necessary to prove the change.

Keep simplifications local, clear `ThreadLocal` state at lifecycle boundaries,
and preserve readable non-interactive logging.

## Documentation

The Docusaurus site is the canonical location for product, usage,
architecture, migration, and maintainer documentation. Do not add public
guides, module READMEs, or a local `docs/` tree to this repository.

When a change affects users:

1. Update [ShaftHQ/shafthq.github.io](https://github.com/ShaftHQ/shafthq.github.io).
2. Verify the documentation deploy preview and its affected canonical routes.
3. Link the documentation pull request in the engine pull request.
4. Merge and deploy the documentation first. Confirm the production routes
   return HTTP 200 before merging dependent engine documentation cleanup.
5. If documentation is not required, provide a concrete reason in the pull
   request template.

Operational Markdown remains allowed for agent instructions, governance,
GitHub templates, skills, and test fixtures.

## Build & Test

```bash
# Build without tests
mvn clean install -DskipTests -Dgpg.skip

# Run a specific test class
mvn -pl shaft-engine -am test -Dtest=TestClassName

# Generate JavaDocs
mvn -pl shaft-engine javadoc:javadoc
```
