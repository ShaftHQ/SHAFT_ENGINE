Contribution is easy!

Just join our Slack channel linked in the readme.md, tell us your idea, and we'll help you implement it yourself.
The more you contribute, the more you'll get the hang of how things are done.

## Requirements

For any fixes we need evidence before and after the fix on all impacted platforms.
For every new feature we need tests to validate it.
For every new public method we need clear JavaDocs.

Nothing fancy... Just keep it clear and simple.

## Code Quality Standards

### JavaDocs
- Every `public` class and method **must** have a JavaDoc comment.
- Include `@param`, `@return`, and `@throws` tags where applicable.
- Add `@see` links to related classes and the [SHAFT User Guide](https://shafthq.github.io/) where appropriate.
- Use `{@code ...}` for inline code references and `{@link ...}` for type references.
- Validate JavaDoc output locally with `mvn javadoc:javadoc` before opening a PR when JavaDocs were changed.

### Logging
- Use Log4j (`LogManager.getLogger(ClassName.class)`) or SHAFT's `ReportManager` for all logging.
- **Never** use `System.out.println` or `System.err.println` in production code.
- Use `ReportManager.log()` for user-visible report steps and `ReportManager.logDiscrete()` for diagnostic messages.

### Testing
- All tests should use SHAFT's fluent assertion API (`driver.assertThat()`, `Validations.assertThat()`).
- Use `@BeforeMethod` / `@AfterMethod(alwaysRun = true)` for driver lifecycle.
- Follow the naming convention: `[Feature]Tests` for classes, `shouldExpectedBehaviorWhenCondition` for methods.
- Store test data in JSON files under `src/test/resources/testDataFiles/`.

### Complexity
- Keep methods short and focused (ideally under 30 lines).
- Prefer extracting helper methods over deep nesting.
- Use meaningful, descriptive names for variables and methods.
- Prefer direct boolean expressions over multi-branch temporary flags when readability is improved.

## Continuous Improvement Guard Rails (PDCA Cycles 2-5)

- **JavaDocs:** For any edited public API, add concise JavaDoc with `@param`/`@return` where applicable and at least one `@see` reference to the [SHAFT User Guide](https://shafthq.github.io/).
- **Complexity:** Keep surgical simplifications localized to touched methods and preserve behavior.
- **Testing:** Every behavior change requires focused automated tests in `src/test/java/`.
- **Memory:** Always clear `ThreadLocal` state with `.remove()` when lifecycle ends to prevent leaks on pooled threads.
- **Logging UX:** Keep CI output readable; disable ANSI-heavy formatting when non-interactive or explicitly configured off.

## Build & Test

```bash
# Build without tests
mvn clean install -DskipTests -Dgpg.skip

# Run a specific test class
mvn test -Dtest=TestClassName

# Generate JavaDocs
mvn javadoc:javadoc
```
