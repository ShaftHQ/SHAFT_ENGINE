---
applyTo: "src/main/java/**/*.java"
---

## Framework Source Code Requirements

When writing or editing framework source files under `src/main/java/`, follow these rules:

### JavaDoc
- Every `public` class and `public` method **must** have a JavaDoc comment
- Include `@param`, `@return`, and `@throws` tags where applicable
- Document non-obvious logic or workarounds inline with `//` comments

### Class Structure
- **Public API facades**: Use nested `public static` classes to expose namespaced APIs (see `SHAFT.java`)
- **Utility classes**: Add a `private` constructor that throws `IllegalStateException("Utility class")` to prevent instantiation
- **Factory methods**: Prefer `getInstance(...)` factory methods alongside or instead of constructors for configurable classes
- **Inner sub-action classes**: Use non-static inner classes when the sub-action needs access to the enclosing instance's state

### Thread Safety
- Use `ThreadLocal<T>` for any state that must be isolated per test thread (e.g., soft assertion buffers, last-used locators)
- Document thread-safety guarantees in the class JavaDoc

### Backward Compatibility
- Do not remove or rename `public` methods without a deprecation cycle (`@Deprecated` + JavaDoc note)
- Prefer overloading over changing existing method signatures

### Enums
- Use enums with constructor parameters for typed constants that carry a value (e.g., `ValidationType(boolean value)`)
- Keep enum names in `UPPER_SNAKE_CASE`

### Error Handling
- Throw specific, descriptive exceptions; never swallow exceptions silently
- Log meaningful messages before re-throwing or handling exceptions
- Use SHAFT's internal logging utilities (not `System.out.println`)

### ⛔ Mandatory Pre-Commit Rules (No Exceptions)
> **You MUST NEVER commit untested framework code. There are no exceptions to these rules.**

Before committing **any** framework source change, you **must**:

1. **Compile**: Run `mvn clean install -DskipTests -Dgpg.skip` and confirm it succeeds
2. **Write Tests**: Create or update tests under `src/test/java/` that cover your changes — every new or modified feature requires test coverage
3. **Run Tests**: Execute `mvn test -Dtest=TestClassName` for all affected tests and confirm they pass
4. **Capture Evidence**: Take screenshots of test results to provide proof that tests were executed and passed
5. **Review Code**: Review your changes for correctness, JavaDoc completeness, backward compatibility, and adherence to SHAFT patterns
