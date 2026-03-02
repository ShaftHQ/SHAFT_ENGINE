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
