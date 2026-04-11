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

### Batteries-Included Architecture
SHAFT must **never** require users to pre-install binaries, drivers, or external tools.  When adding a new tool dependency:
1. Use a three-tier resolution: system PATH → package-manager (npx/etc.) → self-download to `~/.m2/repository/`.
2. Expose the tool version as a `@Key`/`@DefaultValue` property in `Internal.java` (or the relevant properties interface).
3. Read the version via `SHAFT.Properties.internal.<property>()` — **never** hardcode it inline.

```java
// ✅ Read from SHAFT properties — user-overridable
String allureVersion = SHAFT.Properties.internal.allure3Version();

// ❌ Never hardcode versions
private static final String ALLURE_VERSION = "3.4.0";
```

### No Hardcoded Values Policy
Every configurable value — version strings, URLs, timeouts, thresholds, behavioral switches — **must** be declared as a `@Key`/`@DefaultValue` entry in the appropriate interface under `src/main/java/com/shaft/properties/internal/`:

| Interface | Purpose |
|-----------|---------|
| `Flags.java` | Behavioral toggles, engine-wide (static) |
| `Web.java` | Browser/WebDriver config |
| `Mobile.java` | Appium/mobile config |
| `API.java` | REST API defaults |
| `Timeouts.java` | Wait/timeout values |
| `Visuals.java` | Screenshot/visual testing |
| `Reporting.java` / `Allure.java` | Reporting behavior |
| `Paths.java` | File system paths |
| `Performance.java` / `Healenium.java` | Integration-specific |
| `Internal.java` | Internal version/build metadata |

Access properties via `SHAFT.Properties.<group>.<methodName>()`.  **Never** call `System.getProperty()` directly in framework code.

```java
// ❌ Hardcoded timeout value
int timeout = 30;

// ✅ Use SHAFT properties instead
SHAFT.Properties.timeouts.waitForLazyLoadingTimeout();

// ❌ Direct system property access
System.getProperty("targetBrowserName");

// ✅ Use SHAFT properties instead
SHAFT.Properties.web.targetBrowserName();
```

### Properties Scoping: Engine-Wide vs Thread-Local
**Engine-wide (static) — `Flags.java` pattern:**
- Applied via `System.setProperty` under `synchronized(Properties.class)`; affects **all threads**.
- Use only for: retry counts, driver lifecycle flags, engine feature toggles.
- Framework code must never leave these in a mutated state; document when callers are expected to reset them.

**Per-thread — `ThreadLocalPropertiesManager` pattern:**
- Stored in `ThreadLocal<java.util.Properties>`; invisible to other threads.
- `Properties.clearForCurrentThread()` **must** be called at lifecycle boundaries (e.g., after each test) to prevent stale state in thread-pool reuse scenarios (it clears `ThreadLocalPropertiesManager` plus all cached per-interface overrides).
- Use for: per-test browser type, target URL, credentials.

```java
// ❌ Bypasses SHAFT's property layer
System.setProperty("targetBrowserName", "firefox");

// ✅ Thread-local — only affects the calling thread
ThreadLocalPropertiesManager.setProperty("targetBrowserName", "firefox");
// Always clear in teardown:
Properties.clearForCurrentThread();
```

### Bug Fixes
When fixing a bug or unexpected behavior in framework source files, you **must** follow the **Bug Fixing Process** defined in `.github/copilot-instructions.md`. Key requirements:
- Reproduce the bug before writing any code
- Write a **failing test first** that captures the bug, then fix it so the test passes
- Make the **smallest possible change** — do not refactor unrelated code
- Run regression tests to confirm no other behavior is broken

### ⛔ Mandatory Pre-Commit Rules (No Exceptions)
> **You MUST NEVER commit untested framework code. There are no exceptions to these rules.**

Before committing **any** framework source change, you **must**:

1. **Compile**: Run `mvn clean install -DskipTests -Dgpg.skip` and confirm it succeeds
2. **Write Tests**: Create or update tests under `src/test/java/` that cover your changes — every new or modified feature requires test coverage
3. **Run Tests**: Execute `mvn test -Dtest=TestClassName` for all affected tests and confirm they pass
4. **Capture Evidence**: Take screenshots of test results to provide proof that tests were executed and passed
5. **Review Code**: Review your changes for correctness, JavaDoc completeness, backward compatibility, and adherence to SHAFT patterns
