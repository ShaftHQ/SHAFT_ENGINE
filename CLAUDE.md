# SHAFT_ENGINE — AI Agent & Developer Onboarding Context

> This file is the primary context source for AI agents and new contributors working on this codebase.
> Keep it up to date as architecture evolves.

---

## What this project is

SHAFT_ENGINE is an open-source Java test automation framework built on top of Selenium, Playwright,
Appium, RestAssured, and Cucumber. It wraps those tools behind a fluent, opinionated API and adds
cross-cutting concerns: Allure reporting, parallel execution, property-based configuration, and
validation DSL.

- **GitHub:** https://github.com/ShaftHQ/SHAFT_ENGINE
- **Primary maintainer:** ShaftHQ org
- **Consuming projects (internal):** shaftSetup, POS, qhr-test (all live in separate repos)
- **Stack:** Java 25, Maven, TestNG, Allure 2.x, Selenium 4, RestAssured, Cucumber, Appium

---

## Module structure

The project is a 6-module Maven monorepo (migrated from monolith in 2026-05).
Dependency chain flows strictly top-down:

```
shaft-bom
  └─► shaft-core
        ├─► shaft-api
        ├─► shaft-db
        └─► shaft-web
              └─► shaft-engine   (thin aggregator / backwards-compat umbrella)
```

| Module | Package root | Contents |
|--------|-------------|----------|
| `shaft-bom` | — | BOM POM only — all version pins live here |
| `shaft-core` | `com.shaft.{cli,enums,listeners,performance,properties,tools,validation}` | Core framework: property system, tools, listeners, primitive validation, CLI |
| `shaft-api` | `com.shaft.api` | REST — `RestActions`, `RequestBuilder`, `ApiValidations`, REST validation builders |
| `shaft-db` | `com.shaft.db` | `DatabaseActions` |
| `shaft-web` | `com.shaft.{driver,gui,cucumber,validation(web)}` | WebDriver, Appium, Playwright, browser/element actions, web validation builders, ALL test suites |
| `shaft-engine` | — | Empty aggregator POM; depends only on `shaft-web`; published for backwards compat |

**Rule:** Never add a `shaft-web` dependency to `shaft-core` or `shaft-api`. The dependency chain is intentional — consumers who only need REST should not pull in Selenium.

---

## Build environment

```powershell
# Windows — JDK 25 enforcer is active; must set JAVA_HOME explicitly
$env:JAVA_HOME = "C:\Program Files\Eclipse Adoptium\jdk-25.0.3.9-hotspot"

# Full test run (all tests live in shaft-web)
mvn test -pl shaft-web -am

# Compile all modules
mvn compile -pl shaft-web -am

# Compile single module
mvn compile -pl shaft-core

# Run a single test class
mvn test -pl shaft-web -am -Dtest=PropertyFileManagerTest
```

On Linux/macOS CI the enforcer checks are the same; JDK 25 is the only supported version.

---

## Validation layer — the most important architectural boundary

There are three entry points for the validation DSL. Use the right one or you will get compile errors
(shaft-web cannot see shaft-core internal types from within shaft-web's own sources without going
through the public API):

| Entry point | Location | Use for | Returns |
|-------------|----------|---------|---------|
| `Validations` | `shaft-core/.../validation/Validations.java` | Primitives, objects, numbers, files, JSON, collections | `ValidationsBuilder` |
| `WebValidations` | `shaft-web/.../validation/WebValidations.java` | WebDriver elements, browser state | `WebValidationsBuilder` |
| `ApiValidations` | `shaft-api/.../validation/ApiValidations.java` | REST responses (`Response` objects) | `RestValidationsBuilder` |

### Builder hierarchy

```
ValidationsBuilder          (shaft-core)   — base; handles object/number/file/json
  └─► WebValidationsBuilder (shaft-web)    — adds element() and browser()
RestValidationsBuilder      (shaft-api)    — standalone; handles Response objects
```

### Anti-pattern to avoid

`shaft-web` previously contained a duplicate `com.shaft.validation.internal.ValidationsBuilder`
at the exact same FQCN as shaft-core's class. It was deleted in PR #2646. Do NOT create new
classes in shaft-web that shadow shaft-core's FQCNs.

### WizardHelpers

`shaft-web/.../driver/internal/WizardHelpers.java` is a utility that delegates to
`WebValidations.assertThat()` and `WebValidations.verifyThat()`. It is the standard way
driver/page-object helpers build typed validation chains without exposing builders directly.

---

## Property system (shaft-core)

Properties are loaded at runtime from `*.properties` files in the classpath. Key classes:

| Class | Role |
|-------|------|
| `PropertyFileManager` | Loads `.properties` files into `System.properties`; use try-with-resources for FileInputStream (Windows file-locking) |
| `PropertiesHelper` | Post-processes properties after load (e.g., maps OS name to Selenium Platform enum — always lowercase: `"windows"`, `"linux"`, `"mac"`) |
| `Properties` (various) | Typed accessors per domain — `ExecutionProperties`, `BrowserProperties`, etc. |

Default property template: `shaft-web/src/main/resources/properties/default/custom.properties`
(must include `setParallel`, `setThreadCount`, `maximumPerformanceMode`).

Runtime-generated properties live at `shaft-web/src/main/resources/properties/` and are gitignored
(tracked only the `default/` subdirectory).

---

## Listener architecture (shaft-core)

TestNG lifecycle is wired through:

- `TestNGListener` — main entry point; registered via `META-INF/services`
- `FrameworkContext` — holds per-test state
- Allure integration lives in `shaft-web` (web-specific attachment helpers)

---

## Test suite organization (shaft-web)

All tests live under `shaft-web/src/test/`. Key packages:

```
testPackage/
  gui/           — Selenium/Appium element tests
  api/           — REST API tests
  validation/    — ValidationsBuilder unit tests
  LambdaTest/    — LambdaTest cloud tests (require env vars)
  appium/        — Mobile tests (require Android SDK / iOS)
poms/            — Page Object Models used by tests
```

**Pre-existing failures (do not fix without understanding root cause):**
- 14 SSL failures against `jsonplaceholder.typicode.com` — network/CDN flakiness
- 7 `ExcelFileManager` / `RecordManager` Windows file-locking (same unclosed `FileInputStream` class of bug fixed in `PropertyFileManager` — deferred)
- 6 missing environment (no Android SDK, no PostgreSQL, no browser on headless CI)
- 1 Allure version mismatch (system has 3.x, test expects 2.x)

When a `@BeforeClass` SSL call fails, the whole class is skipped — so the reported test count varies
between runs depending on network state.

---

## GitHub workflow

```powershell
# Auth: SSH remote + gh CLI with GH_TOKEN
# GH_TOKEN is set permanently in the user's environment

# Create PR
gh pr create --title "..." --base main --head <branch> --body @'
[body]
'@

# List PRs
gh pr list

# Check PR status
gh pr view 2646
```

**Important PowerShell note:** Use `@'...'@` here-strings (NOT bash `$(cat <<'EOF'...EOF)`) when
passing multiline strings to native executables. The `<<` operator is not valid PowerShell.

---

## Open PRs

| PR | Branch | Description | Status |
|----|--------|-------------|--------|
| [#2646](https://github.com/ShaftHQ/SHAFT_ENGINE/pull/2646) | `Monorepo-Shaft-mvp` | Monorepo Phase 1 + ValidationsBuilder boundary fix | Open → `main` |

---

## Architectural decisions (ADRs)

Full ADRs live in `D:\Projects\ObsidianVault\Projects\SHAFT_ENGINE\architecture\decisions\`.

| ADR | Decision |
|-----|----------|
| ADR-001 | — |
| ADR-002 | — |
| ADR-003 | Optional deps / reflection approach — reverted; preserved for future reference |
| ADR-004 | Monorepo Phase 1 module structure |

---

## Deferred work

| Item | Where | Notes |
|------|-------|-------|
| `ExcelFileManager` file-locking | `shaft-web/.../support/ExcelFileManager.java` | Unclosed `FileInputStream`; same fix as `PropertyFileManager` (try-with-resources) |
| `RecordManager` file-locking | `shaft-web/.../support/RecordManager.java` | Same class of bug |
| Consumer migration from umbrella | downstream repos | `shaft-engine` umbrella kept for backwards compat; teams migrate to individual modules over time |

---

## Key recent changes (2026-05)

- **2026-05-08** — Monorepo Phase 1 complete (PR #2646): 6 modules, 11 commits, 4 bug fixes, ValidationsBuilder shadowing eliminated
- **2026-05-07** — Duplicate log entry fix (PR #2629): removed extra `Reporter.log()` call in `JavaHelper.compareTwoObjects()`
- **2026-05-07** — Optional deps approach reverted upstream (PR #2498 by ShaftHQ); PR #2496 closed
