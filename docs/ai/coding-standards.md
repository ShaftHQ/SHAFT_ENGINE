# Coding Standards

## Existing Patterns
- Java packages use `com.shaft.*`; public consumers generally enter through nested namespaces in `com.shaft.driver.SHAFT`.
- Public APIs are fluent and return action/builders or the enclosing driver context where appropriate.
- Public classes and methods require JavaDoc with applicable `@param`, `@return`, and `@throws` tags.
- Utility classes use a private constructor that throws `IllegalStateException("Utility class")`.
- Configurable classes may expose `getInstance(...)` factories when that matches the surrounding API.
- Use non-static inner action classes when they need enclosing-instance state.
- Enums that carry values use constructor parameters; enum constants use `UPPER_SNAKE_CASE`.
- Lombok is used selectively; preserve neighboring style rather than introducing it everywhere.
- Internal helpers live in `internal` subpackages; do not expose internals merely to simplify a patch.

## Backend / Framework Conventions
This repository has no hosted backend. Treat Java framework source under `src/main/java/com/shaft/` as the backend/framework layer.

- Keep `SHAFT.java` as a thin public facade; place implementation in the responsible package.
- Preserve source and binary compatibility with the project-supported latest LTS Java version (currently JDK 25 in executable configuration).
- Public API removal requires `@Deprecated`, a JavaDoc migration note, and at least three calendar months between deprecation release and removal. Prefer overloads over signature changes.
- Keep driver and test state isolated. Use fresh drivers per test and `ThreadLocal` for state intended to vary by parallel test; document thread-safety guarantees in class JavaDoc.
- Clean up WebDriver/Appium sessions, processes, streams, sockets, JDBC connections, temporary files, and thread-local properties.
- Do not add direct console printing. Use `ReportManager`, `ReportManagerHelper`, or the established Log4j path.
- Do not wrap imports in try/catch blocks.


## Batteries-Included Tooling
SHAFT should not require consumers to pre-install drivers, binaries, or auxiliary tools. For a new tool integration:
1. Resolve from system `PATH` when available.
2. Fall back to an established package manager such as `npx` when appropriate.
3. Self-download/cache under the Maven repository when needed.
4. Define the tool version with `@Key`/`@DefaultValue` in the appropriate property interface, usually `Internal.java`.
5. Read it through `SHAFT.Properties`; never hardcode the version in implementation code.

## Frontend Conventions
There is no standalone frontend toolchain. Frontend-like code is mainly `src/main/resources/realtime/dashboard.html` and HTML test fixtures.

- Keep the realtime dashboard self-contained unless maintainers explicitly approve a build pipeline or dependency.
- Preserve existing CSS custom properties, accessible semantic HTML, light/dark themes, status naming, filtering, and connection-state behavior.
- Avoid external scripts, trackers, remote fonts, or runtime CDN dependencies.
- Escape or render execution data safely; never inject untrusted text with unsafe HTML APIs.
- Fixtures under `src/test/resources/testDataFiles/` should remain deterministic and local.

## API Conventions
- Use `RestActions`/`RequestBuilder` and REST Assured integration patterns rather than parallel HTTP abstractions.
- Keep base URIs, service paths, authentication headers, cookies, timeouts, and behavior configurable.
- Preserve request/response reporting while redacting or omitting secrets and sensitive payloads.
- Maintain existing response-status evaluation and fluent validation behavior.
- Add overloads for new optional request inputs; avoid ambiguous signatures.

## Error Handling
- Throw specific, actionable exceptions; preserve causes when wrapping.
- Do not silently swallow exceptions or convert failures into false success.
- Use discrete/debug logging only for diagnostic detail that is safe to retain.
- Cleanup should run even after test failures; test teardown uses `alwaysRun = true` where applicable.

## Logging and Reporting
- Use SHAFT reporting utilities so logs correlate with Allure steps and attachments.
- Never log access tokens, authorization headers, passwords, cloud keys, private keys, decrypted test data, or full sensitive request/response bodies.
- Avoid duplicate large attachments and unbounded logging in loops.
- Treat screenshots, videos, DOM, API bodies, SQL output, and environment details as potentially sensitive.

## Validation and Properties
- Use SHAFT fluent validation wrappers where applicable; tests should avoid raw TestNG/JUnit assertions when an equivalent SHAFT assertion exists.
- New configurable behavior belongs in the appropriate interface under `src/main/java/com/shaft/properties/internal/`, with `@Key`/`@DefaultValue`, access through `SHAFT.Properties`, and tests.
- Do not call `System.getProperty()` directly in framework code.
- Use `ThreadLocalPropertiesManager` for per-thread overrides such as browser, URL, or credentials, and call `Properties.clearForCurrentThread()` at lifecycle boundaries.
- Engine-wide flags are synchronized static state and are reserved for retry counts, driver lifecycle controls, and true engine-wide feature toggles. Document/reset mutations; do not use them for per-test configuration.
- Property interface placement: `Flags` for engine toggles, `Web` for browser/WebDriver, `Mobile` for Appium, `API` for REST defaults, `Timeouts` for waits, `Visuals` for screenshots/visuals, `Reporting`/`Allure` for reports, `Paths` for filesystem paths, `Performance`/`Healenium` for integrations, and `Internal` for internal tool/version metadata.

## Database Access
- `DatabaseActions` is a JDBC adapter to databases owned by systems under test.
- Use parameterized/prepared SQL where inputs can be influenced externally; do not concatenate credentials or untrusted values.
- Close connections/results and avoid logging connection strings or returned sensitive rows.
- Not found in current codebase scan: an application schema, ORM, repository layer, migrations, or tenant discriminator.
- Any proposed schema migration is outside normal repository architecture and requires team confirmation, migration steps, and rollback notes.

## Permissions and Data Scoping
Not found in current codebase scan: framework-owned users, roles, admin endpoints, authorization guards, or tenant/customer scoping. Do not invent them. Integrations authenticate to external services through caller-provided configuration and must retain least-privilege handling.

Execution isolation is required: do not share mutable headers/cookies, drivers, properties, report buffers, files, or database connections across tests unless the existing API explicitly manages that sharing safely.

## Code Quality and Relevance Gate
Before editing:
- State the behavior/contract that must change and the behavior that must remain unchanged.
- Trace the shortest path from public entry point to implementation and tests; stop exploring once the change boundary and verification path are clear.
- Reuse the closest established pattern. Introduce a new abstraction only when it removes real duplication or enforces an invariant needed by this task.

A change is complete only when:
- The implementation is the smallest coherent patch, with no unrelated cleanup.
- Names and control flow make the behavior legible without relying on comments; comments explain only non-obvious intent or constraints.
- Error, cleanup, concurrency, and sensitive-data paths are considered.
- Public compatibility, configuration, JavaDoc, tests, and user-facing docs are updated where the contract requires them.
- The final diff contains no accidental generated files, debug output, disabled checks, or claims unsupported by executed validation.

## Rules for Safe Changes
1. Inspect the target class, its direct callers, matching tests, property interface, and report/lifecycle hooks.
2. Make the smallest compatible change in the responsible module.
3. Add or update focused tests and test data.
4. Run the narrowest test first, then compile/package and broader checks as risk warrants.
5. Verify Allure result population before treating test status as authoritative.
6. Update JavaDocs/examples for public behavior.
7. Do not mix dependency/version/release changes into ordinary patches.
