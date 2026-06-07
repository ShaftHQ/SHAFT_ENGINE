# SHAFT_ENGINE AI Context

## Product Summary
SHAFT_ENGINE is a Maven-published Java test automation framework consumed by downstream test projects. Its `com.shaft.driver.SHAFT` facade exposes fluent APIs for web and mobile UI automation, REST/GraphQL API testing, CLI and file operations, database queries, test data, validations, accessibility, performance, properties, and Allure-based reporting.

This repository is a library, not a conventional hosted application. Not found in current codebase scan: application routes/controllers, ORM entities, an owned business database schema, migration tooling, queues/workers, or a standalone JavaScript frontend build.

## Core Modules
- `src/main/java/com/shaft/driver/`: public `SHAFT` facade and driver lifecycle/factory code.
- `src/main/java/com/shaft/gui/`: Selenium/Appium browser, element, touch, alert, locator, image, and video support.
- `src/main/java/com/shaft/api/`: REST Assured request building, execution, filtering, response handling, and GraphQL helpers.
- `src/main/java/com/shaft/cli/`: terminal and filesystem actions.
- `src/main/java/com/shaft/db/`: JDBC database actions for test targets.
- `src/main/java/com/shaft/validation/`: fluent native, API, file, JSON, image, and accessibility validations.
- `src/main/java/com/shaft/properties/`: OWNER-backed configuration plus per-thread overrides.
- `src/main/java/com/shaft/listeners/`: TestNG, JUnit, Cucumber, WebDriver, retry, update, and report lifecycle integration.
- `src/main/java/com/shaft/tools/`: test-data IO, reports, project support, integrations, and security helpers.
- `src/main/resources/realtime/dashboard.html`: embedded realtime report dashboard; there is no separate frontend application.

## Architecture Summary
Java 25 and Maven build a single JAR. The facade delegates to modular action/helper classes. Runtime configuration comes from typed property interfaces and default resources. Test lifecycle listeners initialize configuration, drivers, reporting, and integrations. Selenium/Appium drive UI targets; REST Assured drives APIs; JDBC drives external test databases; Allure and Log4j provide reporting/logging. CI exercises local, Selenium Grid, cloud-device, API, database, Cucumber, quality, and release paths.

## Critical Rules
- Preserve public API/fluent compatibility; public APIs require at least three calendar months of deprecation before removal and must remain source/binary-compatible with the project-supported latest LTS Java version.
- Public classes/methods require JavaDoc; use overloads or deprecation rather than silent breaking changes.
- Add configurable values through `src/main/java/com/shaft/properties/internal/` and `SHAFT.Properties`; do not directly read system properties in framework code.
- Use `ThreadLocalPropertiesManager` for per-test/per-thread overrides and clear them at lifecycle boundaries.
- Use SHAFT report/log utilities; do not add `System.out.println` or silently swallow exceptions.
- Tests should use SHAFT fluent assertions where applicable and clean up drivers with always-run teardown.
- Treat `target/`, reports, downloaded binaries, and generated output as non-source artifacts.
- Release/deploy commands require maintainer credentials and must not be run during normal tasks.

## Data Isolation / Multi-Tenancy
Not found in current codebase scan: product-level tenants, organizations, customer records, or tenant-scoped persistence.

The relevant isolation boundary is test execution state. Driver instances, mutable properties, cookies/headers, report state, files, and test data must not leak across tests or parallel threads. Prefer fresh drivers per test, `ThreadLocal` state where supported, unique temporary/output paths, and lifecycle cleanup. Do not log or attach credentials or target-system data unnecessarily.

## Common Risk Areas
- Global/static property mutation causing parallel TestNG flakes.
- Driver, process, file, socket, or database connection leaks.
- Logging/attaching API headers, tokens, request bodies, credentials, or decrypted test data.
- Changes to the large public facade or fluent return types that break downstream binaries/source.
- Browser/Appium/cloud changes that cannot be validated on a bare local environment.
- Surefire's `testFailureIgnore=true`: Maven completion alone is not a reliable test oracle; verify populated Allure results.
- Version alignment among `pom.xml` and release-related internal properties.
- Embedded dashboard changes affecting realtime-report serialization or browser compatibility.

## Context and Memory Hygiene
- Treat current code, tests, executable configuration, and task-linked issue/PR feedback as primary evidence. Prose that conflicts with executable behavior is stale until corrected.
- Search narrowly, then open only the exact sections needed. Prefer summaries and file/line references over copying large documents into working context.
- Do not load `.github/copilot-memory.md` by default. It is historical, non-normative context used only to investigate a recurring issue or prior decision.
- Add a memory entry only for a verified, durable lesson likely to recur. Do not store task status, speculative conclusions, transcripts, secrets, or facts already captured in authoritative policy.
- If new evidence invalidates an instruction, update the authoritative `docs/ai/` policy in the same task or report the conflict; do not work around a stale rule silently.
- Do not repeatedly reopen unchanged files. Keep a compact working summary of decisions, evidence, touched files, validation, and unresolved risks.

## AI Working Policy
- Convert the request into acceptance criteria, then read only context that can change the implementation or validation decision.
- Do not infer an application backend, frontend, tenant model, or permission model that is not present.
- Label inferences as `Inferred from codebase: ...`; link unresolved governance decisions to a GitHub issue and mark them clearly for team confirmation.
- Reproduce bugs and add a failing regression test first when practical.
- Keep diffs minimal; do not mix dependency, release, production-code, and documentation churn.
- Distinguish observed facts, inferences, and unknowns; report validation evidence and environmental limitations explicitly.

## Token-Efficient Reading Policy

Always read this file. This is the only always-read shared context file. Do not preload any other document or skill.

Then read one or more additional docs only when the current task requires them:

- Architecture changes → `docs/ai/architecture.md`
- Behavior/test changes → `docs/ai/testing-policy.md`
- Auth/security/data-scope/admin/logging changes → `docs/ai/security-policy.md`
- Release validation → `docs/ai/release-policy.md`
- Code review → `docs/ai/code-review.md`
- Unclear domain terms → `docs/ai/glossary.md`

Skills are also task-scoped: load only the single relevant skill from `.agents/skills/`; do not load all skills for context. Normal work should inspect only impacted modules and nearby files, not rescan the repository.
