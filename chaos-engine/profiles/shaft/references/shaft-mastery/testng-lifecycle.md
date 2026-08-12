# TestNG/JUnit Lifecycle & Forked-JVM Properties

## SHAFT's hook points
SHAFT integrates via TestNG listeners (`ITestListener`, `ISuiteListener`,
`IInvokedMethodListener`, registered through `ServiceLoader` /
`META-INF/services`) and JUnit 5 extensions — driver lifecycle, Allure
wiring, and reporting all hang off these. Listener exceptions can silently
kill reporting for the whole run; listeners must be defensive.

## Failure-type semantics (real SHAFT fact)
`Actions.report()` wraps element-action failures in plain
`RuntimeException`; `AssertionError` is thrown only by the separate
validation/accessibility APIs. A test expecting `AssertionError` from an
element action is wrong by construction — verify with grep which path
throws what before "fixing" the production side (PR #3408 corrected a
years-wrong test expectation).

## Properties precedence (the #1 trap)
SHAFT auto-scaffolds `src/main/resources/properties/default/*` and reads
`custom.properties`; a committed or auto-created `custom.properties`
SILENTLY OVERRIDES `-D` system properties for some keys. Passing
`-DheadlessExecution=true` is not reliable when no custom.properties
pre-exists or when one sets `headlessExecution=false` — pre-seed a
local-only gitignored custom.properties for deterministic headless runs
(repo gotcha; bit PR #3344 and #3408 verification).

Forked-JVM double hop: Surefire forks the test JVM (`argLine`, systemPropertyVariables),
and SHAFT features that fork AGAIN (replay validation subprocess) must
explicitly re-force critical properties — `GeneratedTestValidator.replay()`
once forwarded the outer JVM's own `headlessExecution` instead of forcing
`true`, crashing only on display-less Linux CI (PR #3344).

## Execution scoping (guard-enforced here)
- Never unscoped `mvn test` on the reactor; one bad fork can crash the JVM
  across all modules. Use `-Dtest=<Class>` or `-pl <module>` WITHOUT `-am`.
- Every potentially-browser-reaching run gets `-DheadlessExecution=true`.
- Running SHAFT tests from the repo root auto-creates `src/main/resources/...`
  scaffolding at the CWD — stray untracked `src/` at reactor root is
  generated garbage, safe to delete.
- Parallel TestNG (`parallel="methods|classes"`) requires everything
  ThreadLocal (SHAFT drivers are); a static mutable field in test scope is a
  flake factory.
