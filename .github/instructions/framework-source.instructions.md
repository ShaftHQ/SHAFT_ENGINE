---
applyTo: "**/src/main/java/**/*.java"
---

# Framework Source Rules

- Preserve public compatibility. Deprecate before removing or renaming public
  APIs; prefer overloads to changed signatures.
- Add concise JavaDoc to public classes and methods, including applicable
  `@param`, `@return`, and `@throws` tags.
- Keep user-facing APIs consistent with the `SHAFT` facade and its namespaced
  nested classes. Utility classes use a private constructor that throws
  `IllegalStateException("Utility class")`.
- Throw specific exceptions, retain useful causes, and use SHAFT
  reporting/logging utilities. Do not print to standard output or silently
  swallow failures.
- Declare configurable versions, URLs, timeouts, thresholds, and switches in
  the appropriate `com.shaft.properties.internal` interface with a default.
  Access them through `SHAFT.Properties`; do not call `System.getProperty()`
  directly in framework code.
- Use `ThreadLocalPropertiesManager` for per-thread overrides and clear cached
  values with `Properties.clearForCurrentThread()` at lifecycle boundaries.
  Document shared or thread-local state where ownership is not obvious.
- New external tools must remain batteries-included: resolve from PATH, then a
  package manager where applicable, then a controlled Maven-cache download.
  Keep the version in the properties layer.
- Merge existing vendor capability maps and keep all WebDriver extensions
  namespaced. Never restore top-level legacy capabilities such as
  `enableVideo`.
- For Allure 3, use `AllureManager.resolveAllureCommandPrefix()`. Do not add the
  removed `allure generate --clean` flag; the generated summary is at the
  report root.
- Preserve UTF-8 flags in Maven and CI configuration when touching process or
  reporting setup.
- For a bug, reproduce the defect and add a focused regression test when
  practical. Make the smallest behavior-preserving fix.
- Route release metadata work through
  [the release guard](../skills/release-dependency-guard/SKILL.md).
