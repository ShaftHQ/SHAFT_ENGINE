---
applyTo: "**/src/main/java/**/*.java"
---

# Framework Source Rules

- Deprecate before removing or renaming public APIs; prefer overloads to
  changed signatures.
- Add concise JavaDoc (`@param`/`@return`/`@throws` where applicable) to
  public classes and methods.
- Keep user-facing APIs consistent with the `SHAFT` facade and its namespaced
  nested classes. Utility classes use a private constructor that throws
  `IllegalStateException("Utility class")`.
- Throw specific exceptions, retain causes, and use SHAFT reporting/logging
  utilities. Never print to standard output or swallow failures silently.
- Declare configurable versions/URLs/timeouts/thresholds/switches in the
  matching `com.shaft.properties.internal` interface with a default; access
  via `SHAFT.Properties`, never `System.getProperty()` directly.
- Use `ThreadLocalPropertiesManager` for per-thread overrides; clear with
  `Properties.clearForCurrentThread()` at lifecycle boundaries. Document
  non-obvious shared/thread-local state ownership.
- New external tools stay batteries-included: resolve from PATH, then a
  package manager where applicable, then a controlled Maven-cache download;
  keep the version in the properties layer.
- Merge existing vendor capability maps and keep all WebDriver extensions
  namespaced. Never restore top-level legacy capabilities such as
  `enableVideo`.
- For Allure 3, use `AllureManager.resolveAllureCommandPrefix()`. Do not add the
  removed `allure generate --clean` flag; the generated summary is at the
  report root.
- Preserve UTF-8 flags in Maven and CI configuration when touching process or
  reporting setup.
- For a bug, reproduce the defect (add a focused regression test when
  practical); make the smallest behavior-preserving fix.
- Route release metadata work through
  [the release guard](../skills/release-dependency-guard/SKILL.md).
