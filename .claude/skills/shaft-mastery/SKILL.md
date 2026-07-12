---
name: shaft-mastery
description: >
  Ten distilled expert domains for working on SHAFT_ENGINE (Java test-automation
  framework). Load the ONE matching chapter from references/ before deep work on:
  Selenium/WebDriver BiDi or the recorder (selenium-bidi), Allure report internals
  (allure-internals), Appium/mobile toolchains (appium-mobile), Maven Central
  release engineering (maven-release), TestNG/JUnit listeners and forked-JVM
  properties (testng-lifecycle), IntelliJ Platform plugin dev (intellij-plugin),
  MCP protocol engineering (mcp-protocol), GitHub Actions CI forensics
  (ci-forensics), flaky-test stabilization and wait strategies (wait-strategies),
  or locator design and self-healing (locator-healing). Read only the chapter you
  need; each is self-contained and grounded in real SHAFT incidents.
---

# SHAFT Mastery

Ten expert domains, one chapter each, in `references/`. Rule: identify the
domain the task actually touches, read that chapter fully, skip the rest.
Chapters encode both general expertise and SHAFT-specific incident history —
the "learned the hard way" notes are real, do not re-derive them.

| Chapter | Load when the task touches |
|---|---|
| `selenium-bidi.md` | recorder, preload scripts, network capture, browser lifecycle |
| `allure-internals.md` | report generation/patching, results JSON, verdict analysis |
| `appium-mobile.md` | mobile recording/replay, emulators, Appium/WinAppDriver CI |
| `maven-release.md` | versioning, Central publishing, BOM, dependency convergence |
| `testng-lifecycle.md` | listeners, forked JVMs, properties precedence, scoped runs |
| `intellij-plugin.md` | shaft-intellij Swing UI, tool windows, Gradle/JDK, tests |
| `mcp-protocol.md` | shaft-mcp tools, stdio transport, workspace roots, clients |
| `ci-forensics.md` | red CI runs, scheduled suites, workflow YAML, sharding |
| `wait-strategies.md` | flaky tests, races, synchronization, deterministic repro |
| `locator-healing.md` | locator choice, aria/semantic selectors, healer/doctor |
