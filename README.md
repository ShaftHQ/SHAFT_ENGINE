<div align="center">

<picture>
  <source media="(prefers-color-scheme: dark)" srcset="shaft-engine/src/main/resources/images/shaft_white.png">
  <img src="shaft-engine/src/main/resources/images/shaft_standard.png" alt="SHAFT logo" width="180">
</picture>

# SHAFT

Java 25 automation framework for Web, Mobile, API, CLI, and Database testing.

[![Maven Central](https://img.shields.io/maven-central/v/io.github.shafthq/shaft-engine?style=flat-square&logo=apachemaven)](https://central.sonatype.com/artifact/io.github.shafthq/shaft-engine)
[![Build](https://img.shields.io/github/actions/workflow/status/ShaftHQ/SHAFT_ENGINE/e2eTests.yml?branch=main&style=flat-square&label=tests)](https://github.com/ShaftHQ/SHAFT_ENGINE/actions/workflows/e2eTests.yml)
[![Docs](https://img.shields.io/badge/docs-live-5b4bff?style=flat-square)](https://shafthq.github.io/docs/start/overview)

**[Open the User Guide](https://shafthq.github.io/docs/start/overview)**

</div>

SHAFT is a Maven-published Java automation framework that keeps the common test
automation plumbing in one place: drivers, synchronization, assertions,
configuration, test data, reporting, evidence, and optional agent-assisted
workflows.

## Why SHAFT

- Java and Maven first, with TestNG, JUnit 5, and Cucumber integration.
- One workflow for Web, Mobile, API, CLI, Database, accessibility, reporting,
  and evidence.
- Configuration-first defaults for local, grid, cloud, and CI execution.
- Optional modules for BrowserStack, Capture, Doctor, Heal, MCP, AI, video, and
  visual testing.

## How It Compares

| Solution | Best fit | SHAFT difference |
|---|---|---|
| Playwright | Modern browser automation with a rich test runner | SHAFT is Java/Maven-first and covers browser, mobile, API, CLI, Database, reporting, and evidence together. |
| Selenide | Concise Java UI tests on Selenium WebDriver | SHAFT adds broader test surfaces, configuration, artifacts, and reports around WebDriver-style automation. |
| WebdriverIO | Node.js browser and mobile automation | SHAFT fits Java teams that want cross-surface automation without adopting a Node.js test stack. |
| Cypress | Browser-focused E2E and component testing | SHAFT fits regression suites that extend beyond frontend browser flows. |
| Selenium, Appium, REST Assured | Focused browser, mobile, and API building blocks | SHAFT packages these concerns into a higher-level framework with synchronized actions and evidence. |
| Karate | Unified API, UI, performance, and mocks in one syntax | SHAFT keeps tests in Java and emphasizes framework reuse, configuration, and reporting. |

Use SHAFT when the suite needs to cover multiple test surfaces with consistent
Java APIs, repeatable configuration, and useful run artifacts. Use a narrower
tool when the project only needs that tool's specific strength.

## Contributing

Read [CONTRIBUTING.md](CONTRIBUTING.md) for local setup, validation, and pull
request guidance.

MIT licensed. See [LICENSE](LICENSE).
