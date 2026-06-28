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

- Java and Maven first, with TestNG, JUnit, and Cucumber integration.
- One workflow for Web, Mobile, API, CLI, Database, accessibility, reporting,
  and evidence.
- Configuration-first defaults for local, grid, cloud, and CI execution.
- Optional modules for BrowserStack, Capture, Doctor, Heal, MCP, AI, video, and
  visual testing.

## Feature Overview

| Area | Built in |
|---|---|
| UI automation | Browser and mobile driver management, synchronized actions, screenshots, and logs. |
| Service testing | REST and GraphQL API workflows with request, response, and assertion support. |
| System coverage | CLI and Database actions for end-to-end validation beyond the browser. |
| Test design | Assertions, validations, test data handling, and configuration overrides. |
| Reporting | Allure-ready evidence, attachments, execution logs, and accessibility artifacts. |
| Extensions | Optional modules for cloud execution, capture, diagnostics, healing, MCP, AI, video, and visual checks. |

SHAFT is built for Java teams that want consistent APIs, repeatable
configuration, and useful run artifacts across the full automation suite.

## User Guide

- Start: [overview](https://shafthq.github.io/docs/start/overview), [installation](https://shafthq.github.io/docs/start/installation), [upgrade](https://shafthq.github.io/docs/start/upgrade).
- Testing: [web](https://shafthq.github.io/docs/testing/web), [mobile](https://shafthq.github.io/docs/testing/mobile), [API](https://shafthq.github.io/docs/testing/api).
- Agentic workflows: [MCP](https://shafthq.github.io/docs/agentic/mcp), [Doctor](https://shafthq.github.io/docs/agentic/doctor), [Heal](https://shafthq.github.io/docs/agentic/heal).

## Contributing

Read [CONTRIBUTING.md](CONTRIBUTING.md) for local setup, validation, and pull
request guidance.

MIT licensed. See [LICENSE](LICENSE).
