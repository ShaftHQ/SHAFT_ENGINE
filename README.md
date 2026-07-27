<div align="center">

<picture>
  <source media="(prefers-color-scheme: dark)" srcset="shaft-engine/src/main/resources/images/shaft_white.png">
  <img src="shaft-engine/src/main/resources/images/shaft_standard.png" alt="SHAFT logo" width="180">
</picture>

# SHAFT

Java 25 automation framework for Web, Mobile, API, CLI, and Database testing.

[![Maven Central](https://img.shields.io/maven-central/v/io.github.shafthq/shaft-engine?style=flat-square&logo=apachemaven)](https://central.sonatype.com/artifact/io.github.shafthq/shaft-engine)
[![PR Gate](https://img.shields.io/github/actions/workflow/status/ShaftHQ/SHAFT_ENGINE/pr-gate.yml?branch=main&style=flat-square&label=build)](https://github.com/ShaftHQ/SHAFT_ENGINE/actions/workflows/pr-gate.yml)
[![Docs](https://img.shields.io/badge/docs-live-5b4bff?style=flat-square)](https://shafthq.github.io/docs/start/overview)

**[Open the User Guide](https://shafthq.github.io/docs/start/overview)**

</div>

SHAFT is a Maven-published Java automation framework that keeps the common test
automation plumbing in one place: drivers, synchronization, assertions,
configuration, test data, reporting, evidence, and optional agent-assisted
workflows.

## What You Get

| Area | Built in |
|---|---|
| UI automation | Browser and mobile driver management, synchronized actions, screenshots, and logs. |
| Service testing | REST and GraphQL API workflows with request, response, and assertion support. |
| System coverage | CLI and Database actions for end-to-end validation beyond the browser. |
| Test design | Assertions, validations, test data handling, and configuration overrides. |
| Reporting | Allure-ready evidence, attachments, execution logs, and accessibility artifacts. |
| Extensions | Opt-in modules for cloud execution, capture, diagnostics, healing, MCP, AI, video, and visual checks, plus an IntelliJ IDEA plugin. |

Java and Maven first, with TestNG, JUnit, and Cucumber integration, and
configuration-first defaults for local, grid, cloud, and CI execution. The
IntelliJ IDEA plugin is the front door for agent-assisted work: plan from the
SHAFT tool window, reuse existing code, review generated blocks, verify locally.

## Documentation

- Start: [overview](https://shafthq.github.io/docs/start/overview), [installation](https://shafthq.github.io/docs/start/installation), [upgrade](https://shafthq.github.io/docs/start/upgrade).
- Testing: [web](https://shafthq.github.io/docs/testing/web), [mobile](https://shafthq.github.io/docs/testing/mobile), [API](https://shafthq.github.io/docs/testing/api).
- Agentic workflows: [IntelliJ IDEA plugin](https://shafthq.github.io/docs/agentic/intellij), [MCP](https://shafthq.github.io/docs/agentic/mcp), [Doctor](https://shafthq.github.io/docs/agentic/doctor), [Heal](https://shafthq.github.io/docs/agentic/heal).
- [Modular-era feature catalog](modular-era-feature-catalog.md) — which optional
  module or command to adopt, with screenshots.

## Contributing

- [CONTRIBUTING.md](CONTRIBUTING.md) — local setup, validation, and pull requests.
- [CODE_OF_CONDUCT.md](CODE_OF_CONDUCT.md) — community expectations.
- [SECURITY.md](SECURITY.md) — report a vulnerability privately, never as a
  public issue.

MIT licensed. See [LICENSE](LICENSE).
