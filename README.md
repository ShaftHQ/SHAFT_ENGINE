<div align="center">

<picture>
  <source media="(prefers-color-scheme: dark)" srcset="shaft-engine/src/main/resources/images/shaft_white.png">
  <img src="shaft-engine/src/main/resources/images/shaft_standard.png" alt="SHAFT logo" width="180">
</picture>

# SHAFT

**Write clearer Java tests. Run them anywhere. Keep the evidence.**

One automation framework for Web, Mobile, API, CLI, and Database testing—
with synchronized actions, readable assertions, rich reports, and optional
agent-assisted workflows.

[![Maven Central](https://img.shields.io/maven-central/v/io.github.shafthq/shaft-engine?style=for-the-badge&logo=apachemaven)](https://central.sonatype.com/artifact/io.github.shafthq/shaft-engine)
[![Build](https://img.shields.io/github/actions/workflow/status/ShaftHQ/SHAFT_ENGINE/pr-gate.yml?branch=main&style=for-the-badge&label=build)](https://github.com/ShaftHQ/SHAFT_ENGINE/actions/workflows/pr-gate.yml)
[![Stars](https://img.shields.io/github/stars/ShaftHQ/SHAFT_ENGINE?style=for-the-badge&logo=github)](https://github.com/ShaftHQ/SHAFT_ENGINE)
[![Docs](https://img.shields.io/badge/user_guide-live-006ec0?style=for-the-badge)](https://shafthq.github.io/docs/start/overview)

**[Try SHAFT](https://shafthq.github.io/project-generator)** ·
**[Read the guide](https://shafthq.github.io/docs/start/overview)** ·
**[Star the repo ⭐](https://github.com/ShaftHQ/SHAFT_ENGINE)**

</div>

SHAFT removes the repeated plumbing around drivers, waits, assertions,
configuration, test data, screenshots, logs, and Allure evidence. Start with
the lean `shaft-engine` artifact, then add Capture, Doctor, Heal, MCP, visual,
video, cloud, or desktop-image modules only when a project needs them.

## Get useful evidence in one run

1. [Generate a ready-to-run Maven project](https://shafthq.github.io/project-generator).
2. Extract it and run:

   ```shell
   mvn test
   ```

3. Confirm the sample passes, then inspect the screenshots, logs, and Allure
   results under the generated project output.

The [quick-start guide](https://shafthq.github.io/docs/start/quick-start) covers
new projects, safe upgrades, manual setup, and CI handoff.

## One readable API, broad coverage

| Need | SHAFT gives you |
|---|---|
| Web and mobile UI | Managed Selenium/Appium drivers, synchronized actions, locator builders, screenshots, and accessibility evidence. |
| API and services | REST and GraphQL requests, responses, assertions, and reporting in the same test flow. |
| System workflows | Database and CLI actions for end-to-end checks beyond the browser. |
| Trustworthy results | Hard and soft assertions, structured logs, attachments, failure evidence, and Allure reports. |
| Scale and portability | Configuration-first local, Grid, BrowserStack, LambdaTest, TestNG, JUnit, and Cucumber execution. |
| Optional intelligence | Capture, Doctor, deterministic Heal, MCP/CLI tools, provider adapters, and an IntelliJ IDEA plugin. |

SHAFT is a Java 25 Maven reactor. Consumers can use the BOM to keep optional
modules aligned; the [module guide](https://shafthq.github.io/docs/features/modules)
maps every published artifact to its intended capability.

## Documentation that stays current

- Start: [overview](https://shafthq.github.io/docs/start/overview),
  [quick start](https://shafthq.github.io/docs/start/quick-start),
  [installation](https://shafthq.github.io/docs/start/installation), and
  [upgrade](https://shafthq.github.io/docs/start/upgrade).
- Test: [web](https://shafthq.github.io/docs/testing/web),
  [mobile](https://shafthq.github.io/docs/testing/mobile), and
  [API](https://shafthq.github.io/docs/testing/api).
- Work with agents: [skills](https://shafthq.github.io/docs/agentic/skills),
  [IntelliJ IDEA](https://shafthq.github.io/docs/agentic/intellij),
  [MCP](https://shafthq.github.io/docs/agentic/mcp),
  [Doctor](https://shafthq.github.io/docs/agentic/doctor), and
  [Heal](https://shafthq.github.io/docs/agentic/heal).
- Explore: [features and modules](https://shafthq.github.io/docs/features/modules),
  [reporting](https://shafthq.github.io/docs/features/reporting), and
  [release notes](https://github.com/ShaftHQ/SHAFT_ENGINE/releases).

## Agent skills, without the guesswork

SHAFT ships 30 first-party skills for planning, authoring, running, diagnosing,
and reporting tests. Start with `$shaft-developer`; it routes the immediate job
to one focused specialist. The
[agent-skills guide](https://shafthq.github.io/docs/agentic/skills) owns the
supported install routes, client-native directories, upgrade behavior, and
verification steps.

Exact MCP names and CLI syntax are generated from source in
[`shaft-mcp-tools.md`](shaft-skills/references/shaft-mcp-tools.md) and
[`shaft-cli-commands.md`](shaft-skills/references/shaft-cli-commands.md).

## Join the project

- Found a bug or have an idea? [Open an issue](https://github.com/ShaftHQ/SHAFT_ENGINE/issues).
- Want to contribute? Read [CONTRIBUTING.md](CONTRIBUTING.md) and our
  [Code of Conduct](CODE_OF_CONDUCT.md).
- Found a vulnerability? Follow [SECURITY.md](SECURITY.md) and report it
  privately.
- Like the direction? [Star SHAFT](https://github.com/ShaftHQ/SHAFT_ENGINE)
  so more automation engineers can discover it.

## Support that keeps SHAFT moving

BrowserStack, LambdaTest, Applitools, and JetBrains have provided tooling or
open-source program support. They are project supporters, not represented as
financial sponsors or endorsements. See the
[support and adoption notes](https://shafthq.github.io/docs/features/modules#partners).

SHAFT is free and MIT licensed. If it saves your team time, you can fund
ongoing maintenance, documentation, and public infrastructure through
[GitHub Sponsors](https://github.com/sponsors/MohabMohie).

MIT licensed — see [LICENSE](LICENSE).
