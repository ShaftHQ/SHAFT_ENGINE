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

## Agent Skills

SHAFT ships a first-party skill pack in [`shaft-skills/`](shaft-skills) that
routes the full software-testing lifecycle through 30 focused skills. Start
every SHAFT task with `$shaft-developer`; it selects one lifecycle,
implementation, or MCP/CLI specialist for the immediate output.

Install as a Claude Code plugin (ships the full pack, including the shared
`references/` lookups):

```text
/plugin marketplace add ShaftHQ/SHAFT_ENGINE
/plugin install shaft-skills@shafthq
```

From a repository checkout, the SHAFT installer copies the complete pack and
shared generated catalogs to every agent-native project directory:

```shell
python scripts/mcp/install_shaft_mcp.py --install-shaft-skills --json
```

The [skills CLI](https://github.com/vercel-labs/skills) can install all 30
skill directories directly:

```shell
npx skills add https://github.com/ShaftHQ/SHAFT_ENGINE/tree/main/shaft-skills --skill '*' --agent '*' -y
```

Its per-skill install omits the shared generated MCP/CLI catalogs, so prefer
the Claude plugin or SHAFT installer when those lookups are needed. With an
installer client selector, `--install-shaft-skills` targets `.agents/skills`,
`.claude/skills`, or `.github/skills` for that client. For a multi-host
project, call
`shaft_project_init_agents(loop="all", targetDirectory=".", overwrite=false)`
through a connected SHAFT MCP client; it creates or updates real host
instruction files while preserving user prose.

On upgrade, the SHAFT installer removes only its seven verifiably owned retired
skill folders; it leaves other user skills and files in the native directories intact.
It refuses linked or junctioned native skill directories using Windows
reparse-point checks, so an install cannot write outside the selected project.

Use direct MCP tools for interactive stateful work, for example
`shaft_guide_search` with `{"query":"page object model","maxResults":3}`.
Use SHAFT CLI for repeatable one-shot work:

```shell
shaft-cli guide search query="browser assertions" maxResults=3 --json
shaft-cli tools --cached
```

Exact current MCP names and CLI syntax live in the generated
[`shaft-mcp-tools.md`](shaft-skills/references/shaft-mcp-tools.md) and
[`shaft-cli-commands.md`](shaft-skills/references/shaft-cli-commands.md)
catalogs; skills never guess tool names or parameters.

## Contributing

- [CONTRIBUTING.md](CONTRIBUTING.md) — local setup, validation, and pull requests.
- [CODE_OF_CONDUCT.md](CODE_OF_CONDUCT.md) — community expectations.
- [SECURITY.md](SECURITY.md) — report a vulnerability privately, never as a
  public issue.

MIT licensed. See [LICENSE](LICENSE).
