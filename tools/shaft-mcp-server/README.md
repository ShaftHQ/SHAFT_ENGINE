# SHAFT MCP Server Starter

This is a community starter for a SHAFT Engine Model Context Protocol (MCP)
server. It is intentionally separate from the main `SHAFT_ENGINE` Maven
artifact so maintainers can review and evolve the interface without adding MCP
runtime dependencies to every SHAFT user.

## Goal

Playwright MCP is excellent for live browser control. This starter focuses on
the SHAFT layer instead:

- inspect a SHAFT workspace
- run allow-listed Maven validations
- summarize Allure result JSON
- list/read SHAFT report artifacts
- expose repository guidance as MCP resources

That makes the MCP server useful for AI agents that need to behave like QA
automation assistants rather than only browser operators.

## Current Status

This starter is a tested MVP.

It can run SHAFT/Maven validation profiles and analyze generated reports. It
does not yet start Selenium Grid, Appium, emulators, BrowserStack, LambdaTest,
or desktop UI automation sessions by itself. Those are mapped below as roadmap
items.

## Tools

| Tool | Purpose |
| --- | --- |
| `shaft_project_info` | Read SHAFT project metadata and report inventory without running tests |
| `shaft_run_validation` | Run allow-listed Maven validation profiles |
| `shaft_extract_allure_failures` | Summarize Allure result JSON statuses and failures |
| `shaft_list_report_artifacts` | List generated SHAFT report artifacts |
| `shaft_read_report_artifact` | Read a text artifact from known report directories |

## Resources

| Resource | Purpose |
| --- | --- |
| `shaft://guidance/agents` | Repository-specific agent guidance from `AGENTS.md` |
| `shaft://project/pom` | Project `pom.xml` for dependency/version context |

## Capability Map

| Area | Works in this MVP | Future end-to-end MCP tools |
| --- | --- | --- |
| Project discovery | Yes: `shaft_project_info` | Add richer dependency, workflow, and environment checks |
| Maven validation | Yes: `shaft_run_validation` allow-listed profiles | Add named suite/profile discovery |
| Web/Selenium tests | Runs existing Maven tests if the local browser/grid is already configured | `shaft_start_selenium_grid`, `shaft_run_web_suite`, `shaft_stop_selenium_grid` |
| API tests | Runs existing Maven/API tests | `shaft_run_api_suite`, sanitized request/response report summaries |
| Android/Appium | Runs existing tests only if device/Appium prerequisites already exist | `shaft_list_android_devices`, `shaft_start_appium`, `shaft_run_android_suite` |
| iOS/Appium | Runs existing tests only if macOS/device/Appium prerequisites already exist | `shaft_list_ios_devices`, `shaft_run_ios_suite` |
| Desktop UI | Not direct. SHAFT is not primarily a desktop UI automation framework | Integrate with a dedicated desktop MCP or future Appium Windows/macOS path |
| CLI tests | Runs existing Maven tests; no generic shell tool is exposed | Add narrow, safe SHAFT CLI workflow helpers |
| DB tests | Runs existing Maven tests if DB is already available | Add DB service readiness/report helpers without exposing credentials |
| Cloud providers | Runs existing tests if credentials/config are already present | `shaft_run_browserstack_suite`, `shaft_run_lambdatest_suite` with strict redaction |
| Reporting | Yes: Allure/Surefire/report artifact inventory and summaries | `shaft_compare_surefire_allure`, `shaft_generate_failure_brief` |
| GitHub reporting | Not included in the server | Add optional private-safe issue/advisory draft helpers after maintainer approval |

## Install and Run Locally

From this directory:

```bash
npm install
npm run smoke
```

Example MCP client configuration:

```json
{
  "mcpServers": {
    "shaft": {
      "command": "node",
      "args": ["/absolute/path/to/SHAFT_ENGINE/tools/shaft-mcp-server/src/index.js"],
      "env": {
        "SHAFT_WORKSPACE": "/absolute/path/to/SHAFT_ENGINE"
      }
    }
  }
}
```

After configuration, restart the MCP client and ask the agent:

```text
Use SHAFT MCP to inspect project info.
```

or:

```text
Use SHAFT MCP to run validate and summarize report artifacts.
```

## Install Options

### Option A: Run from a SHAFT checkout

Use this while the server lives inside the SHAFT repository:

```bash
git clone https://github.com/ShaftHQ/SHAFT_ENGINE.git
cd SHAFT_ENGINE/tools/shaft-mcp-server
npm ci
npm run smoke
```

Configure your MCP client to run:

```bash
node /absolute/path/to/SHAFT_ENGINE/tools/shaft-mcp-server/src/index.js
```

and set:

```bash
SHAFT_WORKSPACE=/absolute/path/to/SHAFT_ENGINE
```

### Option B: Install as a local command

From `tools/shaft-mcp-server`:

```bash
npm install -g .
shaft-mcp
```

Then configure your MCP client with command `shaft-mcp` and the
`SHAFT_WORKSPACE` environment variable.

### Option C: Future npm package

If maintainers publish this package to npm, MCP clients can use:

```json
{
  "command": "npx",
  "args": ["-y", "@shafthq/shaft-mcp-server"],
  "env": {
    "SHAFT_WORKSPACE": "/absolute/path/to/SHAFT_ENGINE"
  }
}
```

## Client Configuration

### Codex

Add a server entry to `~/.codex/config.toml`:

```toml
[features]
rmcp_client = true

[mcp_servers.shaft]
command = "node"
args = ["C:\\Users\\you\\path\\to\\SHAFT_ENGINE\\tools\\shaft-mcp-server\\src\\index.js"]
startup_timeout_sec = 120
tool_timeout_sec = 600

[mcp_servers.shaft.env]
SHAFT_WORKSPACE = "C:\\Users\\you\\path\\to\\SHAFT_ENGINE"
```

Restart Codex after editing the config.

### Claude Desktop

Add to `claude_desktop_config.json`:

```json
{
  "mcpServers": {
    "shaft": {
      "command": "node",
      "args": [
        "C:/Users/you/path/to/SHAFT_ENGINE/tools/shaft-mcp-server/src/index.js"
      ],
      "env": {
        "SHAFT_WORKSPACE": "C:/Users/you/path/to/SHAFT_ENGINE"
      }
    }
  }
}
```

Restart Claude Desktop after editing the config.

### Claude Code and Other MCP Clients

Use the same stdio launch shape:

```json
{
  "mcpServers": {
    "shaft": {
      "command": "node",
      "args": ["/absolute/path/to/SHAFT_ENGINE/tools/shaft-mcp-server/src/index.js"],
      "env": {
        "SHAFT_WORKSPACE": "/absolute/path/to/SHAFT_ENGINE"
      }
    }
  }
}
```

For clients that support a command-line MCP registration flow, register the
same command, args, and environment variable. The server uses stdio transport,
so no port or HTTP endpoint is required.

## Safety Model

The starter deliberately avoids a generic shell tool.

- Maven execution is allow-listed by validation profile.
- Commands are executed with argument arrays, not shell-string interpolation.
- Report reading is restricted to known SHAFT report directories.
- Output is truncated and redacts common secret patterns before returning it to
  the agent.
- `SHAFT_WORKSPACE` must point at a checkout whose `pom.xml` contains the
  `SHAFT_ENGINE` artifact id.

## Recommended Roadmap

1. Keep this server focused on SHAFT-level workflow first: run tests, inspect
   reports, explain failures.
2. Add browser/session tools only after a structured snapshot design exists.
   Otherwise Playwright MCP remains the better browser-control backend.
3. Add a Java-side adapter only if maintainers want direct access to SHAFT
   runtime APIs instead of Maven/report orchestration.
4. Add private-safe GitHub issue/advisory draft helpers only after the project
   has a central redaction policy.
