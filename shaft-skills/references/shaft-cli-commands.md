# SHAFT CLI Command Reference

shaft-cli is a command-line interface to the same shaft-mcp tool set. When a
`shaft-cli` launcher is installed, prefer it over raw MCP tool calls — a plain
shell command costs fewer agent tokens than an MCP schema load. Fall back to
the `shaft-mcp:<tool>` MCP calls in `shaft-mcp-tools.md` when shaft-cli is not
installed or a command fails. For stateful work (recordings, live browser or
mobile drivers) start a `shaft-cli session` first or stay on a persistent MCP
client — a recording must never belong to a one-shot process (see Sessions
below).

Install: pass `--install-shaft-cli` to the shaft-mcp installer (see
`https://shafthq.github.io/docs/agentic/cli`). The launcher lands under the
platform application-data root, e.g.
`%LOCALAPPDATA%\ShaftHQ\shaft-cli\versions\<version>\shaft-cli.cmd` on Windows.

## Tool mapping

Every tool in `shaft-mcp-tools.md` is callable by its exact name, unprefixed:

```
shaft-cli call <TOOL> [key=value ...] [--args '<json>'] [--json] [--stdio-ok]
```

- `key=value` values are coerced (`true`/`false` → boolean, numbers → numeric,
  `{...}`/`[...]` → JSON, otherwise string); `key=value` overrides `--args` keys.
- `--json` prints the raw JSON-RPC result instead of rendered text.
- Exit codes: 0 success; 1 tool or transport error (or `--json` with
  `isError`); 2 unknown alias action.

## Sessions and state

One-shot commands spawn an ephemeral shaft-mcp child per call, so state
(browser, driver, capture) does not survive between them. For stateful tools
start a session first:

```
shaft-cli session start    # persistent daemon; state survives across commands
shaft-cli session status
shaft-cli session stop
```

Stateful commands fail fast without a session unless `--stdio-ok` is passed.
Stateless tools (guide search, doctor analysis) need no session.

## Discovery

`shaft-cli tools [--json]` lists every tool with its description at runtime.
`shaft-cli tools --cached [--json]` reads the bundled generated copy of the
canonical `tool-index.json` with no MCP server or one-shot child process. Its
names, descriptions, parameter schemas, and curated metadata exactly match the
shaft-mcp version shipped with that CLI; use the live command only when the
running server may be a different version.

## Codegen (deterministic, no MCP session)

`shaft-cli codegen [--session <path>] [any `CaptureCli generate` flag]`
generates SHAFT test code directly from a capture recording file — no MCP
server, no session, no `--stdio-ok` (Decision 6: single-session actions like
codegen prefer the deterministic CLI path over MCP). `--session` defaults to
the newest `*.json` file under `./recordings` when omitted, mirroring
`capture_generate_replay`'s zero-arg default, so a bare `shaft-cli codegen`
works right after a `capture start`/`capture stop` pair. Every other
`CaptureCli generate` flag (`--backend webdriver|playwright`, `--output-dir`,
`--package`, `--class-name`, `--overwrite`, `--replay`, ...) passes through
unchanged; exit code 2 means no `--session` was given and none could be
inferred.

## Local infrastructure setup (direct, no MCP session)

`shaft-cli setup` exposes the release-coupled setup catalog and lifecycle. It
defaults to the read-only `EXTERNAL` mode. Managed mutations require a plan
file plus its exact printed digest; a stale or edited plan is rejected before
the cache or durable-data roots are created.

```text
shaft-cli setup catalog|profiles [--json]
shaft-cli setup doctor|status|verify --profile REPORTING [--json]
shaft-cli setup plan --profile REPORTING --mode MANAGED --output <absolute-plan.json> [policy options] [--json]
shaft-cli setup install|apply|update --plan <absolute-plan.json> --approve <sha256:digest> [--accept-license <id>] [policy options] [--json]
shaft-cli setup start|stop --profile <profile>
shaft-cli setup logs --profile REPORTING
```

The first managed provider installs verified, SHAFT-owned portable Node and
Allure 3 artifacts for the `REPORTING` profile. `start` and `stop` return
unsupported for profiles without an owned service; they never adopt or stop
an unknown process. Exit codes are 0 for ready/success, 2 for invalid input or
approval, 3 for missing/degraded readiness, 4 for unsupported providers, and
5 for execution or integrity failures. `--cache-root` and `--data-root` are
paired advanced overrides and must both be absolute.

Plan and install must receive the same policy options because those values are
bound into the approved digest: `--offline`, `--auto-start`,
`--prefer-system-tools=true|false`, `--reuse-owned-processes=true|false`,
`--startup-timeout <ISO-8601 duration>`, and
`--shutdown-timeout <ISO-8601 duration>`.

The equivalent Java surface is `SHAFT.Infrastructure`. Its no-argument
read-only methods use `SHAFT.Properties.infrastructure`; `install` and `start`
require the exact `SetupPlan` and `SetupApproval`. Defaults remain
`EXTERNAL`, `offline=false`, and `autoStart=false`. An explicit remote
execution address wins for web, Grid, and mobile endpoint profiles.

## Curated aliases

Shortcuts over `call`, same options:

- `shaft-cli browser navigate|screenshot|dom|url` (session required)
- `shaft-cli element click|type|hover` (session required)
- `shaft-cli capture start|stop|status|code|step-delete|step-reorder` (session required)
- `shaft-cli guide search` (stateless)
- `shaft-cli doctor analyze|suggest|local-ai-status` (stateless)
