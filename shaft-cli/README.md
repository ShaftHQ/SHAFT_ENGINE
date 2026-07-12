# shaft-cli

shaft-cli is an MCP client and command-line interface to shaft-mcp. It brings all 147 shaft-mcp tools to the command line with zero tool-logic duplication, operating in two modes: stateless one-shot commands and persistent session mode for stateful tools.

## Modes

**One-shot mode** (default): Each command spawns an ephemeral shaft-mcp child process over stdio, runs a single tool call, and exits. Suitable for stateless tools like guide search and doctor analysis.

**Session mode**: `shaft-cli session start` launches a persistent shaft-mcp daemon (Spring `http` profile on a localhost port) and records the session endpoint to `~/.shaft/cli-session.json`. Subsequent commands connect over HTTP, preserving browser/device state across invocations. Commands needing live state (`browser`, `element`, `capture`) fail fast when no session is running, unless `--stdio-ok` is passed.

Routing: If a live session exists, commands use HTTP; otherwise a one-shot stdio child is spawned.

## Discovery

shaft-mcp is located in this order:
1. `SHAFT_MCP_JAR` environment variable
2. The installer's versions directory
3. Sibling `../shaft-mcp/target` (dev checkout)

Note: Users typically install shaft-mcp via the installer; it is a prerequisite.

## Commands

All commands support `-h`/`--help`; the root supports `-V`/`--version`.

**`shaft-cli tools [--json]`**
List all available tools (name and first sentence of description). `--json` outputs the raw `tools/list` result.

**`shaft-cli call <TOOL> [key=value ...] [--args '<json>'] [--json] [--stdio-ok]`**
Invoke any tool by name. Arguments come from repeated `key=value` pairs (values are coerced: `true`/`false` → boolean, integers/decimals → numbers, `{...}`/`[...]` → JSON, otherwise string) and/or `--args '{"k":"v"}'` (key=value overrides keys from `--args`). `--json` outputs the raw JSON-RPC result instead of rendered text.

**`shaft-cli session start | status | stop`**
Manage the daemon. `start` prints `shaft-cli session started on port <port> (pid <pid>).`; `status` prints `running — port <port>, pid <pid>, started <iso-8601>`.

**Curated aliases** (same options as `call`):
- `shaft-cli browser navigate|screenshot|dom|url` (session required)
- `shaft-cli element click|type|hover` (session required)
- `shaft-cli capture start|stop|status|code` (session required)
- `shaft-cli guide search` (stateless)
- `shaft-cli doctor analyze|suggest` (stateless)

## Examples

```
java -jar shaft-cli/target/shaft-cli-<version>.jar tools
# trace_latest — returns recent persisted SHAFT trace indexes from target/shaft-traces
# [... 146 more lines]

shaft-cli session start
# shaft-cli session started on port 12345 (pid 9876).

shaft-cli session status
# running — port 12345, pid 9876, started 2025-12-20T14:30:45Z

shaft-cli call shaft_guide_search query='click element' maxResults=1
# [JSON guide-search result]

shaft-cli guide search query='click element' --stdio-ok
# [guide search works with no session]
```

## Build and Run

**Prerequisites**: Java 25 (inherited from repo toolchain).

**Build the fat jar**:
```
mvn -pl shaft-cli package -DskipTests
```
Output: `shaft-cli/target/shaft-cli-<version>.jar`

**Run tests**:
```
mvn -pl shaft-cli test
```

**Run**:
```
java -jar shaft-cli/target/shaft-cli-<version>.jar <command>
```

## Dependencies

Only picocli and Jackson. No compile-time dependency on shaft-mcp or any shaft module; shaft-cli communicates with shaft-mcp purely over the Model Context Protocol.

## Exit Codes

- **0**: Success
- **1**: Tool error, transport error, or `--json` result with `isError`
- **2**: Unknown alias action

## Scope

Installer `--cli` flag, docs-site page, shell completion, and native binaries are out of scope for the MVP.
