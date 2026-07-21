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

`shaft-cli tools [--json]` lists every tool with its description at runtime —
prefer the cached `shaft-mcp-tools.md` to save tokens. `shaft-cli tools --cached
[--json]` instead reads a bundled offline snapshot with no MCP server involved
at all (not even a one-shot child process) — the fastest discovery path when
you don't need live server state. The snapshot is a stop-gap assembled from
the frozen 89-tool catalog (`mcp-tool-manifest.json` +
`shaft-mcp-tools.md`); it will move onto the canonical `tool-index.json`
generated for #3868 once that lands, so treat it as "recent", not
"authoritative live state".

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

## Curated aliases

Shortcuts over `call`, same options:

- `shaft-cli browser navigate|screenshot|dom|url` (session required)
- `shaft-cli element click|type|hover` (session required)
- `shaft-cli capture start|stop|status|code|step-delete|step-reorder` (session required)
- `shaft-cli guide search` (stateless)
- `shaft-cli doctor analyze|suggest` (stateless)
