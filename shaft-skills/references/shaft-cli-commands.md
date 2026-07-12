# SHAFT CLI Command Reference

shaft-cli is a command-line interface to the same shaft-mcp tool set. When a
`shaft-cli` launcher is installed, prefer it over raw MCP tool calls — a plain
shell command costs fewer agent tokens than an MCP schema load. Fall back to
the `shaft-mcp:<tool>` MCP calls in `shaft-mcp-tools.md` when shaft-cli is not
installed or a command fails.

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
prefer the cached `shaft-mcp-tools.md` to save tokens.

## Curated aliases

Shortcuts over `call`, same options:

- `shaft-cli browser navigate|screenshot|dom|url` (session required)
- `shaft-cli element click|type|hover` (session required)
- `shaft-cli capture start|stop|status|code` (session required)
- `shaft-cli guide search` (stateless)
- `shaft-cli doctor analyze|suggest` (stateless)
