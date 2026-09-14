# Prefer CLI over MCP

Permanent. One owner per job. When a CLI exists and is configured, do not
call the equivalent MCP. MCP schemas tax every turn; CLIs are in training
data and compose with `jq` / `rg`.

## GitHub

Use `gh` when it exists and authenticates (`gh auth status`). GitHub MCP is
never in the default catalog and must not be added to project `.mcp.json`.
Do not call GitHub MCP while `gh` is healthy.

## Graphify

Use the Graphify CLI only. A Graphify MCP server is a uniqueness conflict.

## Docs lookup

Prefer the Context7 CLI (`npx ctx7@latest`) when that is the configured
docs path. Do not load a user-home `find-docs` skill.

## User-home skills

Do not install colliding skills under `~/.claude/skills`, `~/.agents/skills`,
`~/.codex/skills`, or `~/.grok/skills`. The overlay under `.chaos-engine/`
is the only honored skill tree.

See [eliminate-waste](eliminate-waste.md) and [no-proxy](no-proxy.md).
