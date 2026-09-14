# Prefer CLI over MCP

Permanent. One owner per job. When a CLI exists and is configured, do not
call the equivalent MCP. MCP schemas tax every turn; CLIs are in training
data and compose with `jq` / `rg`.

## GitHub

Use `gh` when it exists and authenticates (`gh auth status`). GitHub MCP is
never in the default catalog and must not be added to project `.mcp.json`.
Do not call GitHub MCP while `gh` is healthy.

Doctor/repair probes `gh auth status`:

- When healthy → strip GitHub MCP ids from user-host MCP files (Claude, Codex,
  Grok, Gemini, Copilot paths in `mcp_policy.user_mcp_paths`).
- When missing or unauthenticated → leave existing GitHub MCP in place.
- Project overlay never publishes GitHub MCP.

## Graphify

Use the Graphify CLI only. A Graphify MCP server is a uniqueness conflict.

## Docs lookup

Prefer the Context7 CLI (`npx ctx7@latest`) when that is the configured
docs path. Do not load a user-home `find-docs` skill.

## User-home skills

Do not install colliding skills under `~/.claude/skills`, `~/.agents/skills`,
`~/.codex/skills`, or `~/.grok/skills`. The overlay under `.chaos-engine/`
is the only honored skill tree.

## Grok bundled skills and session GitHub MCP

Grok product bundles (pdf, pptx, imagine, game-*) and session GitHub MCP
injection cannot be deleted from the Grok install tree. ChaosEngine does not
vendor those bundles into the overlay catalog. Where the host allows it,
doctor/repair disables user GitHub MCP when `gh` is healthy and reports the
remaining host-product limit (`GAP-GROK-BUNDLED`, issues #5780 / #5785).

## Maven Tools MCP

Git-tracked project `.mcp.json` must never embed workstation-absolute
java/jar paths. Native mode publishes
`.chaos-engine/tool.py maven-tools-mcp`, which resolves the shared
ChaosEngine cache at runtime. Docker mode may publish a portable image ref.

## Plugins

Enable only `chaos-engine`, `caveman`, and `ponytail`. Doctor heals or fails
extra enabled plugins / marketplace auto-install of non-CE plugins. Do not
rewrite `marketplace.json` without an overlay content change.

See [eliminate-waste](eliminate-waste.md) and [no-proxy](no-proxy.md).
