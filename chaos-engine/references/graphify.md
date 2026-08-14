# Graphify

Graphify is the project-local structural retrieval CLI. It is intentionally a
CLI rather than an MCP server. Run it only through the installed launcher so
the pinned project runtime is used on every host.

## Retrieve first

1. If `graphify-out/graph.json` exists, run one bounded query.
2. Verify every returned path and claim against current project files.
3. If the graph is absent, stale, malformed, or contradicts live files, declare
   degraded mode and continue with native Memory, MemPalace, and targeted `rg`.
4. Never treat an absent MCP entry as a Graphify failure.

```text
python .chaos-engine/tool.py graphify query "<bounded structural question>" --graph graphify-out/graph.json
```

## Refresh

Refresh only from the active primary checkout. Do not refresh from a linked
worktree, while another refresh is active, or when generated state belongs to
another revision.

```text
python .chaos-engine/tool.py graphify update .
python .chaos-engine/tool.py graphify diagnose multigraph --graph graphify-out/graph.json --json
```

The refresh writes generated data under `graphify-out/`; the installed
`.gitignore` keeps it untracked. A failed update never authorizes deleting or
overwriting an existing graph. Use `--force` only after a verified refactor
that intentionally removed nodes.

Useful bounded reads:

```text
python .chaos-engine/tool.py graphify explain "<symbol>" --graph graphify-out/graph.json
python .chaos-engine/tool.py graphify affected "<symbol>" --depth 2 --graph graphify-out/graph.json
python .chaos-engine/tool.py graphify path "<source>" "<target>" --graph graphify-out/graph.json
```
