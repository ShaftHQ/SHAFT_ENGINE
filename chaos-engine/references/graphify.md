# Graphify

Graphify is the project-local structural retrieval CLI. It is intentionally a
CLI rather than an MCP server. Run it only through the installed launcher so
the pinned project runtime is used on every host.

## Retrieve first

The installed launcher owns this provider-neutral G1–G4 route:

- G1: Resolve `graphify-out/graph.json` from the active project and require a
  readable, nonempty graph. Never infer failure from an absent MCP entry.
- G2: After G1 succeeds, run one query bounded to the affected symbol or
  subsystem, then verify every returned path against current project files.
- G3: Run the read-only multigraph diagnostic against that same graph. Do not
  refresh, replace, or repair generated state during retrieval.
- G4: Declare degraded mode if any step lacks current verified results, and
  only after G1 through G3 have been attempted. Continue with native Memory,
  MemPalace, and targeted `rg`.

```text
python .chaos-engine/tool.py graphify query "<bounded structural question>" --graph graphify-out/graph.json
python .chaos-engine/tool.py graphify diagnose multigraph --graph graphify-out/graph.json --json
```

If either read-only command fails or returned paths do not match current files,
declare Graphify degraded and continue with the other retrieval sources. Do not
run refresh commands to repair retrieval during the active task.

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

The active primary checkout is the sole refresh owner. A linked-worktree
revision mismatch or active refresh lock is an expected degraded state after
G1 through G4, not an implementation blocker. A linked worktree or losing
refresh session must not refresh, retry-loop, clear or replace the lock or
cache, or alter the primary checkout to manufacture freshness. Only the primary
owner may schedule one later refresh after ownership is uncontested.

Useful bounded reads:

```text
python .chaos-engine/tool.py graphify explain "<symbol>" --graph graphify-out/graph.json
python .chaos-engine/tool.py graphify affected "<symbol>" --depth 2 --graph graphify-out/graph.json
python .chaos-engine/tool.py graphify path "<source>" "<target>" --graph graphify-out/graph.json
```
