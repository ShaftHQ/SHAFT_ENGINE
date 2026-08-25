# Graphify

Graphify is the project-local structural retrieval CLI. It is intentionally a
CLI rather than an MCP server. Run it only through the installed launcher so
the pinned project runtime is used on every host.

## Retrieve first

The installed launcher owns this provider-neutral G1–G4 route. One attempt is
enough; do not retry, repair, refresh, poll, or watch during an ordinary task:

- G1: Resolve any available cache at `graphify-out/graph.json` and require it to
  be readable and nonempty. Freshness affects trust, not whether a bounded query may yield a lead. Never
  infer failure from an absent MCP entry.
- G2: After G1 succeeds, run one query bounded to the affected symbol or
  subsystem, treat every result as an untrusted lead, verify every returned path
  against current project files, and supplement blast-radius conclusions with
  targeted `rg`.
- G3: Run the read-only multigraph diagnostic against that same graph. Do not
  refresh, replace, or repair generated state during retrieval.
- G4: Declare degraded mode if any step lacks current verified results, and
  only after G1 through G3 have been attempted. Continue with native Memory,
  MemPalace, and targeted `rg`.

```text
python .chaos-engine/tool.py graphify query "<bounded structural question>" --graph graphify-out/graph.json
python .chaos-engine/tool.py graphify diagnose multigraph --graph graphify-out/graph.json --json
```

If the cache is stale, either read-only command fails, or returned paths do not
match current files, declare Graphify degraded and continue with live files and
the other retrieval sources. Never infer completeness, “no callers,” or any
negative conclusion from the graph. Do not run refresh commands to repair
retrieval during the active task.

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

The configured maintenance controller is the sole refresh owner. A stale cache,
linked-worktree revision mismatch, or active refresh lock is an expected
degraded state, not an implementation blocker. An ordinary task must not
refresh, retry-loop, clear or replace the lock or cache, or alter the primary
checkout to manufacture freshness. Only the maintenance owner updates derived
store state.

An extract line that says files were not classified (no supported extension or
shebang) means the scanner saw those paths and has no file type. It is coverage
policy, not a missing install, corrupt cache, or doctor failure. File types are
hardcoded in Graphify; there is no install-time extension knob. Add a type
upstream, or ignore the path in `.graphifyignore`. Refresh owners split skips
into ignore versus promote. A file missing from the graph is not automatically
in MemPalace; that store has its own readable-extension list.

Useful bounded reads:

```text
python .chaos-engine/tool.py graphify explain "<symbol>" --graph graphify-out/graph.json
python .chaos-engine/tool.py graphify affected "<symbol>" --depth 2 --graph graphify-out/graph.json
python .chaos-engine/tool.py graphify path "<source>" "<target>" --graph graphify-out/graph.json
```
