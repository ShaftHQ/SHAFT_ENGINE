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

The shared cache is one `graphify-out/` for the repository, resolved from any
branch or linked worktree. The owner is
[`chaos-engine/stores.py`](../stores.py). Refresh indexes a detached snapshot of the local
default-branch tip. It does not fetch, reset, or clean a checkout, and it does
not read `~/.mempalace`.

```text
python3 .chaos-engine/tool.py stores refresh --if-stale
python3 .chaos-engine/tool.py stores install-schedule
python3 .chaos-engine/install.py repair --project . --component graphify
```

A second refresh exits while the repository lock is held. Session start and the
daily user timer call the same `--if-stale` command. An ordinary task must not
refresh, retry-loop, clear the lock, or alter a checkout to manufacture
freshness. When the default-branch commit is not local, refresh stops with
`fix-next: git fetch`.

Queries still run through `tool.py`, which binds `--graph` to the shared
`graph.json`. `--palace` and `--backend sqlite_exact` are global MemPalace
flags and must precede the subcommand.

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
