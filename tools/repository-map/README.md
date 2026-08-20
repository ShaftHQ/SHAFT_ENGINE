# Repository Map

SHAFT_ENGINE uses Graphify for an optional local repository map. The checked-in files only teach agents how to build and query it; generated `graphify-out/` files stay local.

## Install

Use the official package name, `graphifyy`; the CLI command is still `graphify`.

```powershell
# preferred isolated install
uv tool install graphifyy

# update an existing isolated install
uv tool upgrade graphifyy

# Windows fallback
py -3 -m pip install --user --upgrade graphifyy
```

If `graphify` is not found after the uv install, run `uv tool update-shell` and
open a new terminal. A plain-pip fallback may likewise require adding its
Scripts directory to PATH.

## Build or refresh

Run the repository-owned controller from the **primary checkout**. It supplies
the optional SQL parser in an isolated uv tool environment, audits every
manifest path, clusters the graph, and records freshness only after all three
stages succeed:

```powershell
py -3 tools/repository-map/graphify_maintenance.py refresh --root .
```

Audit an existing cache without modifying it:

```powershell
py -3 tools/repository-map/graphify_maintenance.py audit --root .
```

Both commands accept any repository through `--root`. The read-only `audit`
command also accepts a relative `--graph-out` below that root. Refresh owns the
fixed `graphify-out` cache so it cannot audit one output and mark another.
Concurrent refreshes fail fast under a repository-local advisory OS lock, which
the kernel releases if the maintenance process exits or is killed.
Zero-node JSON files are reported as expected data-only inputs. Zero-node SQL
files or any other parser gaps fail the audit.
The resolver binds a successful graph to the primary checkout's exact Git
revision. Do not refresh or record from a linked worktree.

On PowerShell these are CLI commands; `/graphify` is an agent-chat command and
is not executable in the shell.

## Query

Use Graphify to choose files before broad manual search:

```powershell
$graphOut = py -3 tools/repository-map/resolve_graph_out.py --check
graphify query "Where is the core SHAFT WebDriver facade and related test coverage?" --graph (Join-Path $graphOut "graph.json")
graphify path "SHAFT.GUI.WebDriver" "DriverFactory" --graph (Join-Path $graphOut "graph.json")
graphify export callflow-html --graph (Join-Path $graphOut "graph.json")
```

After Graphify identifies likely files, read exact files with `rg` and small excerpts before editing. Keep using Memory for durable decisions and gotchas; do not rely on Graphify output as the source of truth.

The default `.graphifyignore` keeps semantic document/media formats out of the graph so a code/config map can be built without LLM API keys. Remove those ignore entries only when semantic document extraction is explicitly needed and a suitable key is available.

## Git Hygiene

`graphify-out/` is intentionally ignored (`.gitignore`). Do not commit generated graph reports, HTML, JSON, caches, or binary exports unless a maintainer explicitly asks for a reviewed snapshot.

Do not run `graphify hook install` here. Git LFS already owns `post-commit`,
`post-checkout`, and `pre-push`. Graphify's installer would replace those
hooks. Per-commit rebuilds also violate the primary-only refresh owner
(`graphify_maintenance.py`) and would race linked worktrees.

Do not set `MEMPAL_DIR` to this checkout. Official MemPalace Stop/PreCompact
hooks may live in a host user profile, but auto-mining the SHAFT tree on every
save races `SHAFT-Nightly-Knowledge-Refresh` and duplicates `shaft_engine_main`.
Conversation ingest stays `--mode convos` against transcript dirs, never the
repository root.

## Shared cache across worktrees

Because `graphify-out/` is gitignored, it never exists in a fresh `git worktree` clone. Rather than rebuilding per worktree, treat the **main checkout's** `graphify-out/` as one shared, read-only cache for all linked worktrees:

- Resolve it from any worktree with `python3 tools/repository-map/resolve_graph_out.py` (prints the absolute path under the main checkout root, derived from `git rev-parse --git-common-dir`).
- Set `SHAFT_GRAPHIFY_OUT` to an absolute cache path when the shared cache is
  outside that checkout. An explicitly blank or relative value fails closed.
  A refresh accepts the override only when it is that primary checkout's own
  `graphify-out`; this prevents one checkout from marking another cache current.
- Check availability with `python3 tools/repository-map/resolve_graph_out.py --check`: exits `0` and prints the path only when the cache marker matches the revision being inspected. Missing caches report `absent`; unmarked, changed, or revision-mismatched caches report `stale` with the indexed and requested revisions when available. Both degraded modes exit `1` and fall back to `rg` plus Memory.
- Refresh the cache with `py -3 tools/repository-map/graphify_maintenance.py
  refresh --root .` from the **primary checkout**; worktree sessions only read
  it. On the primary maintainer machine the nightly maintenance task invokes
  the same controller automatically.
- The "mandatory entry point" rule is satisfied by running the `--check` resolve, not by building the graph. If the cache is absent, fall back to `rg` and `.memory` instead of blocking the session on a rebuild.

## Shared MemPalace across worktrees

The SHAFT palace lives at `<git-common-dir>/chaos-engine/mempalace` (today the
primary `.git` tree). Do not create `<checkout>/.chaos-engine-state/mempalace`
and do not junction that empty path onto the centralized palace.

- Resolve it from any worktree with `python3 tools/repository-map/resolve_mempalace.py`.
- Set `SHAFT_MEMPALACE` to an absolute palace path when the shared palace is
  outside that checkout. An explicitly blank or relative value fails closed.
- Operator command from any worktree cwd:

```powershell
py -3 scripts/agents/knowledge_stores.py status
py -3 scripts/agents/knowledge_stores.py search "shared cache" --wing shaft_engine_main
```

`refresh` refuses linked worktrees and ordinary checkouts. Overnight mine/sync
stays with `SHAFT-Nightly-Knowledge-Refresh` (#4809). MemPalace `daemon` is an
opt-in write serializer, not an overnight miner; do not start a second racer.
