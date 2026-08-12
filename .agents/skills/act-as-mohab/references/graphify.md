# Graphify

Graphify answers current structural questions: likely files, calls, and
dependencies. Resolve shared cache first:

```text
py -3 tools/repository-map/resolve_graph_out.py --check
```

If current, pass the resolved graph to every query, then verify returned files
in the current worktree:

```powershell
$graphOut = py -3 tools/repository-map/resolve_graph_out.py --check
graphify query "<structural question>" --graph (Join-Path $graphOut "graph.json")
```

A missing cache reports `absent`; a cache without a matching indexed revision
reports `stale`. Either is degraded mode: use targeted `rg` plus other knowledge
sources, and flag a primary-checkout refresh. Never rebuild or record the cache
from a linked worktree, and never commit `graphify-out/`.

From the primary checkout, use the portable repository-owned controller for a
refresh or a read-only coverage audit:

```text
py -3 tools/repository-map/graphify_maintenance.py refresh --root .
py -3 tools/repository-map/graphify_maintenance.py audit --root .
```

The refresh uses `uv tool run --with tree-sitter-sql --from graphifyy
graphify` (`graphifyy` is the package; `graphify` is its command), then orders
build -> audit -> cluster -> freshness marker. JSON sources with no emitted
nodes remain visible expected data-only inputs; SQL or other parser gaps stop
the refresh before the marker is recorded.

Freshness behavior is pinned by
`tests/scripts/test_resolve_graph_out.py`.
