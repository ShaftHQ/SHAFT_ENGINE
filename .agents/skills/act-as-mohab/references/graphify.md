# Graphify

Graphify answers current structural questions: likely files, calls, and
dependencies. Resolve shared cache first:

```text
py -3 tools/repository-map/resolve_graph_out.py --check
```

If current, run `graphify query "<structural question>"` from the primary
checkout holding the cache, then verify returned files in the current
worktree. A missing cache reports `absent`; a cache without a matching indexed
revision reports `stale`. Either is degraded mode: use targeted `rg` plus other
knowledge sources, and flag a primary-checkout refresh. Never rebuild or record
the cache from a linked worktree, and never commit `graphify-out/`.

Freshness behavior is pinned by
`tests/scripts/test_resolve_graph_out.py`.
