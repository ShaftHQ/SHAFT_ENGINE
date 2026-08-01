# Graphify

Graphify answers current structural questions: likely files, calls, and
dependencies. Resolve shared cache first:

```text
py -3 tools/repository-map/resolve_graph_out.py --check
```

If available, run `graphify query "<structural question>"` from primary
checkout holding cache, then verify returned files in current worktree. If
cache or CLI is unavailable, record degraded mode and use targeted `rg` plus
other knowledge sources. Never rebuild from a worktree or commit
`graphify-out/`. If the repo-local resolver is absent, continue without it;
if it is stale, flag a refresh at completion.
