---
name: graphify
description: Map SHAFT_ENGINE before broad search.
---
# Graphify
Resolve the shared cache first: `python3 tools/repository-map/resolve_graph_out.py --check`. If it prints a path, query that graph before broad `Grep`/`Read` sweeps. If absent, fall back to `rg` + `.memory` (do not block the session on a rebuild); refresh only from the main checkout per `tools/repository-map/README.md`. Never commit `graphify-out`.
