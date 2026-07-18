---
name: graphify
description: Map SHAFT_ENGINE before broad search.
---
# Graphify
Resolve the shared cache first: `py -3`/`python3 tools/repository-map/resolve_graph_out.py --check`. If it prints a path, query that graph before broad `Grep`/`Read` sweeps. If absent, fall back to `rg` + `.memory` (do not block the session on a rebuild); refresh only from the main checkout per `tools/repository-map/README.md`. Never commit `graphify-out`.
Graphify answers structure (deterministic, no DB lock) — use it for calls/depends-on and pre-search file selection; cache rebuilds nightly.
Full build/query pipeline (any corpus): `references/full-pipeline.md`.
