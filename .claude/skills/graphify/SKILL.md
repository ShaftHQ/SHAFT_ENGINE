---
name: graphify
description: Map SHAFT_ENGINE before broad search.
---
# Graphify
Resolve the shared cache first: `py -3`/`python3 tools/repository-map/resolve_graph_out.py --check`. If it prints a path, query that graph before broad `Grep`/`Read` sweeps. If absent, fall back to `rg` + `.memory` (do not block the session on a rebuild); refresh only from the main checkout per `tools/repository-map/README.md`. Never commit `graphify-out`.
Division of labor: graphify answers structure (which files/modules relate, where things live — deterministic, no DB lock); `gbrain query`/`code-def`/`code-refs` answers meaning (semantic/symbol retrieval). Ask the one matching the question. The cache rebuilds nightly on the maintainer machine. For relationship/dependency/"what calls/depends on X" questions, prefer graphify over gbrain's graph tools (`code-callers`/`code-callees`/`backlinks`/`traverse_graph`) — verified 2026-07-17 that gbrain's link graph is unbuilt for this brain (0 links across 1450 shaft-engine pages; the CLI command to build it, `reconcile-links`, is unreachable — [gbrain#2900](https://github.com/garrytan/gbrain/issues/2900)). Re-check once that's fixed upstream; see `skills/retrieval-reflex/SKILL.md` for the paired note.
Full build/query pipeline (any corpus): `references/full-pipeline.md`.
