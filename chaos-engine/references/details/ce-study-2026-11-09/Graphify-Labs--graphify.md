# Graphify-Labs/graphify

- URL: https://github.com/Graphify-Labs/graphify
- Lists: Charlie Hills snapshot, 6 Nov 2026, row 9 (143K stars on that snapshot).
- GitHub description: Turn a codebase, with docs, SQL schemas, configs, and PDFs, into a queryable knowledge graph. Local deterministic AST parsing, every edge explained, no vector store.
- README: README headings seen: what it does, a recall benchmark, documentation, prerequisites, install. The lead says code maps are local AST parses with no vector store.

## Reusable patterns

- A queryable graph from a local parse, with an explanation on each edge, instead of a vector index.
- The skill is a thin command over that graph.

## Adopt

- ChaosEngine already retrieves through Graphify. Keep that path. Do not vendor another graph engine and do not add a vector store beside it.

## Skip

- Do not copy Graphify's skill, parsers, or benchmark harness.
- Do not widen retrieve into a second graph product.

## License check

- Checked 2026-11-10. GraphQL licenseInfo.spdxId is Apache-2.0. gh api repos/Graphify-Labs/graphify/license returned HTTP 200 and path LICENSE.
- Apache-2.0 still requires keeping license and notices with any copy. No copy, and no skill edit from this note.

Adopt the idea. Do not copy third-party code. Do not vendor the repository.
