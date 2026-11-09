# NVIDIA/auto-ontology

- URL: https://github.com/NVIDIA/auto-ontology
- Lists: Weekly top 10, 2-8 Nov 2026, row 03 (+9.3k stars that week). Not in the Charlie Hills 23.
- GitHub description: A Python framework for a semantic, graph-based reasoning layer over data. Deterministic query generation, better retrieval, and more explainable agent answers.
- README: README headings seen: benchmarks, key features, software components including NVIDIA NIM, quickstart, how it works. The captured lead is the SPDX header comment, not a feature sentence.

## Reusable patterns

- Put a named ontology between the agent and the data so a query is generated from the model, not from a free-form prompt.
- Explainability is a property of the query, not a paragraph added after the answer.

## Adopt

- Graphify already answers 'what calls this' from a local graph. An ontology layer would be a new product on top of that. Do not add it in this delivery and do not vendor the framework.

## Skip

- Do not copy the framework, the NIM model list, or the benchmarks.
- Do not add an NVIDIA runtime dependency.

## License check

- Checked 2026-11-10. GraphQL licenseInfo.spdxId is Apache-2.0. gh api repos/NVIDIA/auto-ontology/license returned HTTP 200 and path LICENSE.
- No skill edit from this note.

