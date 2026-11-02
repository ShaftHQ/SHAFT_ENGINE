# report-aggregate

**Purpose:** Non-deployed aggregate quality reports for Java SHAFT modules, plus the deterministic per-shard blob merge CLI.

**Use or skip:** Build and report tooling. Use it for aggregate reports or sharded blob merge. Skip it for product API changes.

## Humans

From the repository root, compile this module without the test suite:

```bash
mvn -pl report-aggregate -am -DskipTests package
```

Published product usage stays on the user guide: [https://shafthq.github.io/](https://shafthq.github.io/).

## Agents

Load [AGENTS.md](../AGENTS.md) and follow ChaosEngine. This README is orientation only. Do not treat it as policy. Open this directory when the task names `report-aggregate`; otherwise skip it.

## Next

- [Root orientation](../README.md)
- [Reactor parent](../pom.xml)
