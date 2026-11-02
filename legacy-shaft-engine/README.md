# legacy-shaft-engine

**Purpose:** Legacy SHAFT coordinate retained as a relocation POM. The artifact id is `SHAFT_ENGINE`, not `legacy-shaft-engine`.

**Use or skip:** Do not start new work here. Consumers should use `io.github.shafthq:shaft-engine`. Open this module only when the relocation POM itself must change.

## Humans

From the repository root, compile this module without the test suite:

```bash
mvn -pl legacy-shaft-engine -am -DskipTests package
```

Published product usage stays on the user guide: [https://shafthq.github.io/](https://shafthq.github.io/).

## Agents

Load [AGENTS.md](../AGENTS.md) and follow ChaosEngine. This README is orientation only. Do not treat it as policy. Open this directory when the task names `legacy-shaft-engine`; otherwise skip it.

## Next

- [Root orientation](../README.md)
- [Reactor parent](../pom.xml)
