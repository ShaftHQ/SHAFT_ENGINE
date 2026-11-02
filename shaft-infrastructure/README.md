# shaft-infrastructure

**Purpose:** Shared, deterministic setup planning and managed runtime infrastructure for SHAFT.

**Use or skip:** Use this module when the task changes shared setup or managed runtimes. Skip it for ordinary test authoring in a consumer project.

## Humans

From the repository root, compile this module without the test suite:

```bash
mvn -pl shaft-infrastructure -am -DskipTests package
```

Published product usage stays on the user guide: [https://shafthq.github.io/](https://shafthq.github.io/).

## Agents

Load [AGENTS.md](../AGENTS.md) and follow ChaosEngine. This README is orientation only. Do not treat it as policy. Open this directory when the task names `shaft-infrastructure`; otherwise skip it.

## Next

- [Root orientation](../README.md)
- [Reactor parent](../pom.xml)
