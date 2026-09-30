# shaft-bom

**Purpose:** Dependency management BOM for SHAFT consumer artifacts.

**Use or skip:** Use `io.github.shafthq:shaft-bom` when aligning consumer dependency versions. Skip it for behavior changes inside a single module.

## Humans

From the repository root, compile this module without the test suite:

```bash
mvn -pl shaft-bom -am -DskipTests package
```

Published product usage stays on the user guide: [https://shafthq.github.io/](https://shafthq.github.io/).

## Agents

Load [AGENTS.md](../AGENTS.md) and follow ChaosEngine. This README is orientation only. Do not treat it as policy. Open this directory when the task names `shaft-bom`; otherwise skip it.

## Next

- [Root orientation](../README.md)
- [Reactor parent](../pom.xml)
