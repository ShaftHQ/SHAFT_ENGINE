# shaft-doctor

**Purpose:** Offline, deterministic SHAFT evidence collection and failure diagnosis.

**Use or skip:** Use this module for Doctor evidence and diagnosis. Skip it when the task does not read or change failure diagnosis.

## Humans

From the repository root, compile this module without the test suite:

```bash
mvn -pl shaft-doctor -am -DskipTests package
```

Published product usage stays on the user guide: [https://shafthq.github.io/](https://shafthq.github.io/).

## Agents

Load [AGENTS.md](../AGENTS.md) and follow ChaosEngine. This README is orientation only. Do not treat it as policy. Open this directory when the task names `shaft-doctor`; otherwise skip it.

## Next

- [Root orientation](../README.md)
- [Reactor parent](../pom.xml)
