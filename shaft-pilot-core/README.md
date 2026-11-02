# shaft-pilot-core

**Purpose:** Provider-neutral contracts, security controls, and deterministic fallback for SHAFT Pilot.

**Use or skip:** Use this module for Pilot contracts and safety controls. Skip it for core engine tests that do not touch Pilot.

## Humans

From the repository root, compile this module without the test suite:

```bash
mvn -pl shaft-pilot-core -am -DskipTests package
```

Published product usage stays on the user guide: [https://shafthq.github.io/](https://shafthq.github.io/).

## Agents

Load [AGENTS.md](../AGENTS.md) and follow ChaosEngine. This README is orientation only. Do not treat it as policy. Open this directory when the task names `shaft-pilot-core`; otherwise skip it.

## Next

- [Root orientation](../README.md)
- [Reactor parent](../pom.xml)
