# shaft-capture-proxy

**Purpose:** Embedded MITM HTTP(S) proxy that records native mobile API traffic into SHAFT Capture sessions.

**Use or skip:** Use this module for the capture proxy (Android and iOS emulators, simulators, and devices). Skip it unless mobile API recording is in scope.

## Humans

From the repository root, compile this module without the test suite:

```bash
mvn -pl shaft-capture-proxy -am -DskipTests package
```

Published product usage stays on the user guide: [https://shafthq.github.io/](https://shafthq.github.io/).

## Agents

Load [AGENTS.md](../AGENTS.md) and follow ChaosEngine. This README is orientation only. Do not treat it as policy. Open this directory when the task names `shaft-capture-proxy`; otherwise skip it.

## Next

- [Root orientation](../README.md)
- [Reactor parent](../pom.xml)
