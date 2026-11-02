# shaft-ui-match

**Purpose:** Optional CPU-only matcher for a UI document that a web, mobile, or desktop capture step already built.

**Use or skip:** Optional module. Use it when a test should score two UI documents. Skip it when pixel highlights from `shaft-visual` are enough.

## System requirements

- CPU only. No GPU and no model download.
- Memory is the JVM heap that already holds the two documents and the JSON text.
- Disk is this jar plus one JSON file per run.
- Java 25, same as the rest of SHAFT.
- The matcher does not read screenshots, HTML, or accessibility trees itself. The caller passes URL, role, text, accessible name, image digest, bounds, and a locator hint.
- It does not import Selenium, Appium, or OpenCV. Icon pairs that share a digest are not distinguished.

## Humans

From the repository root:

```bash
mvn -pl shaft-ui-match -am -DskipTests package
```

Published product usage stays on the user guide: [https://shafthq.github.io/](https://shafthq.github.io/).

## Agents

Load [AGENTS.md](../AGENTS.md) and follow ChaosEngine. This README is orientation only. Do not treat it as policy. Open this directory when the task names `shaft-ui-match`; otherwise skip it.

## Next

- [Root orientation](../README.md)
- [Reactor parent](../pom.xml)
