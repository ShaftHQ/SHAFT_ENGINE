# allure-cucumber8-jvm

**Purpose:** Redistribute the Cucumber JVM 8 Allure adapter so `shaft-engine` resolves it from Maven Central.

**Use or skip:** Use this module when the Cucumber 8 adapter jar must be built or published with SHAFT. Skip it for test code that only depends on the published coordinate.

## Humans

From the repository root, package this module without the test suite:

```bash
mvn -pl allure-cucumber8-jvm -am -DskipTests package
```

Published product usage stays on the user guide: [https://shafthq.github.io/](https://shafthq.github.io/).

## Agents

Load [AGENTS.md](../AGENTS.md) and follow ChaosEngine. This README is orientation only. Do not treat it as policy. Open this directory when the task names `allure-cucumber8-jvm`; otherwise skip it.

## Next

- [Root orientation](../README.md)
- [Reactor parent](../pom.xml)
- [Module POM](pom.xml)
