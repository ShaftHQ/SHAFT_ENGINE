# Maven reactor layout

SHAFT_ENGINE is built as a Maven reactor while the published engine artifact remains
`io.github.shafthq:SHAFT_ENGINE` for compatibility.

## Modules

- The root `pom.xml` is the `io.github.shafthq:shaft-parent` aggregator and build parent. It owns shared version properties, dependency management, and plugin management.
- `shaft-engine/pom.xml` builds the existing engine JAR. All framework source, resources, tests, examples, and runtime support assets live under `shaft-engine/src/`.

This layout is intentionally behavior-preserving. API, Appium/mobile, database, BrowserStack, desktop video, OpenCV visual processing, and every other existing capability remain dependencies of the engine module at this stage.

## Common commands

Run commands from the repository root:

```bash
# Build and install the complete reactor without executing tests.
mvn clean install -DskipTests -Dgpg.skip

# Run one engine test class and build required upstream modules.
mvn -pl shaft-engine -am test -Dtest=TestClassName

# Run the complete engine test suite.
mvn -pl shaft-engine -am test

# Generate the engine JavaDocs.
mvn -pl shaft-engine javadoc:javadoc
```

Build and test outputs are module-local. Important locations include:

- Engine JAR: `shaft-engine/target/SHAFT_ENGINE-<version>.jar`
- Surefire reports: `shaft-engine/target/surefire-reports/`
- JaCoCo report: `shaft-engine/target/jacoco/`
- Allure results: `shaft-engine/allure-results/`
- Allure report: `shaft-engine/allure-report/`

When validating a test run, count `shaft-engine/allure-results/*-result.json` before evaluating statuses. An empty result directory is not a successful test oracle.

## Dependency baseline fixtures

The cold-cache consumer fixtures continue to resolve the legacy engine coordinate. Build the reactor before measuring them:

```bash
mvn clean install -DskipTests -Dgpg.skip
python3 scripts/ci/measure_consumer_dependencies.py --verify
```

The measurement script seeds the engine JAR, the engine module POM, and the reactor parent POM into each isolated Maven repository so the resulting dependency graph matches normal Maven installation.
