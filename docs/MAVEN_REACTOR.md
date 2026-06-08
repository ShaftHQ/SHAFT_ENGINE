# Maven reactor layout

SHAFT_ENGINE is built as a Maven reactor. The canonical published engine coordinate is
`io.github.shafthq:shaft-engine`; the former `io.github.shafthq:SHAFT_ENGINE` coordinate is a POM-only
relocation artifact that points consumers to the canonical JAR.

## Modules

- The root `pom.xml` is the `io.github.shafthq:shaft-parent` aggregator and build parent. It owns shared version properties, dependency management, and plugin management.
- `shaft-engine/pom.xml` builds the engine JAR. All framework source, resources, tests, examples, and runtime support assets remain under `shaft-engine/src/`; Java packages remain under `com.shaft`.
- `shaft-bom/pom.xml` publishes the consumer BOM. Importing it manages the `shaft-engine` version without adding dependencies by itself.
- `legacy-shaft-engine/pom.xml` publishes the legacy `SHAFT_ENGINE` coordinate as relocation metadata only; it contains no classes.

API, Appium/mobile, database, BrowserStack, desktop video, OpenCV visual processing, and every other retained capability remain dependencies of the engine module.

## Consumer usage

Use the canonical dependency directly:

```xml
<dependency>
    <groupId>io.github.shafthq</groupId>
    <artifactId>shaft-engine</artifactId>
    <version>${shaft.version}</version>
</dependency>
```

Or import the BOM and omit the engine version:

```xml
<dependencyManagement>
    <dependencies>
        <dependency>
            <groupId>io.github.shafthq</groupId>
            <artifactId>shaft-bom</artifactId>
            <version>${shaft.version}</version>
            <type>pom</type>
            <scope>import</scope>
        </dependency>
    </dependencies>
</dependencyManagement>
<dependencies>
    <dependency>
        <groupId>io.github.shafthq</groupId>
        <artifactId>shaft-engine</artifactId>
    </dependency>
</dependencies>
```

## Common commands

Run commands from the repository root:

```bash
python3 scripts/ci/validate_reactor_versions.py
mvn clean install -DskipTests -Dgpg.skip
mvn -pl shaft-engine -am test -Dtest=TestClassName
mvn -pl shaft-engine -am test
mvn -pl shaft-engine javadoc:javadoc
```

Build and test outputs are module-local. Important locations include:

- Engine JAR: `shaft-engine/target/shaft-engine-<version>.jar`
- Surefire reports: `shaft-engine/target/surefire-reports/`
- JaCoCo report: `shaft-engine/target/jacoco/`
- Allure results: `shaft-engine/allure-results/`
- Allure report: `shaft-engine/allure-report/`

When validating a test run, count `shaft-engine/allure-results/*-result.json` before evaluating statuses. An empty result directory is not a successful test oracle.

## Dependency baseline fixtures

The cold-cache fixtures cover direct canonical dependencies, BOM-managed versionless dependencies, and the legacy relocation path. Build the reactor before measuring them:

```bash
mvn clean install -DskipTests -Dgpg.skip
python3 scripts/ci/measure_consumer_dependencies.py --verify
```

The measurement script seeds the canonical engine JAR, the BOM, the legacy relocation POM, and the reactor parent into each isolated Maven repository.
