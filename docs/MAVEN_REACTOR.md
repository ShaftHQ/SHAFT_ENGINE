# Maven reactor layout

SHAFT_ENGINE is built as a Maven reactor. The canonical published engine coordinate is
`io.github.shafthq:shaft-engine`; the former `io.github.shafthq:SHAFT_ENGINE` coordinate is a POM-only
relocation artifact that points consumers to the canonical JAR.

## Modules

- The root `pom.xml` is the `io.github.shafthq:shaft-parent` aggregator and build parent. It owns shared version properties, dependency management, and plugin management.
- `shaft-engine/pom.xml` builds the engine JAR. All framework source, resources, tests, examples, and runtime support assets remain under `shaft-engine/src/`; Java packages remain under `com.shaft`.
- `shaft-bom/pom.xml` publishes the consumer BOM. Importing it manages the `shaft-engine`, `shaft-browserstack`, `shaft-video`, and `shaft-visual` versions without adding dependencies by itself.
- `shaft-video/pom.xml` builds the optional desktop video recording provider. Add it when local desktop screen recording is needed; Appium-native recording remains in `shaft-engine`.
- `shaft-visual/pom.xml` builds the optional OpenCV visual-processing provider and its focused visual tests.
- `legacy-shaft-engine/pom.xml` publishes the legacy `SHAFT_ENGINE` coordinate as relocation metadata only; it contains no classes.

API, Appium/mobile, database, and other retained core capabilities remain dependencies of the engine module. Optional BrowserStack SDK, desktop video/FFmpeg, and OpenCV visual-processing support are provided by `shaft-browserstack`, `shaft-video`, and `shaft-visual` respectively.

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
mvn -pl shaft-browserstack -am test -Dtest=BrowserStackHelperUnitTest
mvn -pl shaft-video -am test -Dtest=DesktopVideoRecordingProviderRegistrationTest
mvn -pl shaft-visual -am test -Dtest=ImageProcessingActionsUnitTest
mvn -pl shaft-engine -am test
mvn -pl shaft-engine javadoc:javadoc
```

Build and test outputs are module-local. Important locations include:

- Engine JAR: `shaft-engine/target/shaft-engine-<version>.jar`
- Optional desktop video JAR: `shaft-video/target/shaft-video-<version>.jar`
- Optional visual-processing JAR: `shaft-visual/target/shaft-visual-<version>.jar`
- Surefire reports: `<module>/target/surefire-reports/`
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

The measurement script seeds the canonical engine JAR, optional integration JARs, the BOM, the legacy relocation POM, and the reactor parent into each isolated Maven repository.


## GitHub Actions reactor policy

Workflow commands run from the repository root and select only the modules required by each job:

| Workflow | Reactor policy |
| --- | --- |
| `e2eTests.yml` | Engine jobs use `-pl shaft-engine -am`; BrowserStack jobs use `-pl shaft-browserstack -am`; focused visual and desktop-video jobs use their respective optional modules. |
| `e2eLocalTests.yml`, `e2eLambdaTestTests.yml`, `e2eMoonTests.yml` | Use `-pl shaft-engine -am`; Appium and ordinary browser jobs do not resolve `shaft-video`. |
| `coverage-readiness.yml` | Runs tests and JaCoCo reporting with `-pl shaft-engine -am`. |
| `code-quality-scan.yml`, `codeql-analysis.yml`, `copilot-setup-steps.yml` | Select the code-bearing engine and optional integration modules explicitly; BOM and relocation-only modules are excluded from compilation/analysis. |
| `publishJavaDocs.yml` | Generates JavaDocs only for `shaft-engine` and publishes `shaft-engine/target/reports/apidocs`. |
| `mavenCentral_cd.yml` | Intentionally deploys the complete reactor because every published module, BOM, and relocation POM belongs to a release. |
| `reactor-version-check.yml` | Uses the reactor-version validation script and does not invoke Maven. |
| `dependency_review.yml`, `link-check.yml`, `refresh-agent-instructions.yml` | Do not build Java modules and therefore require no reactor selection. |
| `sync-sample-projects-version.yml`, `update-selenium-grid-versions.yml` | Operate on module-relative assets under `shaft-engine/src/main/resources`. |

The shared post-test action defaults to `shaft-engine` but accepts `module-directory` for optional-module jobs. It reads and uploads `<module>/allure-results`, `<module>/allure-report`, `<module>/target/surefire-reports`, and `<module>/target/jacoco`. It counts raw `*-result.json` files before interpreting Allure summary status and fails an empty test run.
