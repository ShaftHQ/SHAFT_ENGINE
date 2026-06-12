# Maven reactor layout

SHAFT_ENGINE is built as a Maven reactor. The canonical published engine coordinate is
`io.github.shafthq:shaft-engine`; the former `io.github.shafthq:SHAFT_ENGINE` coordinate is a POM-only
relocation artifact that points consumers to the canonical JAR.

## Modules

- The root `pom.xml` is the `io.github.shafthq:shaft-parent` aggregator and build parent. It owns shared version properties, dependency management, and plugin management.
- `shaft-engine/pom.xml` builds the engine JAR. All framework source, resources, tests, examples, and runtime support assets remain under `shaft-engine/src/`; Java packages remain under `com.shaft`.
- `shaft-bom/pom.xml` publishes the consumer BOM. Importing it manages the `shaft-engine`, `shaft-pilot-core`, `shaft-capture`, `shaft-doctor`, `shaft-ai`, `shaft-browserstack`, `shaft-video`, `shaft-visual`, and `SHAFT_MCP` versions without adding dependencies by itself.
- `shaft-pilot-core/pom.xml` builds provider-neutral Pilot contracts, security controls, configuration snapshots, and deterministic fallback. It depends on `shaft-engine`; the engine has no reverse dependency.
- `shaft-capture/pom.xml` builds managed Chrome/Edge recording, versioned contracts, deterministic privacy classification, Java/TestNG generation, compile/replay validation, schema migration, and atomic JSON persistence. It depends on `shaft-pilot-core` and has no dependency on `shaft-ai`.
- `shaft-doctor/pom.xml` builds allowlisted local evidence collection, redacted portable bundles, deterministic failure rules, and JSON/Markdown reports. It depends on `shaft-pilot-core` and has no dependency on `shaft-ai`.
- `shaft-ai/pom.xml` builds optional direct OpenAI, Anthropic, Gemini, and Ollama adapters. It depends on `shaft-pilot-core` and exposes no provider SDK types.
- `shaft-mcp/pom.xml` builds the optional executable MCP server plus SHAFT Capture and Doctor CLIs. It depends on `shaft-engine`, `shaft-capture`, and `shaft-doctor`; the engine has no reverse dependency on MCP.
- `shaft-browserstack/pom.xml` builds the optional BrowserStack Java SDK integration. Direct BrowserStack
  WebDriver/Appium sessions remain in `shaft-engine`.
- `shaft-video/pom.xml` builds the optional desktop video recording provider. Add it when local desktop screen recording is needed; Appium-native recording remains in `shaft-engine`.
- `shaft-visual/pom.xml` builds the optional OpenCV, Applitools Eyes, and Shutterbug visual-processing provider and its
  focused visual tests.
- `legacy-shaft-engine/pom.xml` publishes the legacy `SHAFT_ENGINE` coordinate as relocation metadata only; it contains no classes.

API, Appium/mobile, database, and other retained core capabilities remain dependencies of the engine module. Optional
BrowserStack SDK, desktop video/FFmpeg, and OpenCV/Eyes/Shutterbug visual-processing support are provided by
`shaft-browserstack`, `shaft-video`, and `shaft-visual` respectively.

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
mvn -pl shaft-pilot-core,shaft-capture,shaft-doctor,shaft-ai -am test
mvn -pl shaft-mcp -am test -Dtest=ShaftMcpApplicationTests
mvn -pl shaft-mcp -am package -DskipTests -Dgpg.skip
python3 scripts/ci/validate_shaft_mcp_transports.py
mvn -pl shaft-engine -am test
mvn clean install -DskipTests -Dgpg.skip
python3 scripts/ci/assemble_javadocs.py
```

Build and test outputs are module-local. Important locations include:

- Engine JAR: `shaft-engine/target/shaft-engine-<version>.jar`
- Pilot contracts JAR: `shaft-pilot-core/target/shaft-pilot-core-<version>.jar`
- Capture contracts JAR: `shaft-capture/target/shaft-capture-<version>.jar`
- Doctor analyzer JAR: `shaft-doctor/target/shaft-doctor-<version>.jar`
- Optional direct providers JAR: `shaft-ai/target/shaft-ai-<version>.jar`
- Optional BrowserStack SDK JAR: `shaft-browserstack/target/shaft-browserstack-<version>.jar`
- Optional desktop video JAR: `shaft-video/target/shaft-video-<version>.jar`
- Optional visual-processing JAR: `shaft-visual/target/shaft-visual-<version>.jar`
- MCP executable JAR: `shaft-mcp/target/SHAFT_MCP-<version>.jar`
- Surefire reports: `<module>/target/surefire-reports/`
- Aggregate JaCoCo report: `target/jacoco/`
- Per-module JaCoCo execution data: `<module>/target/jacoco.exec`
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
| `coverage-readiness.yml` | Runs focused engine tests, builds `report-aggregate`, gates `target/jacoco/jacoco.csv`, and uploads only `target/jacoco/jacoco.xml` to Codecov. |
| `code-quality-scan.yml`, `codeql-analysis.yml`, `copilot-setup-steps.yml` | Select the code-bearing engine, optional integrations, and MCP module explicitly; BOM and relocation-only modules are excluded from compilation/analysis. |
| `shaft-mcp.yml` | Runs manually and daily at 01:00 UTC with local E2E workflows; packages the executable and smokes stdio plus Streamable HTTP. |
| `publishJavaDocs.yml` | Builds JavaDocs for all Java-bearing modules, assembles a navigable site at `target/javadocs`, and publishes it only after a successful Central release. |
| `mavenCentral_cd.yml` | Intentionally deploys the complete reactor because every published module, BOM, and relocation POM belongs to a release. |
| `reactor-version-check.yml` | Uses the reactor-version validation script and does not invoke Maven. |
| `dependency_review.yml`, `link-check.yml`, `refresh-agent-instructions.yml` | Do not build Java modules and therefore require no reactor selection. |
| `sync-sample-projects-version.yml`, `update-selenium-grid-versions.yml` | Operate on module-relative assets under `shaft-engine/src/main/resources`. |

The shared post-test action defaults to `shaft-engine` but accepts `module-directory` for optional-module jobs. It reads and uploads `<module>/allure-results`, `<module>/allure-report`, `<module>/target/surefire-reports`, and `<module>/target/jacoco`. Coverage upload is centralized in `coverage-readiness.yml` so parallel E2E jobs do not upload duplicate source reports. It counts raw `*-result.json` files before interpreting Allure summary status and fails an empty test run.

## Aggregate coverage continuity

`report-aggregate` is a build-only module that depends on every Java-bearing module, including the Pilot modules and `shaft-mcp`, and writes JaCoCo XML, CSV, and HTML to root `target/jacoco`. It is skipped by Maven deploy and is not a published SHAFT artifact.

Use pre-reactor commit `570a83674b70477074621ea24d7cfa05517260c6` as the coverage-continuity reference. Run the same test selector on that commit and on the current branch, then compare the resulting CSV summaries with `scripts/ci/jacoco_coverage_gate.py`. The current reactor command is:

```bash
mvn -pl shaft-engine -am test -Dtest=PathParamTest,SwaggerContractTest -Dgpg.skip
mvn -pl report-aggregate -am verify -DskipTests -Dgpg.skip
scripts/ci/jacoco_coverage_gate.py target/jacoco/jacoco.csv --metric line --minimum 0
```

Codecov receives only `target/jacoco/jacoco.xml`; automatic coverage-file discovery is disabled so module-local reports cannot duplicate source entries. Dependabot scans every reactor POM and groups the same Maven dependency across directories into one aligned update. `scripts/ci/validate_quality_configuration.py` protects these settings, while the `shaft-engine` Enforcer execution rejects optional BrowserStack, desktop-video, or visual-processing dependencies that leak back into the core engine.

Consumer-facing method boundaries are documented in
`docs/UPGRADING_TO_MODULAR_SHAFT.md`, with focused guides for
`shaft-browserstack`, `shaft-video`, and `shaft-visual`.

MCP client configuration, transport behavior, and credential boundaries are
documented in [SHAFT MCP](SHAFT_MCP.md).

Pilot contracts, direct-provider configuration, approval, and redaction are
documented in [SHAFT Pilot AI](SHAFT_PILOT_AI.md).

Managed-browser recording, its CLI/MCP controls, the versioned format, and the
deterministic privacy boundary are documented in
[SHAFT Capture](SHAFT_CAPTURE.md).

Portable evidence, deterministic diagnosis, CLI/MCP usage, and sharing guidance
are documented in [SHAFT Doctor](SHAFT_DOCTOR.md).
