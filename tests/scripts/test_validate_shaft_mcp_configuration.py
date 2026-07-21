import importlib.util
import json
import tempfile
import unittest
from pathlib import Path


MODULE_PATH = Path(__file__).resolve().parents[2] / "scripts" / "ci" / "validate_shaft_mcp_configuration.py"
SPEC = importlib.util.spec_from_file_location("validate_shaft_mcp_configuration", MODULE_PATH)
MODULE = importlib.util.module_from_spec(SPEC)
if SPEC.loader is None:
    raise ImportError(f"Unable to load {MODULE_PATH}")
SPEC.loader.exec_module(MODULE)


def _write_text(path: Path, text: str) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(text, encoding="utf-8")


ROOT_POM = """<project xmlns="http://maven.apache.org/POM/4.0.0">
    <modelVersion>4.0.0</modelVersion>
    <artifactId>shaft-parent</artifactId>
    <modules>
        <module>shaft-engine</module>
        <module>shaft-mcp</module>
        <module>shaft-bom</module>
    </modules>
</project>
"""

ENGINE_POM = """<project xmlns="http://maven.apache.org/POM/4.0.0">
    <modelVersion>4.0.0</modelVersion>
    <artifactId>shaft-engine</artifactId>
</project>
"""

BOM_POM = """<project xmlns="http://maven.apache.org/POM/4.0.0">
    <modelVersion>4.0.0</modelVersion>
    <artifactId>shaft-bom</artifactId>
    <dependencyManagement>
        <dependencies>
            <dependency>
                <artifactId>shaft-mcp</artifactId>
                <version>${project.version}</version>
            </dependency>
        </dependencies>
    </dependencyManagement>
</project>
"""

MCP_POM = """<project xmlns="http://maven.apache.org/POM/4.0.0">
    <modelVersion>4.0.0</modelVersion>
    <artifactId>shaft-mcp</artifactId>
    <dependencies>
        <dependency>
            <artifactId>shaft-engine</artifactId>
            <version>${project.version}</version>
        </dependency>
        <dependency>
            <artifactId>shaft-doctor</artifactId>
            <version>${project.version}</version>
        </dependency>
        <dependency>
            <artifactId>shaft-ai</artifactId>
            <version>${project.version}</version>
        </dependency>
    </dependencies>
    <build>
        <plugins>
            <plugin>
                <artifactId>maven-jar-plugin</artifactId>
                <configuration>
                    <archive>
                        <manifestEntries>
                            <Implementation-Version>${project.version}</Implementation-Version>
                        </manifestEntries>
                    </archive>
                </configuration>
            </plugin>
            <plugin>
                <artifactId>maven-compiler-plugin</artifactId>
                <configuration>
                    <parameters>true</parameters>
                </configuration>
            </plugin>
        </plugins>
    </build>
    <!-- packages META-INF/shaft-mcp/runtime-dependencies.txt -->
</project>
"""

APPLICATION_JAVA = """package com.shaft.mcp;

public class ShaftMcpApplication {
    public static void main(String[] args) {
        System.out.println("start");
    }
}
"""

STDIO_PROPERTIES = """spring.main.web-application-type=none
spring.ai.mcp.server.stdio=true
spring.ai.mcp.server.version=@project.version@
spring.main.banner-mode=off
"""

HTTP_PROPERTIES = """spring.main.web-application-type=servlet
spring.ai.mcp.server.stdio=false
spring.ai.mcp.server.protocol=STREAMABLE
spring.ai.mcp.server.streamable-http.mcp-endpoint=/mcp
spring.ai.mcp.server.version=@project.version@
"""

LOGBACK_XML = """<configuration>
    <appender name="STDERR" class="ch.qos.logback.core.ConsoleAppender">
        <target>System.err</target>
    </appender>
</configuration>
"""

SERVER_JSON = """{
  "name": "shaft-mcp",
  "version": "@project.version@",
  "packages": [{"version": "@project.version@"}]
}
"""

REQUIRED_TOOLS = (
    "doctor_analyze_failed_allure",
    "doctor_suggest_fix",
    "capture_checkpoint",
    "capture_generate_replay",
    "capture_code_blocks",
    "browser_get_page_dom",
    "browser_take_screenshot",
    "browser_navigate",
    "capture_start",
    "capture_record_at_target_code_blocks",
    "mobile_get_contexts",
    "mobile_get_accessibility_tree",
    "element_click",
    "element_type",
)


def _tool_manifest(omit_tool: str | None = None) -> str:
    tools = [
        {"name": name, "mutation": False, "sensitive": False, "deprecated": False}
        for name in REQUIRED_TOOLS
        if name != omit_tool
    ]
    return json.dumps({"schemaVersion": "1.0", "tools": tools})


SHAFT_MCP_WORKFLOW = """name: shaft-mcp

on:
  workflow_dispatch:
  schedule:
    - cron: '00 1 * * *'

jobs:
  mcp-tests:
    strategy:
      matrix:
        os: [ubuntu-22.04, macos-15, windows-2025]
    runs-on: ${{ matrix.os }}
    env:
      MCP_RELATED_TESTS: ExampleTest#methodOne
    steps:
      - uses: ./.github/actions/mcp-jar-build
      - run: mvn -pl shaft-mcp -am install
      - run: mvn -pl shaft-mcp test -Dtest="${MCP_RELATED_TESTS}" -DheadlessExecution=true
"""

MAVEN_CENTRAL_WORKFLOW = """name: mavenCentral_cd

on:
  workflow_dispatch:

jobs:
  build_release_and_deliver:
    runs-on: ubuntu-22.04
    steps:
      - uses: ./.github/actions/mcp-jar-build
      - run: python scripts/ci/verify_shaft_mcp_installer_release.py

  verify_public_shaft_mcp_installer:
    runs-on: ubuntu-22.04
    steps:
      - run: echo verify

  announce:
    needs: [build_release_and_deliver, verify_public_shaft_mcp_installer]
    runs-on: ubuntu-22.04
    steps:
      - run: echo done
"""

PILOT_RELEASE_WORKFLOW = """name: shaft-pilot-release

on:
  workflow_dispatch:

jobs:
  release:
    runs-on: ubuntu-22.04
    steps:
      - run: mvn dependency:copy-dependencies -DoutputDirectory=${maven.multiModuleProjectDirectory}/shaft-mcp/target/dependency
"""

MCP_JAR_BUILD_ACTION = """name: 'MCP Jar Build'
runs:
  using: 'composite'
  steps:
    - shell: bash
      run: mvn dependency:copy-dependencies -DoutputDirectory=${maven.multiModuleProjectDirectory}/shaft-mcp/target/dependency
"""

INSTALL_SHAFT_MCP_PY = """# shaft-mcp installer implementation
# handles maven-metadata.xml lookups and .sha256 verification
# packages runtime-dependencies.txt and shaft-mcp.args
# reads shaft.mcp.fallbackWorkspaceRoot
# uses ThreadPoolExecutor for parallel downloads
# USER_GUIDE_URL = "https://example.invalid/guide"
# supports copilot-intellij, claude-desktop, intellij-plugin clients
# --json output mode
# probe_stdio() checks server readiness
"""

INSTALL_PS1 = """# Thin bootstrapper for shaft-mcp
# downloads python-build-standalone runtime and shows progress
# then runs install_shaft_mcp.py
Write-Host "progress: downloading"
"""

INSTALL_SH = """#!/usr/bin/env bash
# Thin bootstrapper for shaft-mcp
# downloads python-build-standalone runtime and shows progress
# then runs install_shaft_mcp.py
echo "progress: downloading"
"""

VERIFY_INSTALLER_PY = """# verifies public shaft-mcp installer release
# covers claude-desktop, copilot-intellij, intellij-plugin clients
# asserts shaft.mcp.fallbackWorkspaceRoot argfile entry:
# "must not pin per-project entries"
"""

DOCKERFILE = """FROM eclipse-temurin:25-jdk AS build
RUN mvn -pl shaft-mcp -am install
RUN mvn dependency:copy-dependencies -DoutputDirectory=/build/shaft-mcp-lib

FROM eclipse-temurin:25-jre
RUN apt-get update && apt-get install -y google-chrome-stable
COPY --from=build /build/shaft-mcp-lib /app/lib
COPY --from=build shaft-mcp.jar /app/shaft-mcp.jar
ENTRYPOINT ["java", "-DheadlessExecution=true", "-cp", "/app/shaft-mcp.jar:/app/lib/*", "com.shaft.mcp.ShaftMcpApplication"]
"""


def _write_minimal_mcp_root(root: Path) -> None:
    _write_text(root / "pom.xml", ROOT_POM)
    _write_text(root / "shaft-mcp/pom.xml", MCP_POM)
    _write_text(root / "shaft-engine/pom.xml", ENGINE_POM)
    _write_text(root / "shaft-bom/pom.xml", BOM_POM)
    _write_text(root / "shaft-mcp/src/main/java/com/shaft/mcp/ShaftMcpApplication.java", APPLICATION_JAVA)
    _write_text(root / "shaft-mcp/src/main/resources/application.properties", STDIO_PROPERTIES)
    _write_text(root / "shaft-mcp/src/main/resources/application-http.properties", HTTP_PROPERTIES)
    _write_text(root / "shaft-mcp/src/main/resources/logback-spring.xml", LOGBACK_XML)
    _write_text(root / "shaft-mcp/server.json", SERVER_JSON)
    _write_text(root / "shaft-mcp/src/test/resources/fixtures/mcp-tool-manifest.json", _tool_manifest())
    _write_text(root / ".github/workflows/shaft-mcp.yml", SHAFT_MCP_WORKFLOW)
    _write_text(root / ".github/workflows/publish-shaft-mcp.yml", "name: publish-shaft-mcp\n")
    _write_text(root / ".github/workflows/deploy-shaft-mcp.yml", "name: deploy-shaft-mcp\n")
    _write_text(root / ".github/workflows/mavenCentral_cd.yml", MAVEN_CENTRAL_WORKFLOW)
    _write_text(root / ".github/workflows/shaft-pilot-release.yml", PILOT_RELEASE_WORKFLOW)
    _write_text(root / ".github/actions/mcp-jar-build/action.yml", MCP_JAR_BUILD_ACTION)
    _write_text(root / "scripts/mcp/install_shaft_mcp.py", INSTALL_SHAFT_MCP_PY)
    _write_text(root / "scripts/mcp/install-shaft-mcp.ps1", INSTALL_PS1)
    _write_text(root / "scripts/mcp/install-shaft-mcp.sh", INSTALL_SH)
    _write_text(root / "scripts/ci/verify_shaft_mcp_installer_release.py", VERIFY_INSTALLER_PY)
    _write_text(root / "shaft-mcp/Dockerfile", DOCKERFILE)


class ValidateShaftMcpConfigurationTest(unittest.TestCase):
    def test_tool_manifest_metadata_errors_are_reported(self):
        errors = MODULE.validate_tool_manifest({
            "schemaVersion": "2.0",
            "tools": [
                {"name": "driver_initialize", "mutation": True, "sensitive": False, "deprecated": False},
                {"name": "driver_initialize", "mutation": True, "sensitive": False, "deprecated": False},
                {"name": "browser_get_page_dom", "mutation": False, "sensitive": "yes"},
                {"mutation": False, "sensitive": False, "deprecated": False},
            ],
        })

        self.assertIn("MCP tool manifest schemaVersion must remain 1.0", errors)
        self.assertIn("MCP tool manifest contains duplicate tool: driver_initialize", errors)
        self.assertIn("MCP tool manifest tool browser_get_page_dom must define boolean sensitive", errors)
        self.assertIn("MCP tool manifest tool browser_get_page_dom must define boolean deprecated", errors)
        self.assertIn("MCP tool manifest contains a tool without a name", errors)

    def test_selector_bound_ignores_yaml_comments(self):
        workflow = (
            "# leading comment\n"
            "jobs:\n"
            "  mcp-tests:\n"
            "    steps:\n"
            "      # another comment with extra # characters ##\n"
            "      - env:\n"
            "          MCP_RELATED_TESTS: ATest#one,ATest#two,BTest#three\n"
        )

        self.assertEqual(MODULE.validate_mcp_test_selector(workflow), [])

    def test_selector_bound_rejects_more_than_ten_methods(self):
        selector = ",".join(f"ATest#method{index}" for index in range(11))
        workflow = f"          MCP_RELATED_TESTS: {selector}\n"

        errors = MODULE.validate_mcp_test_selector(workflow)

        self.assertEqual(
            errors,
            ["shaft-mcp workflow MCP test selector must stay bounded to at most 10 selected test methods"],
        )

    def test_selector_bound_requires_explicit_method_entries(self):
        missing = MODULE.validate_mcp_test_selector("jobs: {}\n")
        class_only = MODULE.validate_mcp_test_selector("MCP_RELATED_TESTS: ATest,BTest\n")

        self.assertEqual(
            missing,
            ["shaft-mcp workflow must define a single-line MCP_RELATED_TESTS selector"],
        )
        self.assertEqual(
            class_only,
            ["shaft-mcp workflow MCP test selector must select explicit Class#method entries"],
        )


class ValidateShaftMcpConfigurationFullContractTest(unittest.TestCase):
    def test_minimal_root_satisfies_full_contract(self):
        with tempfile.TemporaryDirectory() as temp:
            root = Path(temp)
            _write_minimal_mcp_root(root)

            errors = MODULE.validate(root)

        self.assertEqual([], errors)

    def test_reactor_membership_is_enforced(self):
        with tempfile.TemporaryDirectory() as temp:
            root = Path(temp)
            _write_minimal_mcp_root(root)
            _write_text(
                root / "pom.xml",
                ROOT_POM.replace("<module>shaft-mcp</module>\n        ", ""),
            )

            errors = MODULE.validate(root)

        self.assertIn("root reactor must include shaft-mcp", errors)

    def test_shaft_mcp_pom_identity_is_enforced(self):
        with tempfile.TemporaryDirectory() as temp:
            root = Path(temp)
            _write_minimal_mcp_root(root)
            _write_text(
                root / "shaft-mcp/pom.xml",
                MCP_POM.replace("<artifactId>shaft-mcp</artifactId>", "<artifactId>shaft-mcp-server</artifactId>", 1),
            )

            errors = MODULE.validate(root)

        self.assertIn("shaft-mcp must use the shaft-mcp artifactId", errors)

    def test_installer_removal_is_enforced(self):
        with tempfile.TemporaryDirectory() as temp:
            root = Path(temp)
            _write_minimal_mcp_root(root)
            _write_text(
                root / "shaft-mcp/src/main/java/com/shaft/mcp/install/ShaftMcpInstaller.java",
                "package com.shaft.mcp.install;\n\npublic class ShaftMcpInstaller {}\n",
            )

            errors = MODULE.validate(root)

        self.assertIn("shaft-mcp Java installer sources and tests must be removed", errors)

    def test_engine_must_not_depend_on_mcp(self):
        with tempfile.TemporaryDirectory() as temp:
            root = Path(temp)
            _write_minimal_mcp_root(root)
            _write_text(
                root / "shaft-engine/pom.xml",
                ENGINE_POM.replace(
                    "<artifactId>shaft-engine</artifactId>",
                    "<artifactId>shaft-engine</artifactId>\n    <artifactId>shaft-mcp</artifactId>",
                ),
            )

            errors = MODULE.validate(root)

        self.assertIn("shaft-engine must not depend on shaft-mcp", errors)

    def test_shaft_bom_must_manage_shaft_mcp(self):
        with tempfile.TemporaryDirectory() as temp:
            root = Path(temp)
            _write_minimal_mcp_root(root)
            _write_text(
                root / "shaft-bom/pom.xml",
                """<project xmlns="http://maven.apache.org/POM/4.0.0">
    <modelVersion>4.0.0</modelVersion>
    <artifactId>shaft-bom</artifactId>
    <dependencyManagement>
        <dependencies>
        </dependencies>
    </dependencyManagement>
</project>
""",
            )

            errors = MODULE.validate(root)

        self.assertIn("shaft-bom must manage shaft-mcp without adding it as a dependency", errors)

    def test_stdio_properties_are_enforced(self):
        with tempfile.TemporaryDirectory() as temp:
            root = Path(temp)
            _write_minimal_mcp_root(root)
            _write_text(
                root / "shaft-mcp/src/main/resources/application.properties",
                STDIO_PROPERTIES.replace(
                    "spring.main.web-application-type=none",
                    "spring.main.web-application-type=servlet",
                ),
            )

            errors = MODULE.validate(root)

        self.assertIn("stdio configuration must set spring.main.web-application-type=none", errors)

    def test_http_properties_are_enforced(self):
        with tempfile.TemporaryDirectory() as temp:
            root = Path(temp)
            _write_minimal_mcp_root(root)
            _write_text(
                root / "shaft-mcp/src/main/resources/application-http.properties",
                HTTP_PROPERTIES.replace(
                    "spring.main.web-application-type=servlet",
                    "spring.main.web-application-type=none",
                ),
            )

            errors = MODULE.validate(root)

        self.assertIn("HTTP configuration must set spring.main.web-application-type=servlet", errors)

    def test_logback_must_target_stderr(self):
        with tempfile.TemporaryDirectory() as temp:
            root = Path(temp)
            _write_minimal_mcp_root(root)
            _write_text(
                root / "shaft-mcp/src/main/resources/logback-spring.xml",
                "<configuration></configuration>\n",
            )

            errors = MODULE.validate(root)

        self.assertIn("shaft-mcp logging must target stderr", errors)

    def test_server_json_must_derive_version_from_reactor(self):
        with tempfile.TemporaryDirectory() as temp:
            root = Path(temp)
            _write_minimal_mcp_root(root)
            _write_text(
                root / "shaft-mcp/server.json",
                """{"name": "shaft-mcp", "version": "@project.version@"}\n""",
            )

            errors = MODULE.validate(root)

        self.assertIn("MCP registry metadata must derive its version from the reactor", errors)

    def test_tool_manifest_must_cover_lean_remediation_tools(self):
        with tempfile.TemporaryDirectory() as temp:
            root = Path(temp)
            _write_minimal_mcp_root(root)
            _write_text(
                root / "shaft-mcp/src/test/resources/fixtures/mcp-tool-manifest.json",
                _tool_manifest(omit_tool="element_type"),
            )

            errors = MODULE.validate(root)

        self.assertIn("MCP tool manifest is missing lean remediation tools: ['element_type']", errors)

    def test_workflow_layout_and_schedule_are_enforced(self):
        with tempfile.TemporaryDirectory() as temp:
            root = Path(temp)
            _write_minimal_mcp_root(root)
            _write_text(
                root / ".github/workflows/shaft-mcp.yml",
                SHAFT_MCP_WORKFLOW.replace("  schedule:\n    - cron: '00 1 * * *'\n", ""),
            )

            errors = MODULE.validate(root)

        self.assertIn(
            "shaft-mcp workflow must run manually and once daily with local E2E workflows", errors
        )

    def test_mcp_jar_build_composite_action_copy_contract_is_enforced(self):
        with tempfile.TemporaryDirectory() as temp:
            root = Path(temp)
            _write_minimal_mcp_root(root)
            (root / ".github/actions/mcp-jar-build/action.yml").unlink()

            errors = MODULE.validate(root)

        self.assertIn(
            "mcp-jar-build composite action must copy shaft-mcp runtime dependencies to the root target/dependency path",
            errors,
        )

    def test_python_installer_must_contain_required_standalone_support(self):
        with tempfile.TemporaryDirectory() as temp:
            root = Path(temp)
            _write_minimal_mcp_root(root)
            _write_text(
                root / "scripts/mcp/install_shaft_mcp.py",
                INSTALL_SHAFT_MCP_PY.replace(
                    '# USER_GUIDE_URL = "https://example.invalid/guide"\n', ""
                ),
            )

            errors = MODULE.validate(root)

        self.assertIn(
            "shared Python MCP installer must contain standalone support for USER_GUIDE_URL", errors
        )

    def test_installer_bootstrap_must_stay_a_thin_python_wrapper(self):
        with tempfile.TemporaryDirectory() as temp:
            root = Path(temp)
            _write_minimal_mcp_root(root)
            _write_text(
                root / "scripts/mcp/install-shaft-mcp.ps1",
                INSTALL_PS1.replace(
                    "# downloads python-build-standalone runtime and shows progress\n",
                    "# downloads a runtime and shows progress\n",
                ),
            )

            errors = MODULE.validate(root)

        self.assertIn(
            "scripts/mcp/install-shaft-mcp.ps1 must be a thin Python bootstrapper containing python-build-standalone",
            errors,
        )

    def test_public_installer_verification_must_assert_argfile_entry(self):
        with tempfile.TemporaryDirectory() as temp:
            root = Path(temp)
            _write_minimal_mcp_root(root)
            _write_text(
                root / "scripts/ci/verify_shaft_mcp_installer_release.py",
                VERIFY_INSTALLER_PY.replace(
                    "# asserts shaft.mcp.fallbackWorkspaceRoot argfile entry:\n", ""
                ),
            )

            errors = MODULE.validate(root)

        self.assertIn(
            "public shaft-mcp installer verification must assert argfile entry: shaft.mcp.fallbackWorkspaceRoot",
            errors,
        )

    def test_dockerfile_must_launch_thin_classpath_not_java_jar(self):
        with tempfile.TemporaryDirectory() as temp:
            root = Path(temp)
            _write_minimal_mcp_root(root)
            _write_text(
                root / "shaft-mcp/Dockerfile",
                DOCKERFILE + '# do not use "-jar" here\n',
            )

            errors = MODULE.validate(root)

        self.assertIn(
            "Dockerfile must launch the thin shaft-mcp classpath, not java -jar", errors
        )


if __name__ == "__main__":
    unittest.main()
