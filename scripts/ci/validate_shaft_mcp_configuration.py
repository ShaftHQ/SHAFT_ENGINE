#!/usr/bin/env python3
"""Validate shaft-mcp reactor, transport, metadata, and workflow integration."""

from __future__ import annotations

import sys
import xml.etree.ElementTree as ET
import json
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
NS = {"m": "http://maven.apache.org/POM/4.0.0"}


def text(element: ET.Element, path: str) -> str | None:
    value = element.findtext(path, namespaces=NS)
    return value.strip() if value else None


def property_map(path: Path) -> dict[str, str]:
    properties: dict[str, str] = {}
    for raw_line in path.read_text(encoding="utf-8").splitlines():
        line = raw_line.strip()
        if not line or line.startswith("#") or "=" not in line:
            continue
        key, value = line.split("=", 1)
        properties[key.strip()] = value.strip()
    return properties


def validate_tool_manifest(manifest: dict[str, object]) -> list[str]:
    errors: list[str] = []
    if manifest.get("schemaVersion") != "1.0":
        errors.append("MCP tool manifest schemaVersion must remain 1.0")
    seen_tools: set[str] = set()
    tools = manifest.get("tools", [])
    if not isinstance(tools, list):
        return errors + ["MCP tool manifest tools must be a list"]
    for tool in tools:
        if not isinstance(tool, dict):
            errors.append("MCP tool manifest contains a non-object tool entry")
            continue
        name = tool.get("name")
        if not isinstance(name, str) or not name:
            errors.append("MCP tool manifest contains a tool without a name")
            continue
        if name in seen_tools:
            errors.append(f"MCP tool manifest contains duplicate tool: {name}")
        seen_tools.add(name)
        for key in ("mutation", "sensitive", "deprecated"):
            if not isinstance(tool.get(key), bool):
                errors.append(f"MCP tool manifest tool {name} must define boolean {key}")
    return errors


def validate(root: Path = ROOT) -> list[str]:
    errors: list[str] = []
    parent = ET.parse(root / "pom.xml").getroot()
    modules = {
        (module.text or "").strip()
        for module in parent.findall("m:modules/m:module", NS)
    }
    if "shaft-mcp" not in modules:
        errors.append("root reactor must include shaft-mcp")

    mcp = ET.parse(root / "shaft-mcp" / "pom.xml").getroot()
    if text(mcp, "m:artifactId") != "shaft-mcp":
        errors.append("shaft-mcp must use the shaft-mcp artifactId")
    dependencies = {
        (text(dependency, "m:artifactId"), text(dependency, "m:version"))
        for dependency in mcp.findall("m:dependencies/m:dependency", NS)
    }
    if ("shaft-engine", "${project.version}") not in dependencies:
        errors.append("shaft-mcp must use the canonical in-reactor shaft-engine dependency")
    if ("shaft-doctor", "${project.version}") not in dependencies:
        errors.append("shaft-mcp must use the in-reactor shaft-doctor dependency")
    if ("shaft-ai", "${project.version}") not in dependencies:
        errors.append("shaft-mcp must package the in-reactor direct provider adapters")
    mcp_pom_text = (root / "shaft-mcp" / "pom.xml").read_text(encoding="utf-8")
    if "shaft_engine.version" in mcp_pom_text:
        errors.append("shaft-mcp must not define an independent SHAFT engine version")
    if "<Implementation-Version>${project.version}</Implementation-Version>" not in mcp_pom_text:
        errors.append("shaft-mcp executable manifest must expose the reactor Implementation-Version")
    if "spring-boot-maven-plugin" in mcp_pom_text or "<goal>repackage</goal>" in mcp_pom_text:
        errors.append("shaft-mcp must publish a thin JAR without Spring Boot repackaging")
    if "META-INF/shaft-mcp/runtime-dependencies.txt" not in mcp_pom_text:
        errors.append("shaft-mcp must package its resolved runtime dependency manifest")
    if "<parameters>true</parameters>" not in mcp_pom_text:
        errors.append("shaft-mcp compiler must retain Java parameter names for MCP tool schemas")

    application_source = (
        root / "shaft-mcp/src/main/java/com/shaft/mcp/ShaftMcpApplication.java"
    ).read_text(encoding="utf-8")
    if '"install".equalsIgnoreCase(args[0])' in application_source or "ShaftMcpInstaller" in application_source:
        errors.append("shaft-mcp installation must be owned by scripts, not the packaged JAR")
    installer_sources = list((root / "shaft-mcp/src/main/java/com/shaft/mcp/install").glob("*.java"))
    installer_tests = list((root / "shaft-mcp/src/test/java/com/shaft/mcp/install").glob("*.java"))
    if installer_sources or installer_tests:
        errors.append("shaft-mcp Java installer sources and tests must be removed")

    engine_pom = (root / "shaft-engine" / "pom.xml").read_text(encoding="utf-8")
    if "<artifactId>shaft-mcp</artifactId>" in engine_pom:
        errors.append("shaft-engine must not depend on shaft-mcp")

    bom = ET.parse(root / "shaft-bom" / "pom.xml").getroot()
    bom_artifacts = {
        text(dependency, "m:artifactId")
        for dependency in bom.findall("m:dependencyManagement/m:dependencies/m:dependency", NS)
    }
    if "shaft-mcp" not in bom_artifacts:
        errors.append("shaft-bom must manage shaft-mcp without adding it as a dependency")

    stdio = property_map(root / "shaft-mcp" / "src/main/resources/application.properties")
    expected_stdio = {
        "spring.main.web-application-type": "none",
        "spring.ai.mcp.server.stdio": "true",
        "spring.ai.mcp.server.version": "@project.version@",
        "spring.main.banner-mode": "off",
    }
    for key, expected in expected_stdio.items():
        if stdio.get(key) != expected:
            errors.append(f"stdio configuration must set {key}={expected}")

    http = property_map(root / "shaft-mcp" / "src/main/resources/application-http.properties")
    expected_http = {
        "spring.main.web-application-type": "servlet",
        "spring.ai.mcp.server.stdio": "false",
        "spring.ai.mcp.server.protocol": "STREAMABLE",
        "spring.ai.mcp.server.streamable-http.mcp-endpoint": "/mcp",
        "spring.ai.mcp.server.version": "@project.version@",
    }
    for key, expected in expected_http.items():
        if http.get(key) != expected:
            errors.append(f"HTTP configuration must set {key}={expected}")

    logback = (root / "shaft-mcp/src/main/resources/logback-spring.xml").read_text(encoding="utf-8")
    if "<target>System.err</target>" not in logback:
        errors.append("shaft-mcp logging must target stderr")

    server_json = (root / "shaft-mcp/server.json").read_text(encoding="utf-8")
    if server_json.count("@project.version@") < 2:
        errors.append("MCP registry metadata must derive its version from the reactor")

    manifest_path = root / "shaft-mcp/src/test/resources/fixtures/mcp-tool-manifest.json"
    manifest = json.loads(manifest_path.read_text(encoding="utf-8"))
    errors.extend(validate_tool_manifest(manifest))
    tools = {tool["name"] for tool in manifest.get("tools", [])}
    required_tools = {
        "doctor_analyze_failed_allure",
        "doctor_suggest_fix",
        "capture_checkpoint",
        "capture_generate_replay",
        "capture_code_blocks",
        "browser_get_page_dom",
        "browser_take_screenshot",
        "playwright_initialize",
        "playwright_browser_navigate",
        "playwright_browser_get_page_dom",
        "playwright_browser_take_screenshot",
        "playwright_record_start",
        "playwright_recording_code_blocks",
        "playwright_replay_recording",
        "playwright_element_click",
        "playwright_element_type",
        "playwright_capture_code_blocks",
        "playwright_doctor_analyze_failed_allure",
        "playwright_healer_run_failed_test",
        "mobile_initialize_web_emulation",
        "mobile_initialize_native",
        "mobile_get_contexts",
        "mobile_get_accessibility_tree",
        "mobile_take_screenshot",
        "mobile_tap",
        "mobile_type",
        "mobile_record_start",
        "mobile_recording_code_blocks",
        "mobile_replay_recording",
        "element_click",
        "element_type",
    }
    missing_tools = required_tools - tools
    if missing_tools:
        errors.append(f"MCP tool manifest is missing lean remediation tools: {sorted(missing_tools)}")
    removed_tools = {
        "doctor_publish_draft_pr",
        "browser_get_page_source",
        "browser_get_cookie",
        "browser_get_all_cookies",
        "element_click_semantic",
        "element_type_semantic",
        "element_click_ai",
        "element_type_ai",
    }
    present_removed_tools = removed_tools & tools
    if present_removed_tools:
        errors.append(f"MCP tool manifest still exposes removed/sensitive tools: {sorted(present_removed_tools)}")

    nested_workflows = list((root / "shaft-mcp/.github/workflows").glob("*.yml"))
    if nested_workflows:
        errors.append("shaft-mcp workflows must live under the root .github/workflows directory")
    for workflow in ("shaft-mcp.yml", "publish-shaft-mcp.yml", "deploy-shaft-mcp.yml"):
        if not (root / ".github/workflows" / workflow).is_file():
            errors.append(f"missing root MCP workflow: {workflow}")
    mcp_workflow = (root / ".github/workflows/shaft-mcp.yml").read_text(encoding="utf-8")
    if "workflow_dispatch:" not in mcp_workflow or "cron: '00 1 * * *'" not in mcp_workflow:
        errors.append("shaft-mcp workflow must run manually and once daily with local E2E workflows")
    if "pull_request:" in mcp_workflow or "\n  push:" in mcp_workflow:
        errors.append("shaft-mcp workflow must not run on pull_request or push")
    for runner in ("ubuntu-22.04", "macos-15", "windows-2025"):
        if runner not in mcp_workflow:
            errors.append(f"shaft-mcp installer script verification must run on {runner}")
    for required_mcp_test_token in ("MCP_RELATED_TESTS", '-Dtest="${MCP_RELATED_TESTS}"', "-DheadlessExecution=true"):
        if required_mcp_test_token not in mcp_workflow:
            errors.append(f"shaft-mcp workflow must run a bounded headless MCP test selector containing {required_mcp_test_token}")
    if mcp_workflow.count("#") > 10:
        errors.append("shaft-mcp workflow MCP test selector must stay bounded to at most 10 selected test methods")
    central_workflow = (root / ".github/workflows/mavenCentral_cd.yml").read_text(encoding="utf-8")
    pilot_release_workflow = (root / ".github/workflows/shaft-pilot-release.yml").read_text(encoding="utf-8")
    if "verify_shaft_mcp_installer_release.py" not in central_workflow:
        errors.append("Maven Central delivery must verify the public shaft-mcp LATEST installer")
    if "needs: [build_release_and_deliver, verify_public_shaft_mcp_installer]" not in central_workflow:
        errors.append("release announcements must wait for the public shaft-mcp installer matrix")
    dependency_output = "-DoutputDirectory=${maven.multiModuleProjectDirectory}/shaft-mcp/target/dependency"
    for workflow_name, workflow_text in (
            ("shaft-mcp.yml", mcp_workflow),
            ("mavenCentral_cd.yml", central_workflow),
            ("shaft-pilot-release.yml", pilot_release_workflow),
    ):
        if "dependency:copy-dependencies" not in workflow_text or dependency_output not in workflow_text:
            errors.append(f"{workflow_name} must copy shaft-mcp runtime dependencies to the root target/dependency path")
    if "-pl shaft-mcp -am install" not in mcp_workflow:
        errors.append("shaft-mcp workflow must install reactor artifacts before copying thin runtime dependencies")
    installer_implementation = root / "scripts/mcp/install_shaft_mcp.py"
    if not installer_implementation.is_file():
        errors.append("missing shared Python MCP installer implementation: scripts/mcp/install_shaft_mcp.py")
        installer_implementation_text = ""
    else:
        installer_implementation_text = installer_implementation.read_text(encoding="utf-8")
        for required in ("maven-metadata.xml", ".sha256", "runtime-dependencies.txt", "shaft-mcp.args",
                         "shaft.mcp.workspaceRoot", "ThreadPoolExecutor", "USER_GUIDE_URL",
                         "copilot-intellij", "claude-desktop", "probe_stdio"):
            if required not in installer_implementation_text:
                errors.append(f"shared Python MCP installer must contain standalone support for {required}")
        if '"-jar", str(jar)' in installer_implementation_text or '"-jar", str(args_file)' in installer_implementation_text:
            errors.append("shared Python MCP installer must configure the thin classpath argfile, not java -jar")
        for legacy_client in ("codex-app", "copilot-vscode"):
            if legacy_client in installer_implementation_text:
                errors.append(f"shared Python MCP installer must not contain removed client target: {legacy_client}")
    for installer in ("scripts/mcp/install-shaft-mcp.ps1", "scripts/mcp/install-shaft-mcp.sh"):
        if not (root / installer).is_file():
            errors.append(f"missing standalone MCP installer bootstrap: {installer}")
        else:
            installer_text = (root / installer).read_text(encoding="utf-8")
            forbidden = (
                "maven-metadata.xml",
                ".sha256",
                "maven-dependency-plugin",
                "exec-maven-plugin",
                "Download-Maven",
                "Get-Maven",
                "SHAFT_MCP_FORCE_BOOTSTRAP_MAVEN",
                "java -jar $jar install",
                "-jar $jar install",
                "-jar $Jar install",
                "codex-app",
                "copilot-vscode",
            )
            for token in forbidden:
                if token in installer_text:
                    errors.append(f"{installer} must not contain implementation or legacy installer token: {token}")
            for required in ("install_shaft_mcp.py", "python-build-standalone"):
                if required not in installer_text:
                    errors.append(f"{installer} must be a thin Python bootstrapper containing {required}")
            if "progress" not in installer_text:
                errors.append(f"{installer} must show download progress during bootstrap downloads")
    public_installer = (root / "scripts/ci/verify_shaft_mcp_installer_release.py").read_text(encoding="utf-8")
    if "exec-maven-plugin" in public_installer:
        errors.append("public shaft-mcp installer verification must not depend on the Maven exec plugin")
    if "SHAFT_MCP_FORCE_BOOTSTRAP_MAVEN" in public_installer:
        errors.append("public shaft-mcp installer verification must not exercise Maven bootstrapping")
    for required_client in ("claude-desktop", "copilot-intellij"):
        if required_client not in public_installer:
            errors.append(f"public shaft-mcp installer verification must cover {required_client}")
    for required_token in ("-Duser.dir", "shaft.mcp.workspaceRoot"):
        if required_token not in public_installer:
            errors.append(f"public shaft-mcp installer verification must assert argfile entry: {required_token}")

    for dockerfile in (root / "shaft-mcp").glob("Dockerfile*"):
        content = dockerfile.read_text(encoding="utf-8")
        if "repo1.maven.org" in content or "shaft-mcp/10." in content:
            errors.append(f"{dockerfile.name} must build from the reactor without a hardcoded release")
        if "-pl shaft-mcp -am" not in content:
            errors.append(f"{dockerfile.name} must build shaft-mcp from the root reactor")
        if "-pl shaft-mcp -am install" not in content:
            errors.append(f"{dockerfile.name} must install reactor artifacts before copying thin runtime dependencies")
        if "dependency:copy-dependencies" not in content or "-DoutputDirectory=/build/shaft-mcp-lib" not in content:
            errors.append(f"{dockerfile.name} must copy shaft-mcp runtime dependencies into /build/shaft-mcp-lib")
        if "google-chrome-stable" not in content:
            errors.append(f"{dockerfile.name} must install Chrome and its runtime dependencies")
        if '"-DheadlessExecution=true"' not in content:
            errors.append(f"{dockerfile.name} must launch Chrome headlessly in its container")
        if '"-jar"' in content or "BOOT-INF/lib" in content:
            errors.append(f"{dockerfile.name} must launch the thin shaft-mcp classpath, not java -jar")
        if '"/app/shaft-mcp.jar:/app/lib/*"' not in content:
            errors.append(f"{dockerfile.name} must include copied runtime dependencies on the classpath")
    return errors


def main() -> int:
    errors = validate()
    if errors:
        print("\n".join(errors), file=sys.stderr)
        return 1
    print("shaft-mcp reactor, transport, metadata, container, and workflow configuration is valid.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
