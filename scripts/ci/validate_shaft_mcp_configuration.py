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

    application_source = (
        root / "shaft-mcp/src/main/java/com/shaft/mcp/ShaftMcpApplication.java"
    ).read_text(encoding="utf-8")
    if '"install".equalsIgnoreCase(args[0])' not in application_source:
        errors.append("shaft-mcp must route the install command before Spring MCP startup")

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
    tools = {tool["name"] for tool in manifest.get("tools", [])}
    required_tools = {
        "doctor_analyze_failed_allure",
        "doctor_suggest_fix",
        "capture_checkpoint",
        "capture_generate_replay",
        "capture_code_blocks",
        "browser_get_page_dom",
        "browser_take_screenshot",
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
        "element_click_semantic",
        "element_type_semantic",
    }
    missing_tools = required_tools - tools
    if missing_tools:
        errors.append(f"MCP tool manifest is missing lean remediation tools: {sorted(missing_tools)}")
    removed_tools = {
        "doctor_publish_draft_pr",
        "browser_get_page_source",
        "browser_get_cookie",
        "browser_get_all_cookies",
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
            errors.append(f"shaft-mcp installer tests must run on {runner}")
    central_workflow = (root / ".github/workflows/mavenCentral_cd.yml").read_text(encoding="utf-8")
    if "verify_shaft_mcp_installer_release.py" not in central_workflow:
        errors.append("Maven Central delivery must verify the public shaft-mcp LATEST installer")
    if "needs: [build_release_and_deliver, verify_public_shaft_mcp_installer]" not in central_workflow:
        errors.append("release announcements must wait for the public shaft-mcp installer matrix")

    for dockerfile in (root / "shaft-mcp").glob("Dockerfile*"):
        content = dockerfile.read_text(encoding="utf-8")
        if "repo1.maven.org" in content or "shaft-mcp/10." in content:
            errors.append(f"{dockerfile.name} must build from the reactor without a hardcoded release")
        if "-pl shaft-mcp -am" not in content:
            errors.append(f"{dockerfile.name} must build shaft-mcp from the root reactor")
        if "google-chrome-stable" not in content:
            errors.append(f"{dockerfile.name} must install Chrome and its runtime dependencies")
        if '"-DheadlessExecution=true"' not in content:
            errors.append(f"{dockerfile.name} must launch Chrome headlessly in its container")
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
