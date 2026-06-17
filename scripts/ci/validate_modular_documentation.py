#!/usr/bin/env python3
"""Validate modular examples, source metadata, fixtures, and canonical docs links."""

from pathlib import Path
import re
import sys
import xml.etree.ElementTree as ET

ROOT = Path(__file__).resolve().parents[2]
EXAMPLES = ROOT / "shaft-engine/src/main/resources/examples"
MCP_FIXTURES = ROOT / "shaft-mcp/src/test/resources/fixtures/shaft-pilot/mcp"
NS = {"m": "http://maven.apache.org/POM/4.0.0"}
DOCS_BASE = "https://shaftengine.netlify.app/docs"
EXPECTED_OPTIONAL = {
    "shaft-cucumber-web": "shaft-visual",
    "shaft-junit-web": "shaft-visual",
    "shaft-testng-web": "shaft-visual",
}


def fail(message: str) -> None:
    print(f"ERROR: {message}", file=sys.stderr)
    raise SystemExit(1)


def text(node, path):
    return (node.findtext(path, default="", namespaces=NS) or "").strip()


def parse_trusted_pom(path: Path):
    # Repository-owned Maven POMs are trusted validator inputs.
    return ET.parse(path).getroot()  # nosec B314


def main() -> None:
    reactor_version = text(parse_trusted_pom(ROOT / "pom.xml"), "m:version")
    poms = sorted(EXAMPLES.rglob("pom.xml"))
    if len(poms) != 7:
        fail(f"expected seven bundled example POMs, found {len(poms)}")

    for pom in poms:
        root = parse_trusted_pom(pom)
        artifact = text(root, "m:artifactId")
        props = root.find("m:properties", NS)
        prop_names = {
            child.tag.rsplit("}", 1)[-1]
            for child in list(props)
            if props is not None
        }
        if "shaft.version" not in prop_names or "shaft_engine.version" in prop_names:
            fail(f"{pom}: use <shaft.version> only")
        if text(props, "m:shaft.version") != reactor_version:
            fail(f"{pom}: shaft.version must match reactor version {reactor_version}")
        managed = {
            text(dependency, "m:artifactId")
            for dependency in root.findall(
                "m:dependencyManagement/m:dependencies/m:dependency", NS
            )
        }
        if "shaft-bom" not in managed:
            fail(f"{pom}: import shaft-bom")
        deps = {
            text(dependency, "m:artifactId"): text(dependency, "m:version")
            for dependency in root.findall("m:dependencies/m:dependency", NS)
        }
        if "SHAFT_ENGINE" in deps or "shaft-engine" not in deps:
            fail(f"{pom}: declare shaft-engine and not SHAFT_ENGINE")
        if deps["shaft-engine"]:
            fail(f"{pom}: shaft-engine version must come from shaft-bom")
        optional = {
            name
            for name in deps
            if name in {"shaft-browserstack", "shaft-video", "shaft-visual"}
        }
        expected = {EXPECTED_OPTIONAL[artifact]} if artifact in EXPECTED_OPTIONAL else set()
        if optional != expected:
            fail(
                f"{pom}: expected optional modules {sorted(expected)}, "
                f"found {sorted(optional)}"
            )

    for pom in sorted((ROOT / "tools/modularization/consumer-fixtures").glob("*/pom.xml")):
        fixture_version = text(
            parse_trusted_pom(pom).find("m:properties", NS),
            "m:shaft.version",
        )
        if fixture_version != reactor_version:
            fail(f"{pom}: shaft.version must match reactor version {reactor_version}")

    internal = (
        ROOT / "shaft-engine/src/main/java/com/shaft/properties/internal/Internal.java"
    ).read_text(encoding="utf-8")
    version_match = re.search(
        r'@DefaultValue\("([^"]+)"\)\s+String shaftEngineVersion\(\);',
        internal,
    )
    if not version_match or version_match.group(1) != reactor_version:
        fail("Internal.shaftEngineVersion must match the reactor version")

    canonical_links = {
        ROOT / "README.md": (
            f"{DOCS_BASE}/start/installation",
            f"{DOCS_BASE}/start/upgrade",
            f"{DOCS_BASE}/agentic/mcp",
            f"{DOCS_BASE}/agentic/doctor",
            f"{DOCS_BASE}/agentic/heal",
            f"{DOCS_BASE}/testing/web",
            f"{DOCS_BASE}/testing/mobile",
            f"{DOCS_BASE}/testing/api",
        ),
        ROOT / ".github/RELEASE_BODY_TEMPLATE.md": (
            f"{DOCS_BASE}/start/upgrade",
            f"{DOCS_BASE}/agentic/pilot",
        ),
        ROOT / "legacy-shaft-engine/pom.xml": (f"{DOCS_BASE}/start/upgrade",),
    }
    for path, required_links in canonical_links.items():
        contents = path.read_text(encoding="utf-8")
        for link in required_links:
            if link not in contents:
                fail(f"{path}: missing canonical documentation link {link}")
        if "github.com/ShaftHQ/SHAFT_ENGINE/blob/main/docs/" in contents:
            fail(f"{path}: contains a deleted local documentation link")

    for fixture in (
        "codex-config.toml",
        "claude-desktop.json",
        "gemini-settings.json",
        "vscode-mcp.json",
    ):
        contents = (MCP_FIXTURES / fixture).read_text(encoding="utf-8")
        if "shaft-mcp.args" not in contents:
            fail(f"{fixture}: missing shaft-mcp argfile command")
        if "-jar" in contents or "shaft-mcp-<version>.jar" in contents:
            fail(f"{fixture}: MCP client fixture must use the thin classpath argfile")
        if "API_KEY" in contents or "apiKey" in contents:
            fail(f"{fixture}: MCP client fixture must not request a provider API key")

    doctor_invocations = (
        MCP_FIXTURES / "doctor-analyze-invocations.json"
    ).read_text(encoding="utf-8")
    for term in (
        "doctor_analyze",
        "ChatGPT",
        "Codex",
        "Claude",
        "Gemini",
        "GitHub Copilot",
    ):
        if term not in doctor_invocations:
            fail(f"doctor-analyze-invocations.json: missing {term!r}")
    if "API_KEY" in doctor_invocations or "apiKey" in doctor_invocations:
        fail("doctor-analyze-invocations.json must not request provider credentials")

    readme = (ROOT / "README.md").read_text(encoding="utf-8")
    if "maven-central/v/io.github.shafthq/SHAFT_ENGINE" in readme:
        fail("README Maven Central badge still targets the legacy coordinate")

    properties_helper = (
        ROOT / "shaft-engine/src/main/java/com/shaft/properties/internal/PropertiesHelper.java"
    ).read_text(encoding="utf-8")
    setup_page = (
        ROOT / "shaft-engine/src/main/javadoc/resources/index.html"
    ).read_text(encoding="utf-8")
    if "refs/heads/main/src/main/resources/" in properties_helper:
        fail("runtime property downloads still use the pre-reactor resource path")
    if "refs/heads/main/src/main/resources/" in setup_page:
        fail("setup-page downloads still use the pre-reactor resource path")
    if (
        "refs/heads/main/shaft-engine/src/main/resources/properties/default/"
        not in properties_helper
    ):
        fail("runtime property download path is missing")
    if "refs/heads/main/shaft-engine/src/main/resources/examples/" not in setup_page:
        fail("setup-page example download paths are missing")

    example_workflows = {
        name: (EXAMPLES / ".github/workflows" / name).read_text(encoding="utf-8")
        for name in ("api.yml", "web.yml")
    }
    for name, workflow in example_workflows.items():
        if "actions/checkout@v6" not in workflow:
            fail(f"{name}: use the current checkout action")
        if "Set up JDK 25" not in workflow or "java-version: '25'" not in workflow:
            fail(f"{name}: use the repository JDK 25 baseline")
    web_workflow = example_workflows["web.yml"]
    if "refs/heads/main/src/main/resources/" in web_workflow:
        fail("web example workflow still uses the pre-reactor resource path")
    if (
        "refs/heads/main/shaft-engine/src/main/resources/docker-compose/selenium4.yml"
        not in web_workflow
    ):
        fail("web example workflow Docker Compose download path is missing")

    sync_workflow = (
        ROOT / ".github/workflows/sync-sample-projects-version.yml"
    ).read_text(encoding="utf-8")
    if 'extract_property_version "jdk.version"' not in sync_workflow:
        fail("sample sync workflow must read jdk.version from the root properties")
    if "Updated all example pom.xml files to use SHAFT_ENGINE" in sync_workflow:
        fail("sample sync workflow still describes the legacy artifact")

    print("Modular examples and canonical documentation pointers are valid.")


if __name__ == "__main__":
    main()
