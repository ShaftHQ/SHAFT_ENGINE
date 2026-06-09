#!/usr/bin/env python3
"""Validate bundled examples and user-facing modular SHAFT migration docs."""
from pathlib import Path
import re
import sys
import xml.etree.ElementTree as ET

ROOT = Path(__file__).resolve().parents[2]
EXAMPLES = ROOT / "shaft-engine/src/main/resources/examples"
NS = {"m": "http://maven.apache.org/POM/4.0.0"}
EXPECTED_OPTIONAL = {
    "shaft-cucumber-web": "shaft-visual",
    "shaft-junit-web": "shaft-video",
    "shaft-testng-web": "shaft-browserstack",
}
REQUIRED_GUIDE_TERMS = (
    "io.github.shafthq:SHAFT_ENGINE", "io.github.shafthq:shaft-engine",
    "shaft-bom", "shaft-browserstack", "shaft-video", "shaft-visual",
    "API", "Appium", "database", "relocation", "cache", "Rollback",
    "Linux x64", "Linux ARM64", "Windows x64", "macOS x64", "macOS ARM64",
)

def fail(message: str) -> None:
    print(f"ERROR: {message}", file=sys.stderr)
    raise SystemExit(1)

def text(node, path):
    return (node.findtext(path, default="", namespaces=NS) or "").strip()

def main() -> None:
    reactor_version = text(ET.parse(ROOT / "pom.xml").getroot(), "m:version")
    poms = sorted(EXAMPLES.rglob("pom.xml"))
    if len(poms) != 7:
        fail(f"expected seven bundled example POMs, found {len(poms)}")
    for pom in poms:
        root = ET.parse(pom).getroot()
        artifact = text(root, "m:artifactId")
        props = root.find("m:properties", NS)
        prop_names = {child.tag.rsplit("}", 1)[-1] for child in list(props) if props is not None}
        if "shaft.version" not in prop_names or "shaft_engine.version" in prop_names:
            fail(f"{pom}: use <shaft.version> only")
        if text(props, "m:shaft.version") != reactor_version:
            fail(f"{pom}: shaft.version must match reactor version {reactor_version}")
        managed = {text(d, "m:artifactId") for d in root.findall("m:dependencyManagement/m:dependencies/m:dependency", NS)}
        if "shaft-bom" not in managed:
            fail(f"{pom}: import shaft-bom")
        deps = {text(d, "m:artifactId"): text(d, "m:version") for d in root.findall("m:dependencies/m:dependency", NS)}
        if "SHAFT_ENGINE" in deps or "shaft-engine" not in deps:
            fail(f"{pom}: declare shaft-engine and not SHAFT_ENGINE")
        if deps["shaft-engine"]:
            fail(f"{pom}: shaft-engine version must come from shaft-bom")
        optional = {name for name in deps if name in {"shaft-browserstack", "shaft-video", "shaft-visual"}}
        expected = {EXPECTED_OPTIONAL[artifact]} if artifact in EXPECTED_OPTIONAL else set()
        if optional != expected:
            fail(f"{pom}: expected optional modules {sorted(expected)}, found {sorted(optional)}")
    for pom in sorted((ROOT / "tools/modularization/consumer-fixtures").glob("*/pom.xml")):
        fixture_version = text(ET.parse(pom).getroot().find("m:properties", NS), "m:shaft.version")
        if fixture_version != reactor_version:
            fail(f"{pom}: shaft.version must match reactor version {reactor_version}")
    internal = (ROOT / "shaft-engine/src/main/java/com/shaft/properties/internal/Internal.java").read_text(
        encoding="utf-8"
    )
    version_match = re.search(
        r'@DefaultValue\("([^"]+)"\)\s+String shaftEngineVersion\(\);',
        internal,
    )
    if not version_match or version_match.group(1) != reactor_version:
        fail("Internal.shaftEngineVersion must match the reactor version")
    guide = (ROOT / "docs/UPGRADING_TO_MODULAR_SHAFT.md").read_text(encoding="utf-8")
    for term in REQUIRED_GUIDE_TERMS:
        if term not in guide:
            fail(f"upgrade guide is missing {term!r}")
    if "FINAL_" in guide or "MODULAR_RELEASE_VERSION" in guide:
        fail("upgrade guide contains unfinished release placeholders")
    if reactor_version not in guide:
        fail(f"upgrade guide does not identify modular release {reactor_version}")
    for path in (ROOT / "README.md", ROOT / ".github/RELEASE_BODY_TEMPLATE.md"):
        if "UPGRADING_TO_MODULAR_SHAFT.md" not in path.read_text(encoding="utf-8"):
            fail(f"{path}: prominently link the upgrade guide")
    readme = (ROOT / "README.md").read_text(encoding="utf-8")
    if "maven-central/v/io.github.shafthq/SHAFT_ENGINE" in readme:
        fail("README Maven Central badge still targets the legacy coordinate")
    if "shaft-engine/src/main/resources/images/" not in readme:
        fail("README image links do not use the reactor module path")
    properties_helper = (
        ROOT / "shaft-engine/src/main/java/com/shaft/properties/internal/PropertiesHelper.java"
    ).read_text(encoding="utf-8")
    setup_page = (ROOT / "shaft-engine/src/main/javadoc/resources/index.html").read_text(encoding="utf-8")
    if "refs/heads/main/src/main/resources/" in properties_helper:
        fail("runtime property downloads still use the pre-reactor resource path")
    if "refs/heads/main/src/main/resources/" in setup_page:
        fail("setup-page downloads still use the pre-reactor resource path")
    if "refs/heads/main/shaft-engine/src/main/resources/properties/default/" not in properties_helper:
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
    sync_workflow = (ROOT / ".github/workflows/sync-sample-projects-version.yml").read_text(
        encoding="utf-8"
    )
    if 'extract_property_version "jdk.version"' not in sync_workflow:
        fail("sample sync workflow must read jdk.version from the root properties")
    if "Updated all example pom.xml files to use SHAFT_ENGINE" in sync_workflow:
        fail("sample sync workflow still describes the legacy artifact")
    print("Modular examples and documentation contract is valid.")

if __name__ == "__main__":
    main()
