#!/usr/bin/env python3
"""Validate bundled examples and user-facing modular SHAFT migration docs."""
from pathlib import Path
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
    guide = (ROOT / "docs/UPGRADING_TO_MODULAR_SHAFT.md").read_text(encoding="utf-8")
    for term in REQUIRED_GUIDE_TERMS:
        if term not in guide:
            fail(f"upgrade guide is missing {term!r}")
    if "FINAL_" in guide or "MODULAR_RELEASE_VERSION" not in guide:
        fail("upgrade guide contains unfinished measurement placeholders or lacks release placeholder")
    for path in (ROOT / "README.md", ROOT / ".github/RELEASE_BODY_TEMPLATE.md"):
        if "UPGRADING_TO_MODULAR_SHAFT.md" not in path.read_text(encoding="utf-8"):
            fail(f"{path}: prominently link the upgrade guide")
    print("Modular examples and documentation contract is valid.")

if __name__ == "__main__":
    main()
