#!/usr/bin/env python3
"""Validate SHAFT Pilot release contracts, fixtures, evidence, and outputs."""

from __future__ import annotations

import argparse
import json
import re
import sys
import tomllib
import xml.etree.ElementTree as ET
import zipfile
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
NS = {"m": "http://maven.apache.org/POM/4.0.0"}
PILOT_MODULES = (
    "shaft-pilot-core",
    "shaft-capture",
    "shaft-doctor",
    "shaft-ai",
    "shaft-mcp",
)
PUBLIC_ARTIFACTS = {
    "shaft-pilot-core": "shaft-pilot-core",
    "shaft-capture": "shaft-capture",
    "shaft-doctor": "shaft-doctor",
    "shaft-ai": "shaft-ai",
    "shaft-mcp": "SHAFT_MCP",
}
SECRET_CANARIES = (
    b"DO-NOT-RETAIN-SECRET",
    b"capture-browser-secret-canary",
    b"capture-secret-canary-value",
    b"sk-secret-canary-123456789",
    b"provider-secret-canary",
    b"advisory-report-secret",
    b"truncated-json-canary",
)
CREDENTIAL_PATTERNS = (
    re.compile(
        rb"-----BEGIN (?:RSA |EC |OPENSSH )?PRIVATE KEY-----"
        rb"\s+[A-Za-z0-9+/=\r\n]{40,}"
    ),
    re.compile(rb"\bsk-(?:proj|ant)-[A-Za-z0-9_-]{16,}\b"),
    re.compile(rb"\bgithub_pat_[A-Za-z0-9_]{16,}\b"),
    re.compile(rb"\bgh[pousr]_[A-Za-z0-9]{20,}\b"),
    re.compile(rb"\bAKIA[0-9A-Z]{16}\b"),
)


def text(element: ET.Element, path: str) -> str:
    return (element.findtext(path, default="", namespaces=NS) or "").strip()


def reactor_version(root: Path = ROOT) -> str:
    return text(ET.parse(root / "pom.xml").getroot(), "m:version")


def scan_bytes(label: str, content: bytes) -> list[str]:
    errors = []
    for canary in SECRET_CANARIES:
        if canary in content:
            errors.append(f"{label}: contains Pilot secret canary {canary.decode('ascii')}")
    for pattern in CREDENTIAL_PATTERNS:
        if pattern.search(content):
            errors.append(f"{label}: contains a credential-shaped value")
    return errors


def validate_static(root: Path = ROOT) -> list[str]:
    errors: list[str] = []
    version = reactor_version(root)
    if not re.fullmatch(r"\d+\.[1-4]\.\d{8}", version):
        errors.append(f"reactor version is not a dated SHAFT release: {version!r}")

    parent = ET.parse(root / "pom.xml").getroot()
    modules = {
        text(module, ".")
        for module in parent.findall("m:modules/m:module", NS)
    }
    missing_modules = set(PILOT_MODULES) - modules
    if missing_modules:
        errors.append(f"reactor is missing Pilot modules: {sorted(missing_modules)}")

    bom = ET.parse(root / "shaft-bom/pom.xml").getroot()
    managed = {
        text(dependency, "m:artifactId")
        for dependency in bom.findall(
            "m:dependencyManagement/m:dependencies/m:dependency", NS
        )
    }
    expected_managed = set(PUBLIC_ARTIFACTS.values())
    if not expected_managed.issubset(managed):
        errors.append(
            f"shaft-bom is missing Pilot artifacts: {sorted(expected_managed - managed)}"
        )

    internal = (
        root / "shaft-engine/src/main/java/com/shaft/properties/internal/Internal.java"
    ).read_text(encoding="utf-8")
    version_match = re.search(
        r'@DefaultValue\("([^"]+)"\)\s+String shaftEngineVersion\(\);',
        internal,
    )
    if not version_match or version_match.group(1) != version:
        errors.append("Internal.shaftEngineVersion must match the reactor version")

    pilot_properties = (
        root / "shaft-engine/src/main/java/com/shaft/properties/internal/Pilot.java"
    ).read_text(encoding="utf-8")
    for required in (
        '@Key("pilot.ai.enabled")\n    @DefaultValue("false")',
        '@Key("pilot.ai.provider")\n    @DefaultValue("none")',
        '@Key("pilot.ai.consent.local")\n    @DefaultValue("false")',
        '@Key("pilot.ai.consent.remote")\n    @DefaultValue("false")',
        '@Key("pilot.ai.telemetry.enabled")\n    @DefaultValue("false")',
    ):
        if required not in pilot_properties:
            errors.append(f"Pilot safe default is missing: {required.splitlines()[0]}")

    examples = root / "shaft-mcp/src/test/resources/fixtures/shaft-pilot"
    for path in sorted(examples.rglob("*")):
        if not path.is_file():
            continue
        content = path.read_bytes()
        errors.extend(scan_bytes(str(path.relative_to(root)), content))
        if path.suffix == ".json":
            try:
                json.loads(content)
            except json.JSONDecodeError as error:
                errors.append(f"{path.relative_to(root)}: invalid JSON: {error}")
        if path.suffix == ".toml":
            try:
                tomllib.loads(content.decode("utf-8"))
            except (UnicodeDecodeError, tomllib.TOMLDecodeError) as error:
                errors.append(f"{path.relative_to(root)}: invalid TOML: {error}")

    workflow = (root / ".github/workflows/mavenCentral_cd.yml").read_text(
        encoding="utf-8"
    )
    required_steps = (
        "Validate SHAFT Pilot release contract",
        "Run deterministic SHAFT Pilot tests",
        "Run headless SHAFT Capture release journey",
        "Validate Maven publication",
        "Deploy to Maven Central",
        "Verify published Maven Central coordinates",
    )
    positions = [workflow.find(step) for step in required_steps]
    if any(position < 0 for position in positions) or positions != sorted(positions):
        errors.append("Maven Central workflow does not enforce the Pilot release gate")

    return errors


def validate_test_results(root: Path = ROOT) -> list[str]:
    errors = []
    for module in PILOT_MODULES:
        results = root / module / "allure-results"
        populated = list(results.glob("*-result.json")) if results.is_dir() else []
        if not populated:
            errors.append(f"{module}: populated Allure result JSON is missing")
            continue
        for path in results.rglob("*"):
            if path.is_file() and path.stat().st_size <= 10 * 1024 * 1024:
                errors.extend(
                    scan_bytes(str(path.relative_to(root)), path.read_bytes())
                )

    for relative in (
        Path("shaft-capture/target/shaft-capture"),
        Path("shaft-doctor/target/shaft-doctor"),
    ):
        directory = root / relative
        if not directory.is_dir():
            continue
        for path in directory.rglob("*"):
            if path.is_file() and path.stat().st_size <= 10 * 1024 * 1024:
                errors.extend(
                    scan_bytes(str(path.relative_to(root)), path.read_bytes())
                )
    return errors


def validate_build_outputs(root: Path = ROOT) -> list[str]:
    errors = []
    version = reactor_version(root)
    for module, artifact in PUBLIC_ARTIFACTS.items():
        jar = root / module / "target" / f"{artifact}-{version}.jar"
        if not jar.is_file():
            errors.append(f"missing packaged Pilot artifact: {jar.relative_to(root)}")
            continue
        try:
            with zipfile.ZipFile(jar) as archive:
                for entry in archive.infolist():
                    if entry.is_dir() or entry.file_size > 10 * 1024 * 1024:
                        continue
                    errors.extend(
                        scan_bytes(
                            f"{jar.relative_to(root)}!/{entry.filename}",
                            archive.read(entry),
                        )
                    )
        except zipfile.BadZipFile as error:
            errors.append(f"{jar.relative_to(root)}: invalid JAR: {error}")

    if not (root / "target/bom.json").is_file():
        errors.append("missing aggregate CycloneDX output: target/bom.json")
    return errors


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--check-test-results", action="store_true")
    parser.add_argument("--check-build-outputs", action="store_true")
    args = parser.parse_args()

    errors = validate_static()
    if args.check_test_results:
        errors.extend(validate_test_results())
    if args.check_build_outputs:
        errors.extend(validate_build_outputs())
    if errors:
        print("\n".join(errors), file=sys.stderr)
        return 1
    print("SHAFT Pilot release contract is valid.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
