#!/usr/bin/env python3
"""Reject SHAFT reactor modules or BOM entries that diverge from the root version."""

from __future__ import annotations

import sys
import xml.etree.ElementTree as ET
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
NAMESPACE = {"m": "http://maven.apache.org/POM/4.0.0"}
SHAFT_GROUP = "io.github.shafthq"


def text(element: ET.Element, path: str) -> str | None:
    value = element.findtext(path, namespaces=NAMESPACE)
    return value.strip() if value else None


def validate_reactor_versions(root_pom: Path = ROOT / "pom.xml") -> list[str]:
    root = ET.parse(root_pom).getroot()
    root_version = text(root, "m:version")
    if not root_version:
        return [f"root project version is missing from {root_pom}"]

    errors: list[str] = []
    base = root_pom.parent
    for module_name in root.findall("m:modules/m:module", NAMESPACE):
        module_pom = base / (module_name.text or "").strip() / "pom.xml"
        module = ET.parse(module_pom).getroot()
        parent_group = text(module, "m:parent/m:groupId")
        parent_version = text(module, "m:parent/m:version")
        if parent_group != SHAFT_GROUP or parent_version != root_version:
            errors.append(
                f"{module_pom}: parent version {parent_version!r} does not match {root_version!r}"
            )

        shaft_dependencies = [
            *module.findall("m:dependencies/m:dependency", NAMESPACE),
            *module.findall("m:dependencyManagement/m:dependencies/m:dependency", NAMESPACE),
        ]
        for dependency in shaft_dependencies:
            if text(dependency, "m:groupId") not in {SHAFT_GROUP, "${project.groupId}"}:
                continue
            dependency_version = text(dependency, "m:version")
            if dependency_version not in {root_version, "${project.version}"}:
                artifact_id = text(dependency, "m:artifactId")
                errors.append(
                    f"{module_pom}: {SHAFT_GROUP}:{artifact_id} version "
                    f"{dependency_version!r} diverges from {root_version!r}"
                )
    return errors


def main() -> int:
    errors = validate_reactor_versions()
    if errors:
        print("\n".join(errors), file=sys.stderr)
        return 1
    print("All SHAFT reactor modules and inter-module dependencies use the root project version.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
