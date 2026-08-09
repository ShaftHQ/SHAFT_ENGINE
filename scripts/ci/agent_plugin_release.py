"""Release metadata and artifacts for portable Agent Plugin packages (#4576)."""

from __future__ import annotations

import json
import re
from pathlib import Path


REQUIRED_PACKAGES = ("act-as-mohab", "shaft-skills")
SEMVER = re.compile(r"^(0|[1-9]\d*)\.(0|[1-9]\d*)\.(0|[1-9]\d*)$")
RELEASE_MANIFEST = Path("agent-plugins/release.json")


def load_release_manifest(repository_root: Path) -> dict[str, str]:
    """Return the declared stable SemVer version for every portable package."""
    manifest_path = Path(repository_root) / RELEASE_MANIFEST
    try:
        document = json.loads(manifest_path.read_text(encoding="utf-8"))
    except (OSError, UnicodeDecodeError, json.JSONDecodeError) as error:
        raise ValueError(f"invalid agent plugin release manifest: {error}") from error
    packages = document.get("packages") if isinstance(document, dict) else None
    if not isinstance(packages, list):
        raise ValueError("agent plugin release manifest packages must be an array")

    versions: dict[str, str] = {}
    for package in packages:
        if not isinstance(package, dict):
            raise ValueError("agent plugin release manifest packages must be objects")
        name = package.get("name")
        version = package.get("version")
        if not isinstance(name, str) or name not in REQUIRED_PACKAGES:
            raise ValueError("agent plugin release manifest has an unknown package")
        if not isinstance(version, str) or not SEMVER.fullmatch(version):
            raise ValueError(f"{name} version must be stable SemVer")
        if name in versions:
            raise ValueError(f"agent plugin release manifest duplicates {name}")
        versions[name] = version

    if set(versions) != set(REQUIRED_PACKAGES):
        raise ValueError("agent plugin release manifest must declare every package")
    return versions
