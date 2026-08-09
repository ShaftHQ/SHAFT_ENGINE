"""Release metadata and artifacts for portable Agent Plugin packages (#4576)."""

from __future__ import annotations

import json
import re
import hashlib
import os
import sys
import tempfile
import zipfile
from pathlib import Path


REQUIRED_PACKAGES = ("act-as-mohab", "shaft-skills")
SEMVER = re.compile(r"^(0|[1-9]\d*)\.(0|[1-9]\d*)\.(0|[1-9]\d*)$")
RELEASE_MANIFEST = Path("agent-plugins/release.json")
ROOT = Path(__file__).resolve().parents[2]
PORTABLE_TEXT_SUFFIXES = {".LICENSE", ".json", ".md", ".yaml", ".yml"}

if str(ROOT) not in sys.path:
    sys.path.insert(0, str(ROOT))


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


def release_version(repository_root: Path, package_name: str, requested: str | None = None) -> str:
    """Return one manifest-owned version and reject a conflicting override."""
    version = load_release_manifest(repository_root)[package_name]
    if requested is not None and requested != version:
        raise ValueError(f"{package_name} version must match the release manifest: {version}")
    return version


def write_deterministic_zip(package_root: Path, archive: Path) -> None:
    """Archive package files in lexical order with fixed portable metadata."""
    with zipfile.ZipFile(archive, "w", compression=zipfile.ZIP_STORED) as output:
        files = (path for path in package_root.rglob("*") if path.is_file())
        for source in sorted(files, key=lambda path: path.relative_to(package_root).as_posix()):
            name = source.relative_to(package_root).as_posix()
            entry = zipfile.ZipInfo(name, (1980, 1, 1, 0, 0, 0))
            entry.create_system = 3
            entry.external_attr = 0o100644 << 16
            entry.compress_type = zipfile.ZIP_STORED
            contents = source.read_bytes()
            if source.suffix in PORTABLE_TEXT_SUFFIXES or source.name == "LICENSE":
                contents = contents.replace(b"\r\n", b"\n")
            output.writestr(entry, contents, compress_type=zipfile.ZIP_STORED)


def write_checksum(archive: Path) -> Path:
    """Write the conventional SHA-256 sidecar for one archive."""
    checksum = archive.with_suffix(archive.suffix + ".sha256")
    digest = hashlib.sha256(archive.read_bytes()).hexdigest()
    checksum.write_bytes(f"{digest}  {archive.name}\n".encode("utf-8"))
    return checksum


def build_release_artifacts(repository_root: Path, output_directory: Path) -> list[Path]:
    """Assemble, validate, archive, and checksum every manifest-declared package."""
    from scripts.ci.assemble_act_as_mohab_plugin import assemble as assemble_act_as_mohab
    from scripts.ci.assemble_shaft_skills_plugin import assemble as assemble_shaft_skills
    from scripts.ci.validate_agent_plugins import validate_package

    repository_root = Path(repository_root).resolve()
    output_directory = Path(output_directory)
    if output_directory.exists():
        raise ValueError(f"release artifact output must not already exist: {output_directory}")
    output_directory.parent.mkdir(parents=True, exist_ok=True)
    assemblers = {
        "act-as-mohab": assemble_act_as_mohab,
        "shaft-skills": assemble_shaft_skills,
    }
    artifacts: list[Path] = []
    with tempfile.TemporaryDirectory(
        prefix="shaft-agent-plugin-release-", dir=output_directory.parent
    ) as staging:
        staging_root = Path(staging)
        staged_assets = staging_root / "assets"
        staged_assets.mkdir()
        for package_name in REQUIRED_PACKAGES:
            version = release_version(repository_root, package_name)
            package_root = staging_root / package_name
            assemblers[package_name](repository_root, package_root, version)
            errors = validate_package(package_root)
            if errors:
                details = "; ".join(
                    f"{finding.get('path', 'package')}: {finding.get('message', finding)}"
                    if isinstance(finding, dict) else str(finding)
                    for finding in errors
                )
                raise ValueError(f"invalid assembled {package_name} package: {details}")
            archive = staged_assets / f"{package_name}-{version}.zip"
            write_deterministic_zip(package_root, archive)
            artifacts.extend((archive, write_checksum(archive)))
        os.replace(staged_assets, output_directory)
    return [output_directory / asset.name for asset in artifacts]


def main() -> int:
    import argparse

    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("output", type=Path, help="new directory for ZIP and checksum assets")
    parser.add_argument("--repository-root", type=Path, default=ROOT)
    arguments = parser.parse_args()
    build_release_artifacts(arguments.repository_root, arguments.output)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
