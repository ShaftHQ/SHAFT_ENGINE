#!/usr/bin/env python3
"""Measure SHAFT downstream fixtures with one empty Maven repository per fixture."""

from __future__ import annotations

import argparse
import hashlib
import json
import platform
import re
import shutil
import subprocess
import sys
import tempfile
import time
import xml.etree.ElementTree as ET
from pathlib import Path
from typing import Iterable

ROOT = Path(__file__).resolve().parents[2]
FIXTURES_ROOT = ROOT / "tools" / "modularization" / "consumer-fixtures"
BASELINE_ROOT = ROOT / "docs" / "modularization" / "dependency-baseline"
ENGINE_ROOT = ROOT / "shaft-engine"
ENGINE_POM = ENGINE_ROOT / "pom.xml"
PROJECT_COORDINATE = "io.github.shafthq:SHAFT_ENGINE"
DEPENDENCY_LINE = re.compile(r"^\s*([^\s].*?):(/.*|[A-Za-z]:\\.*)$")


def sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as stream:
        for block in iter(lambda: stream.read(1024 * 1024), b""):
            digest.update(block)
    return digest.hexdigest()


def directory_bytes(path: Path) -> int:
    return sum(item.stat().st_size for item in path.rglob("*") if item.is_file())


def project_version(pom: Path = ROOT / "pom.xml") -> str:
    root = ET.parse(pom).getroot()
    namespace = {"m": "http://maven.apache.org/POM/4.0.0"}
    version = root.findtext("m:version", namespaces=namespace)
    if not version:
        raise ValueError(f"Unable to read project version from {pom}")
    return version.strip()


def seed_project_artifact(repository: Path, jar: Path, version: str) -> int:
    destination = repository / "io" / "github" / "shafthq" / "SHAFT_ENGINE" / version
    destination.mkdir(parents=True, exist_ok=True)
    shutil.copy2(jar, destination / f"SHAFT_ENGINE-{version}.jar")
    shutil.copy2(ENGINE_POM, destination / f"SHAFT_ENGINE-{version}.pom")
    parent_destination = repository / "io" / "github" / "shafthq" / "shaft-parent" / version
    parent_destination.mkdir(parents=True, exist_ok=True)
    shutil.copy2(ROOT / "pom.xml", parent_destination / f"shaft-parent-{version}.pom")
    return directory_bytes(repository)


def parse_dependency_list(text: str, repository: Path) -> list[dict[str, object]]:
    artifacts: list[dict[str, object]] = []
    for raw_line in text.splitlines():
        raw_line = raw_line.split(" -- ", 1)[0]
        match = DEPENDENCY_LINE.match(raw_line)
        if not match:
            continue
        coordinate_and_scope, filename = match.groups()
        parts = coordinate_and_scope.strip().split(":")
        if len(parts) < 5:
            continue
        scope = parts[-1]
        coordinate = ":".join(parts[:-1])
        artifact_path = Path(filename.strip())
        if not artifact_path.is_absolute():
            artifact_path = repository / artifact_path
        if not artifact_path.is_file():
            continue
        artifacts.append({
            "coordinate": coordinate,
            "scope": scope,
            "compressedBytes": artifact_path.stat().st_size,
            "sha256": sha256(artifact_path),
        })
    return sorted(artifacts, key=lambda artifact: str(artifact["coordinate"]))


def expected_jave_coordinate(system: str | None = None, machine: str | None = None) -> str:
    system = (system or platform.system()).lower()
    machine = (machine or platform.machine()).lower()
    arm64 = machine in {"arm64", "aarch64"}
    if system == "linux":
        artifact = "jave-nativebin-linux-arm64" if arm64 else "jave-nativebin-linux64"
    elif system == "darwin":
        artifact = "jave-nativebin-osxm1" if arm64 else "jave-nativebin-osx64"
    elif system == "windows":
        artifact = "jave-nativebin-win64"
    else:
        raise ValueError(f"Unsupported JAVE platform: {system}/{machine}")
    return f"ws.schild:{artifact}:jar:3.5.0"


def validate_required_artifacts(manifest: dict[str, object]) -> list[str]:
    artifacts = {artifact["coordinate"]: artifact for artifact in manifest["artifacts"]}
    errors: list[str] = []
    required = {
        "org.openpnp:opencv:jar:4.9.0-0": 109_619_828,
        "com.browserstack:browserstack-java-sdk:jar:1.59.8": 38_204_017,
    }
    for coordinate, expected_bytes in required.items():
        artifact = artifacts.get(coordinate)
        if artifact is None:
            errors.append(f"missing required artifact {coordinate}")
        elif artifact["compressedBytes"] != expected_bytes:
            errors.append(
                f"{coordinate} is {artifact['compressedBytes']} bytes; expected {expected_bytes}"
            )
    expected_jave = expected_jave_coordinate()
    resolved_jave = sorted(
        coordinate for coordinate in artifacts if coordinate.startswith("ws.schild:jave-nativebin-")
    )
    if resolved_jave != [expected_jave]:
        errors.append(f"resolved JAVE artifacts {resolved_jave}; expected [{expected_jave}]")
    return errors


def run_fixture(fixture: Path, jar: Path, keep_repositories: Path | None = None) -> dict[str, object]:
    version = project_version()
    temporary = Path(tempfile.mkdtemp(prefix=f"shaft-{fixture.name}-"))
    repository = temporary / "repository"
    repository.mkdir()
    seeded_bytes = seed_project_artifact(repository, jar, version)
    dependency_output = temporary / "dependencies.txt"
    command = [
        "mvn", "--batch-mode", "--no-transfer-progress", "-f", str(fixture / "pom.xml"),
        f"-Dmaven.repo.local={repository}", f"-Dshaft.version={version}", "test-compile",
        "dependency:list", "-DincludeScope=test", "-DoutputAbsoluteArtifactFilename=true",
        f"-DoutputFile={dependency_output}", "-DappendOutput=false",
    ]
    started = time.monotonic()
    completed = subprocess.run(command, cwd=ROOT, text=True, capture_output=True)
    elapsed = round(time.monotonic() - started, 3)
    if completed.returncode:
        sys.stderr.write(completed.stdout)
        sys.stderr.write(completed.stderr)
        raise RuntimeError(f"Fixture {fixture.name} failed with exit code {completed.returncode}")
    artifacts = parse_dependency_list(dependency_output.read_text(encoding="utf-8"), repository)
    repository_bytes = directory_bytes(repository)
    manifest: dict[str, object] = {
        "schemaVersion": 1,
        "fixture": fixture.name,
        "rootCoordinate": f"{PROJECT_COORDINATE}:{version}",
        "platform": {"system": platform.system(), "machine": platform.machine()},
        "artifactCount": len(artifacts),
        "artifactBytes": sum(int(artifact["compressedBytes"]) for artifact in artifacts),
        "seededRepositoryBytes": seeded_bytes,
        "repositoryBytes": repository_bytes,
        "repositoryGrowthBytes": repository_bytes - seeded_bytes,
        "elapsedSeconds": elapsed,
        "artifacts": artifacts,
    }
    if keep_repositories:
        keep_repositories.mkdir(parents=True, exist_ok=True)
        destination = keep_repositories / fixture.name
        if destination.exists():
            shutil.rmtree(destination)
        shutil.move(str(temporary), destination)
    else:
        shutil.rmtree(temporary)
    return manifest


def stable_manifest(manifest: dict[str, object]) -> dict[str, object]:
    ignored = {
        "elapsedSeconds", "repositoryBytes", "repositoryGrowthBytes", "seededRepositoryBytes",
        "artifactsRef",
    }
    return {key: value for key, value in manifest.items() if key not in ignored}


def load_expected_manifest(expected_path: Path) -> dict[str, object]:
    expected = json.loads(expected_path.read_text(encoding="utf-8"))
    artifacts_ref = expected.get("artifactsRef")
    if artifacts_ref:
        expected["artifacts"] = json.loads(
            (expected_path.parent / str(artifacts_ref)).read_text(encoding="utf-8")
        )["artifacts"]
    return expected


def verify_manifest(actual: dict[str, object], expected_path: Path) -> list[str]:
    expected = load_expected_manifest(expected_path)
    errors = validate_required_artifacts(actual)
    if stable_manifest(actual) != stable_manifest(expected):
        errors.append(f"stable dependency manifest differs from {expected_path}")
    return errors


def write_recorded_manifests(manifests: list[dict[str, object]], output: Path) -> None:
    artifact_sets = [manifest["artifacts"] for manifest in manifests]
    if any(artifacts != artifact_sets[0] for artifacts in artifact_sets[1:]):
        raise ValueError("Fixture dependency sets differ; separate artifact catalogs are required")
    (output / "artifacts.json").write_text(
        json.dumps({"schemaVersion": 1, "artifacts": artifact_sets[0]}, indent=2) + "\n",
        encoding="utf-8",
    )
    for manifest in manifests:
        summary = {key: value for key, value in manifest.items() if key != "artifacts"}
        summary["artifactsRef"] = "artifacts.json"
        (output / f"{manifest['fixture']}.json").write_text(
            json.dumps(summary, indent=2) + "\n", encoding="utf-8"
        )


def write_report(manifests: Iterable[dict[str, object]], destination: Path) -> None:
    rows = []
    for manifest in manifests:
        rows.append(
            f"| {manifest['fixture']} | {manifest['artifactCount']} | "
            f"{manifest['artifactBytes']:,} | {manifest['repositoryGrowthBytes']:,} | "
            f"{manifest['elapsedSeconds']:.3f} |"
        )
    destination.write_text(
        "# Pre-modularization cold-cache dependency baseline\n\n"
        "Each fixture was compiled against the legacy `io.github.shafthq:SHAFT_ENGINE` coordinate "
        "using its own empty temporary Maven repository. The SHAFT JAR and POM are seeded before "
        "measurement; repository growth therefore represents downloaded dependencies and build plugins.\n\n"
        "| Fixture | Artifacts | Classpath JAR bytes | Repository growth bytes | Elapsed seconds |\n"
        "| --- | ---: | ---: | ---: | ---: |\n" + "\n".join(rows) + "\n\n"
        "Elapsed time and repository growth are observations, not equality gates. Artifact coordinates, "
        "scopes, compressed JAR sizes, and SHA-256 values are compared exactly by `--verify`.\n",
        encoding="utf-8",
    )


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--fixture", action="append", help="fixture name; repeat to select multiple")
    parser.add_argument("--jar", type=Path, help="SHAFT JAR; defaults to shaft-engine/target/SHAFT_ENGINE-<version>.jar")
    parser.add_argument("--record", action="store_true", help="replace committed manifests and report")
    parser.add_argument("--verify", action="store_true", help="compare against committed manifests")
    parser.add_argument("--output", type=Path, help="write generated manifests to this directory")
    parser.add_argument("--keep-repositories", type=Path, help="retain isolated repositories for diagnosis")
    args = parser.parse_args()
    version = project_version()
    jar = (args.jar or ENGINE_ROOT / "target" / f"SHAFT_ENGINE-{version}.jar").resolve()
    if not jar.is_file():
        parser.error(f"SHAFT JAR does not exist: {jar}; run mvn clean install -DskipTests -Dgpg.skip")
    selected = set(args.fixture or [])
    fixtures = sorted(path for path in FIXTURES_ROOT.iterdir() if (path / "pom.xml").is_file())
    unknown = selected - {path.name for path in fixtures}
    if unknown:
        parser.error(f"unknown fixtures: {', '.join(sorted(unknown))}")
    if selected:
        fixtures = [path for path in fixtures if path.name in selected]
    output = BASELINE_ROOT / "manifests" if args.record else (args.output or Path(tempfile.mkdtemp()))
    output.mkdir(parents=True, exist_ok=True)
    manifests = []
    errors: list[str] = []
    for fixture in fixtures:
        print(f"Measuring {fixture.name}...", flush=True)
        manifest = run_fixture(fixture, jar, args.keep_repositories)
        manifests.append(manifest)
        if not args.record:
            destination = output / f"{fixture.name}.json"
            destination.write_text(json.dumps(manifest, indent=2) + "\n", encoding="utf-8")
        errors.extend(f"{fixture.name}: {error}" for error in validate_required_artifacts(manifest))
        if args.verify:
            errors.extend(
                f"{fixture.name}: {error}"
                for error in verify_manifest(manifest, BASELINE_ROOT / "manifests" / f"{fixture.name}.json")
            )
    if args.record:
        write_recorded_manifests(manifests, output)
        write_report(manifests, BASELINE_ROOT / "REPORT.md")
    if errors:
        print("\n".join(errors), file=sys.stderr)
        return 1
    print(f"Wrote {len(manifests)} manifest(s) to {output}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
