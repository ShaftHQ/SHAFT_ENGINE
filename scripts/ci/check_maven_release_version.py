#!/usr/bin/env python3
"""Stop a release before setup when its Maven Central version already exists."""

from __future__ import annotations

import argparse
import urllib.error
import urllib.request
import xml.etree.ElementTree as ET
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
DEFAULT_REPOSITORY = "https://repo.maven.apache.org/maven2"
NS = {"m": "http://maven.apache.org/POM/4.0.0"}


def publication_url(pom_path: Path, repository_url: str = DEFAULT_REPOSITORY) -> tuple[str, str]:
    project = ET.parse(pom_path).getroot()
    group_id = project.findtext("m:groupId", namespaces=NS)
    artifact_id = project.findtext("m:artifactId", namespaces=NS)
    version = project.findtext("m:version", namespaces=NS)
    if not all((group_id, artifact_id, version)):
        raise ValueError(f"{pom_path} must declare groupId, artifactId, and version")

    group_path = group_id.strip().replace(".", "/")
    artifact_id = artifact_id.strip()
    version = version.strip()
    coordinate = f"{group_id.strip()}:{artifact_id}:{version}"
    path = f"{group_path}/{artifact_id}/{version}/{artifact_id}-{version}.pom"
    return coordinate, f"{repository_url.rstrip('/')}/{path}"


def release_exists(pom_path: Path, repository_url: str = DEFAULT_REPOSITORY,
                   timeout_seconds: int = 30) -> tuple[str, bool]:
    coordinate, url = publication_url(pom_path, repository_url)
    request = urllib.request.Request(url, method="HEAD")
    try:
        with urllib.request.urlopen(request, timeout=timeout_seconds):
            return coordinate, True
    except urllib.error.HTTPError as error:
        if error.code == 404:
            return coordinate, False
        raise RuntimeError(
            f"Could not verify {coordinate} on Maven Central: HTTP {error.code}"
        ) from error
    except (urllib.error.URLError, TimeoutError) as error:
        raise RuntimeError(f"Could not verify {coordinate} on Maven Central: {error}") from error


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--pom", type=Path, default=ROOT / "pom.xml")
    parser.add_argument("--repository-url", default=DEFAULT_REPOSITORY)
    parser.add_argument("--timeout-seconds", type=int, default=30)
    args = parser.parse_args()

    coordinate, exists = release_exists(
        args.pom, args.repository_url, args.timeout_seconds
    )
    if exists:
        print(f"Release already exists on Maven Central: {coordinate}")
        return 1
    print(f"Maven Central version is available: {coordinate}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
