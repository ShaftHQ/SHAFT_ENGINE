#!/usr/bin/env python3
"""Verify published SHAFT artifacts and smoke consumers from Maven Central."""

from __future__ import annotations

import argparse
import platform
import shutil
import subprocess
import tempfile
import time
import urllib.error
import urllib.request
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
GROUP_PATH = "io/github/shafthq"
DEFAULT_REPOSITORY = "https://repo.maven.apache.org/maven2"
JAR_ARTIFACTS = ("shaft-engine", "shaft-browserstack", "shaft-video", "shaft-visual", "SHAFT_MCP")
POM_ARTIFACTS = ("shaft-parent", "shaft-bom", "SHAFT_ENGINE")
FIXTURE_GOALS = {
    "api": "test-compile",
    "combined-modules": "verify",
    "legacy-coordinate": "test-compile",
    "mcp": "verify",
}


def maven_executable(system: str | None = None) -> str:
    system = system or platform.system()
    candidates = ("mvn.cmd", "mvn") if system == "Windows" else ("mvn",)
    for candidate in candidates:
        executable = shutil.which(candidate)
        if executable:
            return executable
    raise RuntimeError("Maven executable was not found on PATH.")


def publication_paths(version: str) -> list[str]:
    paths = []
    for artifact in JAR_ARTIFACTS:
        base = f"{GROUP_PATH}/{artifact}/{version}/{artifact}-{version}"
        paths.extend(
            f"{base}{suffix}"
            for suffix in (".pom", ".pom.asc", ".jar", ".jar.asc", "-sources.jar",
                           "-sources.jar.asc", "-javadoc.jar", "-javadoc.jar.asc")
        )
    for artifact in POM_ARTIFACTS:
        base = f"{GROUP_PATH}/{artifact}/{version}/{artifact}-{version}.pom"
        paths.extend((base, f"{base}.asc"))
    return paths


def missing_publication_paths(repository_url: str, version: str) -> list[str]:
    missing = []
    for path in publication_paths(version):
        request = urllib.request.Request(f"{repository_url.rstrip('/')}/{path}", method="HEAD")
        try:
            with urllib.request.urlopen(request, timeout=30):
                pass
        except (urllib.error.HTTPError, urllib.error.URLError, TimeoutError):
            missing.append(path)
    return missing


def write_settings(path: Path, repository_url: str) -> None:
    path.write_text(
        "<settings><mirrors><mirror><id>release-central</id>"
        f"<url>{repository_url}</url><mirrorOf>*</mirrorOf>"
        "</mirror></mirrors></settings>\n",
        encoding="utf-8",
    )


def fixture_commands(version: str, repository: Path, settings: Path) -> list[list[str]]:
    commands = []
    for fixture, goal in FIXTURE_GOALS.items():
        commands.append([
            maven_executable(), "--batch-mode", "--no-transfer-progress", "-U",
            "--settings", str(settings),
            "-f", str(ROOT / "tools" / "modularization" / "consumer-fixtures" / fixture / "pom.xml"),
            f"-Dmaven.repo.local={repository / fixture}",
            f"-Dshaft.version={version}",
            goal,
        ])
    return commands


def verify_release(version: str, repository_url: str = DEFAULT_REPOSITORY,
                   attempts: int = 10, delay_seconds: int = 30) -> None:
    with tempfile.TemporaryDirectory(prefix="shaft-central-smoke-") as temp_dir:
        temp = Path(temp_dir)
        settings = temp / "settings.xml"
        write_settings(settings, repository_url)
        last_error = ""
        for attempt in range(1, attempts + 1):
            missing = missing_publication_paths(repository_url, version)
            if missing:
                last_error = "missing artifacts: " + ", ".join(missing)
            else:
                failed = []
                for command in fixture_commands(version, temp / "repository", settings):
                    completed = subprocess.run(command, cwd=ROOT, check=False)
                    if completed.returncode:
                        failed.append(Path(command[command.index("-f") + 1]).parent.name)
                if not failed:
                    return
                last_error = "failed consumer fixtures: " + ", ".join(failed)
            if attempt < attempts:
                print(f"Central smoke attempt {attempt}/{attempts} failed: {last_error}")
                time.sleep(delay_seconds)
        raise RuntimeError(f"Maven Central release verification failed after {attempts} attempts: {last_error}")


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("version")
    parser.add_argument("--repository-url", default=DEFAULT_REPOSITORY)
    parser.add_argument("--attempts", type=int, default=10)
    parser.add_argument("--delay-seconds", type=int, default=30)
    args = parser.parse_args()
    verify_release(args.version, args.repository_url, args.attempts, args.delay_seconds)
    print(f"Verified SHAFT {args.version} from Maven Central.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
