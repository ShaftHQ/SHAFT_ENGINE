#!/usr/bin/env python3
"""Install the portable ChaosEngine tree into a consumer project."""

from __future__ import annotations

import argparse
import hashlib
import json
import re
import shutil
import tempfile
from pathlib import Path


INSTALL_DIRECTORY = ".chaos-engine"
MANIFEST_NAME = "manifest.json"
SCHEMA_VERSION = 1
COMMIT_PATTERN = re.compile(r"[0-9a-f]{40}")


def file_sha256(path: Path) -> str:
    return hashlib.sha256(path.read_bytes()).hexdigest()


def source_files(source: Path) -> tuple[Path, ...]:
    if not (source / "skills/chaos-engine/SKILL.md").is_file():
        raise ValueError(f"source is not a portable ChaosEngine tree: {source}")
    if (source / MANIFEST_NAME).exists():
        raise ValueError(f"source contains the reserved manifest path: {MANIFEST_NAME}")
    files: list[Path] = []
    for path in sorted(source.rglob("*")):
        relative = path.relative_to(source)
        if "__pycache__" in relative.parts or path.suffix == ".pyc":
            continue
        if path.is_symlink():
            raise ValueError(f"source contains a symbolic link: {relative}")
        if path.is_file():
            files.append(path)
    return tuple(files)


def verify_staged_payload(stage: Path, ownership: dict[str, str]) -> None:
    staged = {
        path.relative_to(stage).as_posix(): file_sha256(path)
        for path in sorted(stage.rglob("*"))
        if path.is_file()
    }
    if staged != ownership:
        raise ValueError("staged payload does not match the immutable ownership plan")


def install(project: Path, source: Path, commit: str) -> Path:
    project = project.resolve()
    source = source.resolve()
    if not project.is_dir():
        raise ValueError(f"project is not a directory: {project}")
    if COMMIT_PATTERN.fullmatch(commit) is None:
        raise ValueError("commit must be a lowercase 40-hex revision")

    target = project / INSTALL_DIRECTORY
    if target.exists():
        raise ValueError(f"ChaosEngine is already installed: {target}")

    files = source_files(source)
    ownership = {path.relative_to(source).as_posix(): file_sha256(path) for path in files}
    stage = Path(tempfile.mkdtemp(prefix=f"{INSTALL_DIRECTORY}-stage-", dir=project))
    try:
        for path in files:
            relative = path.relative_to(source)
            destination = stage / relative
            destination.parent.mkdir(parents=True, exist_ok=True)
            shutil.copy2(path, destination)
        verify_staged_payload(stage, ownership)
        manifest = {
            "schemaVersion": SCHEMA_VERSION,
            "source": {"commit": commit, "kind": "local"},
            "files": ownership,
        }
        (stage / MANIFEST_NAME).write_text(
            json.dumps(manifest, indent=2, sort_keys=True) + "\n", encoding="utf-8"
        )
        stage.replace(target)
    finally:
        if stage.exists():
            shutil.rmtree(stage)
    return target


def parser() -> argparse.ArgumentParser:
    result = argparse.ArgumentParser(description=__doc__)
    commands = result.add_subparsers(dest="command", required=True)
    install_command = commands.add_parser("install")
    install_command.add_argument("--project", required=True, type=Path)
    install_command.add_argument("--source", required=True, type=Path)
    install_command.add_argument("--commit", required=True)
    return result


def main() -> int:
    args = parser().parse_args()
    try:
        target = install(args.project, args.source, args.commit)
    except (OSError, ValueError) as error:
        print(str(error), file=__import__("sys").stderr)
        return 1
    print(json.dumps({"status": "installed", "root": str(target)}))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
