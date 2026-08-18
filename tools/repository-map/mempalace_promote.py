#!/usr/bin/env python3
"""List SHAFT MemPalace force-include paths for families 3.7.0 cannot classify."""

from __future__ import annotations

import subprocess  # nosec B404 - resolved git, list-form, no shell.
from pathlib import Path, PurePosixPath
import shutil


def is_promote_path(relative: str) -> bool:
    posix = PurePosixPath(relative.replace("\\", "/"))
    name = posix.name
    suffix = posix.suffix.lower()
    if suffix == ".properties" or suffix == ".feature":
        return True
    if suffix == ".xml" and name.casefold() != "pom.xml":
        return True
    if name.startswith("Dockerfile"):
        return True
    return "/META-INF/services/" in f"/{posix.as_posix()}"


def list_promote_paths(root: Path) -> list[str]:
    git = shutil.which("git")
    if git is None:
        return []
    try:
        completed = subprocess.run(  # nosec B603 - resolved git, no shell.
            [git, "ls-files", "-z"],
            cwd=root,
            check=False,
            capture_output=True,
        )
    except OSError:
        return []
    if completed.returncode != 0:
        return []
    listed: list[str] = []
    for raw in completed.stdout.split(b"\0"):
        if not raw:
            continue
        relative = PurePosixPath(raw.decode("utf-8", "surrogateescape")).as_posix()
        if is_promote_path(relative):
            listed.append(relative)
    return sorted(listed)


def include_ignored_batches(paths: list[str], *, max_chars: int = 4000) -> list[str]:
    batches: list[str] = []
    current: list[str] = []
    size = 0
    for path in paths:
        extra = len(path) + (1 if current else 0)
        if current and size + extra > max_chars:
            batches.append(",".join(current))
            current = [path]
            size = len(path)
            continue
        current.append(path)
        size += extra
    if current:
        batches.append(",".join(current))
    return batches
