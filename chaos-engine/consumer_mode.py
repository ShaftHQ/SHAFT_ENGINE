#!/usr/bin/env python3
"""Consumer-repository install mode: keep the overlay out of `git status` (#6237).

Opt in with ``install --consumer`` or ``CHAOS_ENGINE_CONSUMER=1``; the choice
persists in ``.chaos-engine-state/consumer-mode`` so repairs keep it. In this
mode ChaosEngine never edits a tracked file (an exclude cannot hide a tracked
change) and never writes ``.gitignore``; it writes one marked block to the
repository's ``info/exclude`` instead. Uninstall removes only that block.
Adopter repositories keep the default tracked-overlay behavior.
"""

from __future__ import annotations

import os
import subprocess  # nosec B404 - fixed git argv, never a shell.
from pathlib import Path

CONSUMER_ENV = "CHAOS_ENGINE_CONSUMER"
MARKER = ".chaos-engine-state/consumer-mode"
EXCLUDE_START = "# CHAOSENGINE-CONSUMER:START"
EXCLUDE_END = "# CHAOSENGINE-CONSUMER:END"
# Installer artifacts outside the host receipt's managed files.
STATIC_PATTERNS = (
    "/.chaos-engine/",
    "/.chaos-engine-*",
    "/.chaos-engine.*",
    "/.memory/",
    "/.mempalace/",
    "/graphify-out/",
    "/.graphifyignore",
    "/mempalace.yaml",
    ".chaos-engine-owned-directory",
)
_FALSE = {"", "0", "false", "no", "off"}


def enabled(project: Path) -> bool:
    """True when the env opt-in is set or the persisted marker exists."""
    flag = os.environ.get(CONSUMER_ENV)
    if flag is not None and flag.strip().casefold() not in _FALSE:
        return True
    marker = Path(project) / MARKER
    return marker.is_file() and not marker.is_symlink()


def record(project: Path) -> None:
    """Persist an env opt-in so later repairs and reinstalls keep consumer mode."""
    flag = os.environ.get(CONSUMER_ENV)
    if flag is None or flag.strip().casefold() in _FALSE:
        return
    marker = Path(project) / MARKER
    marker.parent.mkdir(parents=True, exist_ok=True)
    marker.write_text("consumer\n", encoding="utf-8")


def _git(project: Path, *arguments: str) -> str | None:
    try:
        completed = subprocess.run(  # nosec B603 B607 - fixed git argv.
            ["git", *arguments],
            cwd=str(project),
            capture_output=True,
            text=True,
            timeout=30,
            check=False,
        )
    except (OSError, subprocess.SubprocessError):
        return None
    return completed.stdout if completed.returncode == 0 else None


def exclude_file(project: Path) -> Path | None:
    """`info/exclude` of the repository; linked worktrees share the common one."""
    rendered = _git(project, "rev-parse", "--path-format=absolute", "--git-path", "info/exclude")
    return Path(rendered.strip()) if rendered and rendered.strip() else None


def tracked(project: Path, relatives) -> set[str]:
    """Subset of ``relatives`` that git tracks in ``project``."""
    names = sorted(set(relatives))
    if not names:
        return set()
    listed = _git(project, "ls-files", "-z", "--", *names)
    return {item for item in (listed or "").split("\0") if item}


def filter_content(
    project: Path,
    before: dict[str, bytes | None],
    after: dict[str, bytes | None],
) -> tuple[dict[str, bytes | None], list[str]]:
    """Keep every tracked file and ``.gitignore`` at its current bytes.

    Returns the filtered desired content and the tracked files ChaosEngine
    would have changed but left untouched.
    """
    filtered = dict(after)
    filtered[".gitignore"] = before.get(".gitignore")
    changed = [rel for rel in filtered if filtered[rel] != before.get(rel)]
    untouched = sorted(tracked(project, changed))
    for relative in untouched:
        filtered[relative] = before.get(relative)
    return filtered, untouched


def exclude_block(managed) -> str:
    patterns = [*STATIC_PATTERNS, *(f"/{path}" for path in sorted(managed) if path != ".gitignore")]
    return "\n".join([EXCLUDE_START, *dict.fromkeys(patterns), EXCLUDE_END]) + "\n"


def _without_block(text: str) -> str:
    if EXCLUDE_START not in text or EXCLUDE_END not in text:
        return text
    begin = text.index(EXCLUDE_START)
    finish = text.index(EXCLUDE_END, begin) + len(EXCLUDE_END)
    if text[finish:finish + 1] == "\n":
        finish += 1
    return text[:begin] + text[finish:]


def write_exclude(project: Path, managed) -> Path | None:
    """Write or refresh the one marked block; other exclude lines are kept."""
    path = exclude_file(project)
    if path is None:
        return None
    existing = path.read_text(encoding="utf-8") if path.is_file() else ""
    kept = _without_block(existing)
    if kept and not kept.endswith("\n"):
        kept += "\n"
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(kept + exclude_block(managed), encoding="utf-8")
    return path


def remove_exclude(project: Path) -> bool:
    """Remove only the ChaosEngine block; True when one was removed."""
    path = exclude_file(project)
    if path is None or not path.is_file():
        return False
    existing = path.read_text(encoding="utf-8")
    kept = _without_block(existing)
    if kept == existing:
        return False
    path.write_text(kept, encoding="utf-8")
    return True
