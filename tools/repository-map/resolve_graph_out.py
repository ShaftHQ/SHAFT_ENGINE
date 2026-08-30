#!/usr/bin/env python3
"""Resolve the shared graphify-out/ cache path from any worktree."""

from __future__ import annotations

import argparse
import hashlib
import json
import os
import shutil
# Only used to run one fixed git command (list-args, no shell) with the
# executable resolved to an absolute path below.
import subprocess  # nosec B404
import sys
from pathlib import Path


MARKER_NAME = ".shaft-source-revision.json"
MANIFEST_NAME = "manifest.json"


def find_shared_graph_out(cwd: Path) -> Path:
    """Return the shared graphify-out/ path under the main checkout root."""
    if "SHAFT_GRAPHIFY_OUT" in os.environ:
        configured = os.environ["SHAFT_GRAPHIFY_OUT"].strip()
        if not configured:
            raise RuntimeError("SHAFT_GRAPHIFY_OUT must not be blank")
        graph_out = Path(configured).expanduser()
        if not graph_out.is_absolute():
            raise RuntimeError("SHAFT_GRAPHIFY_OUT must be absolute")
        return graph_out.resolve()
    git_executable = shutil.which("git")
    if git_executable is None:
        raise RuntimeError("git is not on PATH")
    # Absolute executable path, fixed internal arguments, no shell.
    completed = subprocess.run(  # nosec B603
        [git_executable, "rev-parse", "--git-common-dir"],
        cwd=cwd,
        capture_output=True,
        text=True,
        check=True,
    )
    common_dir = Path(completed.stdout.strip())
    if not common_dir.is_absolute():
        common_dir = (cwd / common_dir).resolve()
    return common_dir.parent / "graphify-out"


def require_primary_checkout(cwd: Path) -> None:
    """Reject linked worktrees even when an override points below their root."""
    git_executable = shutil.which("git")
    if git_executable is None:
        raise RuntimeError("git is not on PATH")
    completed = subprocess.run(  # nosec B603
        [git_executable, "rev-parse", "--show-toplevel", "--git-common-dir"],
        cwd=cwd,
        capture_output=True,
        text=True,
        check=True,
    )
    lines = completed.stdout.splitlines()
    if len(lines) != 2:
        raise RuntimeError("cannot resolve the primary Git checkout")
    top_level = Path(lines[0])
    common_dir = Path(lines[1])
    if not common_dir.is_absolute():
        common_dir = cwd / common_dir
    if cwd.resolve() != top_level.resolve() or cwd.resolve() != common_dir.resolve().parent:
        raise RuntimeError("record the Graphify revision only from the primary checkout")


def git_revision(cwd: Path) -> str:
    """Return the shared cache source revision from origin/main."""
    git_executable = shutil.which("git")
    if git_executable is None:
        raise RuntimeError("git is not on PATH")
    completed = subprocess.run(  # nosec B603
        [git_executable, "rev-parse", "refs/remotes/origin/main"],
        cwd=cwd,
        capture_output=True,
        text=True,
        check=True,
    )
    return completed.stdout.strip()


def manifest_digest(graph_out: Path) -> str:
    """Hash the Graphify manifest so a marker cannot outlive its cache build."""
    return hashlib.sha256((graph_out / MANIFEST_NAME).read_bytes()).hexdigest()


def cache_freshness(cwd: Path, graph_out: Path) -> tuple[bool, str]:
    """Report whether the shared cache exactly matches origin/main."""
    if not graph_out.is_dir() or not any(graph_out.iterdir()):
        return False, (
            "absent - build it from the main checkout "
            "(see tools/repository-map/README.md) or fall back to rg/.memory"
        )
    manifest = graph_out / MANIFEST_NAME
    if not manifest.is_file():
        return False, "stale - shared Graphify cache has no manifest.json"
    marker_path = graph_out / MARKER_NAME
    if not marker_path.is_file():
        return False, (
            "stale - cache has no indexed revision marker; refresh it from the "
            "primary checkout or fall back to rg/.memory"
        )
    try:
        marker = json.loads(marker_path.read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError) as error:
        return False, f"stale - indexed revision marker is unreadable: {error}"
    schema_version = marker.get("schema_version") if isinstance(marker, dict) else None
    if type(schema_version) is not int or schema_version != 1:  # pylint: disable=unidiomatic-typecheck  # Exact type rejects bool aliases.
        return False, "stale - indexed revision marker schema is unsupported"
    indexed = marker.get("indexed_revision")
    expected_manifest = marker.get("manifest_sha256")
    if not isinstance(indexed, str) or len(indexed) != 40:
        return False, "stale - indexed revision marker has no valid revision"
    if not isinstance(expected_manifest, str) or len(expected_manifest) != 64:
        return False, "stale - indexed revision marker has no valid manifest digest"
    if manifest_digest(graph_out) != expected_manifest:
        return False, "stale - Graphify manifest changed after revision marker"
    requested = git_revision(cwd)
    if indexed != requested:
        return False, f"stale - indexed={indexed} requested={requested}"
    return True, str(graph_out)


def record_current_cache(cwd: Path, graph_out: Path) -> Path:
    """Bind a completed primary-checkout cache build to its source revision."""
    require_primary_checkout(cwd)
    git_executable = shutil.which("git")
    if git_executable is None:
        raise RuntimeError("git is not on PATH")
    head = subprocess.run(  # nosec B603
        [git_executable, "rev-parse", "HEAD"],
        cwd=cwd,
        capture_output=True,
        text=True,
        check=True,
    ).stdout.strip()
    if head != git_revision(cwd):
        raise RuntimeError("primary checkout HEAD must equal origin/main before recording")
    if cwd.resolve() != graph_out.parent.resolve():
        raise RuntimeError("record the Graphify revision only from the primary checkout")
    if not (graph_out / MANIFEST_NAME).is_file():
        raise RuntimeError("Graphify manifest.json is absent; run graphify . first")
    marker_path = graph_out / MARKER_NAME
    temporary = marker_path.with_suffix(marker_path.suffix + ".tmp")
    marker = {
        "schema_version": 1,
        "indexed_revision": git_revision(cwd),
        "manifest_sha256": manifest_digest(graph_out),
    }
    temporary.write_text(json.dumps(marker, indent=2) + "\n", encoding="utf-8")
    temporary.replace(marker_path)
    return marker_path


def build_parser() -> argparse.ArgumentParser:
    """Build the command-line parser."""
    parser = argparse.ArgumentParser(description=__doc__)
    action = parser.add_mutually_exclusive_group()
    action.add_argument(
        "--check",
        action="store_true",
        help="Exit 0 only if the shared cache matches origin/main.",
    )
    action.add_argument(
        "--record-current",
        action="store_true",
        help="After a primary-checkout build, record the indexed revision.",
    )
    return parser


def main(argv: list[str] | None = None, cwd: Path | None = None) -> int:
    """Run the CLI."""
    args = build_parser().parse_args(argv)
    working_directory = cwd or Path.cwd()
    try:
        graph_out = find_shared_graph_out(working_directory)
        if args.check:
            fresh, message = cache_freshness(working_directory, graph_out)
            if fresh:
                print(message)
                return 0
            print(message, file=sys.stderr)
            return 1
        if args.record_current:
            marker_path = record_current_cache(working_directory, graph_out)
            print(marker_path)
            return 0
        print(graph_out)
    except (OSError, RuntimeError, subprocess.CalledProcessError) as error:
        print(str(error), file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
