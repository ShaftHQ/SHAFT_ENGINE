#!/usr/bin/env python3
"""Audit or refresh a repository's local Graphify map."""

from __future__ import annotations

import argparse
from contextlib import contextmanager
import json
import os
import shutil
import subprocess  # nosec B404 - fixed, list-form repository maintenance commands.
import sys
from pathlib import Path, PurePosixPath


CLASSIFICATIONS = (
    "covered",
    "expected_data_only",
    "missing_optional_parser",
    "unexpected_parser_gap",
)
DEFAULT_GRAPH_OUT = Path("graphify-out")


def normalized_source(value: str) -> str:
    """Return a stable repository-relative spelling for a Graphify source path."""
    normalized = value.replace("\\", "/")
    while normalized.startswith("./"):
        normalized = normalized[2:]
    return PurePosixPath(normalized).as_posix()


def load_json(path: Path, expected_type: type) -> object:
    """Read one required Graphify JSON artifact with a useful error."""
    if not path.is_file():
        raise ValueError(f"required Graphify artifact is absent: {path}")
    try:
        value = json.loads(path.read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError) as error:
        raise ValueError(f"cannot read Graphify artifact {path}: {error}") from error
    if not isinstance(value, expected_type):
        raise ValueError(f"Graphify artifact has unexpected shape: {path}")
    return value


def audit_graph(root: Path, graph_out: Path) -> dict[str, object]:
    """Classify every manifest path by whether Graphify emitted a node for it."""
    if not root.is_dir():
        raise ValueError(f"repository root is absent or not a directory: {root}")
    output = graph_out if graph_out.is_absolute() else root / graph_out
    manifest = load_json(output / "manifest.json", dict)
    graph = load_json(output / "graph.json", dict)
    nodes = graph.get("nodes")
    if not isinstance(nodes, list):
        raise ValueError(f"Graphify graph has no node list: {output / 'graph.json'}")

    covered_sources = {
        normalized_source(source)
        for node in nodes
        if isinstance(node, dict)
        and isinstance((source := node.get("source_file")), str)
        and source
    }
    result: dict[str, object] = {name: [] for name in CLASSIFICATIONS}
    for raw_path in sorted(manifest):
        if not isinstance(raw_path, str):
            raise ValueError("Graphify manifest keys must be paths")
        path = normalized_source(raw_path)
        if path in covered_sources:
            classification = "covered"
        elif Path(path).suffix.lower() == ".json":
            classification = "expected_data_only"
        elif Path(path).suffix.lower() == ".sql":
            classification = "missing_optional_parser"
        else:
            classification = "unexpected_parser_gap"
        result[classification].append(path)  # type: ignore[union-attr]
    result["total_manifest_paths"] = len(manifest)
    return result


def run_stage(name: str, command: list[str], root: Path) -> None:
    """Run one refresh stage and preserve its failure as a named error."""
    try:
        subprocess.run(command, cwd=root, check=True)  # nosec B603
    except subprocess.CalledProcessError as error:
        raise RuntimeError(f"Graphify {name} stage failed with exit {error.returncode}") from error


def run_audit(root: Path, graph_out: Path) -> dict[str, object]:
    """Run the audit stage and reject actionable extraction gaps."""
    report = audit_graph(root, graph_out)
    print(json.dumps(report, indent=2, sort_keys=True))
    if report["missing_optional_parser"] or report["unexpected_parser_gap"]:
        raise RuntimeError("Graphify coverage audit found actionable parser gaps")
    return report


def require_primary_checkout(root: Path) -> Path:
    """Fail before mutation unless root is the primary checkout of a Git repository."""
    git = shutil.which("git")
    if git is None:
        raise ValueError("git is not on PATH; refresh requires a primary Git checkout")
    try:
        completed = subprocess.run(  # nosec B603
            [git, "rev-parse", "--show-toplevel", "--git-common-dir"],
            cwd=root,
            check=True,
            capture_output=True,
            text=True,
        )
    except (OSError, subprocess.SubprocessError) as error:
        raise ValueError("refresh root is not a primary Git checkout") from error
    lines = completed.stdout.splitlines()
    if len(lines) != 2:
        raise ValueError("cannot resolve the primary Git checkout")
    top_level = Path(lines[0])
    common_dir = Path(lines[1])
    if not common_dir.is_absolute():
        common_dir = root / common_dir
    if root.resolve() != top_level.resolve() or root.resolve() != common_dir.resolve().parent:
        raise ValueError("refresh root must be the primary Git checkout, not a linked worktree")
    return common_dir.resolve()


@contextmanager
def refresh_lock(common_dir: Path):
    """Hold a nonblocking, crash-released OS lock for the complete refresh."""
    lock_path = common_dir / "shaft-graphify-refresh.lock"
    lock_file = lock_path.open("a+b")
    if lock_file.seek(0, os.SEEK_END) == 0:
        lock_file.write(b"\0")
        lock_file.flush()
    lock_file.seek(0)
    try:
        if os.name == "nt":
            import msvcrt  # pylint: disable=import-outside-toplevel

            msvcrt.locking(lock_file.fileno(), msvcrt.LK_NBLCK, 1)
        else:
            import fcntl  # pylint: disable=import-outside-toplevel,import-error

            fcntl.flock(lock_file.fileno(), fcntl.LOCK_EX | fcntl.LOCK_NB)
    except OSError as error:
        lock_file.close()
        raise RuntimeError("Graphify refresh is already running for this repository") from error
    try:
        yield
    finally:
        lock_file.seek(0)
        if os.name == "nt":
            msvcrt.locking(lock_file.fileno(), msvcrt.LK_UNLCK, 1)
        else:
            fcntl.flock(lock_file.fileno(), fcntl.LOCK_UN)
        lock_file.close()


def refresh(root: Path, graph_out: Path) -> None:
    """Build, audit, cluster, then bind the complete cache to the current revision."""
    if not root.is_dir():
        raise ValueError(f"repository root is absent or not a directory: {root}")
    requested_output = graph_out if graph_out.is_absolute() else root / graph_out
    if requested_output.resolve() != (root / DEFAULT_GRAPH_OUT).resolve():
        raise ValueError("refresh owns the fixed graphify-out cache; use audit for custom output")
    common_dir = require_primary_checkout(root)
    uv = shutil.which("uv")
    if uv is None:
        raise ValueError("uv is not on PATH")
    graphify = [
        "uv",
        "tool",
        "run",
        "--with",
        "tree-sitter-sql",
        "--from",
        "graphifyy",
        "graphify",
    ]
    with refresh_lock(common_dir):
        (requested_output / ".shaft-source-revision.json").unlink(missing_ok=True)
        run_stage(
            "build",
            [uv, *graphify[1:], "extract", ".", "--code-only", "--no-cluster"],
            root,
        )
        run_audit(root, graph_out)
        run_stage("cluster", [uv, *graphify[1:], "cluster-only", "."], root)
        run_stage(
            "record",
            [sys.executable, "tools/repository-map/resolve_graph_out.py", "--record-current"],
            root,
        )


def parser() -> argparse.ArgumentParser:
    result = argparse.ArgumentParser(description=__doc__)
    subcommands = result.add_subparsers(dest="command", required=True)
    audit = subcommands.add_parser("audit")
    audit.add_argument("--root", type=Path, default=Path("."))
    audit.add_argument("--graph-out", type=Path, default=DEFAULT_GRAPH_OUT)
    refresh_command = subcommands.add_parser("refresh")
    refresh_command.add_argument("--root", type=Path, default=Path("."))
    return result


def main(argv: list[str] | None = None) -> int:
    args = parser().parse_args(argv)
    root = args.root.resolve()
    try:
        if args.command == "audit":
            report = audit_graph(root, args.graph_out)
            print(json.dumps(report, indent=2, sort_keys=True))
            return int(
                bool(report["missing_optional_parser"] or report["unexpected_parser_gap"])
            )
        refresh(root, DEFAULT_GRAPH_OUT)
    except (OSError, ValueError, RuntimeError) as error:
        print(str(error), file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
