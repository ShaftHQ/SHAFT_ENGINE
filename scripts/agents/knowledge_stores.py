#!/usr/bin/env python3
"""Operator CLI for centralized SHAFT MemPalace and Graphify stores."""

from __future__ import annotations

import argparse
import shutil
# Fixed list-form calls to repository resolvers and the MemPalace CLI.
import subprocess  # nosec B404
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[2]
RESOLVE_MEMPALACE = REPO_ROOT / "tools/repository-map/resolve_mempalace.py"
RESOLVE_GRAPH_OUT = REPO_ROOT / "tools/repository-map/resolve_graph_out.py"


def run_python(script: Path, arguments: list[str], cwd: Path) -> subprocess.CompletedProcess[str]:
    """Run one repository-owned Python helper without a shell."""
    return subprocess.run(  # nosec B603
        [sys.executable, str(script), *arguments],
        cwd=cwd,
        capture_output=True,
        text=True,
        check=False,
    )


def resolve_palace(cwd: Path) -> str:
    """Return the centralized palace path or raise a fail-closed error."""
    completed = run_python(RESOLVE_MEMPALACE, [], cwd)
    palace = completed.stdout.strip()
    if completed.returncode != 0 or not palace:
        raise RuntimeError(completed.stderr.strip() or "cannot resolve MemPalace")
    return palace


def run_mempalace(palace: str, arguments: list[str], cwd: Path) -> int:
    """Invoke mempalace against the resolved palace only."""
    executable = shutil.which("mempalace")
    if executable is None:
        raise RuntimeError("mempalace is not on PATH")
    completed = subprocess.run(  # nosec B603
        [executable, "--palace", palace, "--backend", "sqlite_exact", *arguments],
        cwd=cwd,
        check=False,
    )
    return completed.returncode


def cmd_status(cwd: Path) -> int:
    """Print the resolved palace, MemPalace status, and a Graphify freshness check."""
    palace = resolve_palace(cwd)
    print(f"MemPalace: {palace}", flush=True)
    status_code = run_mempalace(palace, ["status"], cwd)
    graph = run_python(RESOLVE_GRAPH_OUT, ["--check"], cwd)
    if graph.stdout:
        print(graph.stdout, end="" if graph.stdout.endswith("\n") else "\n")
    if graph.stderr:
        print(graph.stderr, end="" if graph.stderr.endswith("\n") else "\n", file=sys.stderr)
    if graph.returncode != 0:
        print(
            "Graphify: degraded (use targeted rg; do not rebuild from this command)",
            file=sys.stderr,
        )
    return status_code


def cmd_search(cwd: Path, query: str, wing: str | None, room: str | None, results: int | None) -> int:
    """Search the resolved palace without creating a checkout-local copy."""
    palace = resolve_palace(cwd)
    arguments = ["search", query]
    if wing:
        arguments.extend(["--wing", wing])
    if room:
        arguments.extend(["--room", room])
    if results is not None:
        arguments.extend(["--results", str(results)])
    return run_mempalace(palace, arguments, cwd)


def cmd_refresh() -> int:
    """Refuse refresh from ordinary checkouts and linked worktrees."""
    print(
        "Refresh is owned by SHAFT-Nightly-Knowledge-Refresh (#4809). "
        "Refuse linked worktrees and ordinary checkouts. "
        "From the installer-owned maintenance clone run "
        "py -3 tools/repository-map/graphify_maintenance.py refresh --root .",
        file=sys.stderr,
    )
    return 1


def build_parser() -> argparse.ArgumentParser:
    """Build the operator command parser."""
    parser = argparse.ArgumentParser(description=__doc__)
    subparsers = parser.add_subparsers(dest="command", required=True)
    subparsers.add_parser("status", help="Show the resolved palace and Graphify check.")
    search = subparsers.add_parser("search", help="Search the resolved SHAFT palace.")
    search.add_argument("query", help="Search query")
    search.add_argument("--wing", help="Limit to one wing")
    search.add_argument("--room", help="Limit to one room")
    search.add_argument("--results", type=int, help="Number of results")
    subparsers.add_parser("refresh", help="Refuse; point at the nightly maintenance owner.")
    return parser


def main(argv: list[str] | None = None, cwd: Path | None = None) -> int:
    """Run the CLI."""
    args = build_parser().parse_args(argv)
    working_directory = cwd or Path.cwd()
    try:
        if args.command == "status":
            return cmd_status(working_directory)
        if args.command == "search":
            return cmd_search(
                working_directory,
                args.query,
                args.wing,
                args.room,
                args.results,
            )
        return cmd_refresh()
    except (OSError, RuntimeError, subprocess.SubprocessError) as error:
        print(str(error), file=sys.stderr)
        return 1


if __name__ == "__main__":
    raise SystemExit(main())
