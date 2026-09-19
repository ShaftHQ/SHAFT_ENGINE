#!/usr/bin/env python3
"""Idempotent MCP catalog tool inserts (issue #6000).

S1-07 apply.sh used replace_once keyed on a neighboring coverage trailer that
still matched after the first insert, so a second run duplicated design_lint.
This helper inserts by **tool name**: skip when the name is already present,
fail closed on any duplicate names before writing.
"""

from __future__ import annotations

import argparse
import json
import sys
from pathlib import Path
from typing import Any

REPO_ROOT = Path(__file__).resolve().parents[2]
DEFAULT_MANIFEST = (
    REPO_ROOT / "shaft-mcp" / "src" / "test" / "resources" / "fixtures" / "mcp-tool-manifest.json"
)
DEFAULT_OVERLAY = (
    REPO_ROOT
    / "shaft-mcp"
    / "src"
    / "main"
    / "resources"
    / "META-INF"
    / "shaft-mcp"
    / "tool-index-overlay.json"
)


def _fail(message: str, code: int = 2) -> None:
    print(message, file=sys.stderr)
    raise SystemExit(code)


def load_json(path: Path) -> Any:
    try:
        return json.loads(path.read_text(encoding="utf-8"))
    except FileNotFoundError:
        _fail(f"catalog file not found: {path}")
    except json.JSONDecodeError as exc:
        _fail(f"invalid JSON in {path}: {exc}")


def uniqueness_error(names: list[str]) -> str | None:
    seen: set[str] = set()
    dupes: list[str] = []
    for name in names:
        if not name or not str(name).strip():
            return "catalog contains a tool without a name"
        if name in seen and name not in dupes:
            dupes.append(name)
        seen.add(name)
    if dupes:
        return "duplicate tool name(s): " + ", ".join(sorted(dupes))
    return None


def assert_unique(names: list[str], *, path: Path) -> None:
    err = uniqueness_error(names)
    if err:
        _fail(f"{path}: {err}")


def insert_manifest_list_entry(
    document: dict[str, Any],
    *,
    name: str,
    mutation: bool,
    sensitive: bool,
    deprecated: bool,
    after: str | None,
) -> str:
    """Insert into mcp-tool-manifest.json tools array. Returns inserted|skipped."""
    tools = document.get("tools")
    if not isinstance(tools, list):
        _fail("manifest tools must be a JSON array")
    names = [str(t.get("name", "")) for t in tools if isinstance(t, dict)]
    assert_unique(names, path=Path("manifest"))
    if name in names:
        return "skipped"
    entry = {
        "name": name,
        "mutation": mutation,
        "sensitive": sensitive,
        "deprecated": deprecated,
    }
    insert_at = len(tools)
    if after:
        for index, tool in enumerate(tools):
            if isinstance(tool, dict) and tool.get("name") == after:
                insert_at = index + 1
                break
        else:
            _fail(f"--after tool not found in manifest: {after}")
    tools.insert(insert_at, entry)
    assert_unique(
        [str(t.get("name", "")) for t in tools if isinstance(t, dict)],
        path=Path("manifest"),
    )
    return "inserted"


def insert_overlay_entry(
    document: dict[str, Any],
    *,
    name: str,
    mutation: bool,
    sensitive: bool,
    deprecated: bool,
) -> str:
    """Insert into tool-index-overlay.json tools object. Returns inserted|skipped."""
    tools = document.get("tools")
    if not isinstance(tools, dict):
        _fail("overlay tools must be a JSON object keyed by tool name")
    assert_unique(list(tools.keys()), path=Path("overlay"))
    if name in tools:
        return "skipped"
    tools[name] = {
        "mutation": mutation,
        "sensitive": sensitive,
        "deprecated": deprecated,
    }
    assert_unique(list(tools.keys()), path=Path("overlay"))
    return "inserted"


def write_json(path: Path, document: Any) -> None:
    path.write_text(json.dumps(document, indent=2) + "\n", encoding="utf-8")


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        description="Idempotently insert an MCP tool into catalog fixtures (#6000)."
    )
    parser.add_argument("--name", required=True, help="MCP tool name (unique key)")
    parser.add_argument("--mutation", choices=("true", "false"), required=True)
    parser.add_argument("--sensitive", choices=("true", "false"), required=True)
    parser.add_argument("--deprecated", choices=("true", "false"), default="false")
    parser.add_argument(
        "--after",
        help="Optional neighbor name; insert immediately after it in the list fixture",
    )
    parser.add_argument(
        "--manifest",
        type=Path,
        default=DEFAULT_MANIFEST,
        help="Path to mcp-tool-manifest.json",
    )
    parser.add_argument(
        "--overlay",
        type=Path,
        default=None,
        help="Optional tool-index-overlay.json (dict keyed by name)",
    )
    parser.add_argument(
        "--also-overlay",
        action="store_true",
        help=f"Also update default overlay at {DEFAULT_OVERLAY}",
    )
    parser.add_argument(
        "--check-only",
        action="store_true",
        help="Only assert uniqueness of the target file(s); do not insert",
    )
    return parser


def main(argv: list[str] | None = None) -> int:
    args = build_parser().parse_args(argv)
    name = args.name.strip()
    if not name:
        _fail("--name must be non-empty")
    mutation = args.mutation == "true"
    sensitive = args.sensitive == "true"
    deprecated = args.deprecated == "true"

    targets: list[tuple[str, Path]] = [("manifest", args.manifest)]
    overlay_path = args.overlay
    if args.also_overlay and overlay_path is None:
        overlay_path = DEFAULT_OVERLAY
    if overlay_path is not None:
        targets.append(("overlay", overlay_path))

    results: dict[str, str] = {}
    for kind, path in targets:
        document = load_json(path)
        if args.check_only:
            if kind == "manifest":
                tools = document.get("tools")
                if not isinstance(tools, list):
                    _fail(f"{path}: tools must be a JSON array")
                assert_unique(
                    [str(t.get("name", "")) for t in tools if isinstance(t, dict)],
                    path=path,
                )
            else:
                tools = document.get("tools")
                if not isinstance(tools, dict):
                    _fail(f"{path}: tools must be a JSON object")
                assert_unique(list(tools.keys()), path=path)
            results[kind] = "ok"
            continue

        if kind == "manifest":
            if not isinstance(document, dict):
                _fail(f"{path}: expected object root")
            status = insert_manifest_list_entry(
                document,
                name=name,
                mutation=mutation,
                sensitive=sensitive,
                deprecated=deprecated,
                after=args.after,
            )
            write_json(path, document)
            results[kind] = status
        else:
            if not isinstance(document, dict):
                _fail(f"{path}: expected object root")
            status = insert_overlay_entry(
                document,
                name=name,
                mutation=mutation,
                sensitive=sensitive,
                deprecated=deprecated,
            )
            write_json(path, document)
            results[kind] = status

    print(json.dumps({"name": name, "results": results}, sort_keys=True))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
