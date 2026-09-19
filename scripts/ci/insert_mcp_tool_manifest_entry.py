#!/usr/bin/env python3
"""
Idempotent MCP catalog tool inserts (issue #6000).

Skip when the tool name already exists; fail closed on duplicate names
before writing. Prefer this over replace_once on a neighboring trailer.
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
    return None


def uniqueness_error(names: list[str]) -> str | None:
    seen: set[str] = set()
    dupes: set[str] = set()
    for name in names:
        if not str(name).strip():
            return "catalog contains a tool without a name"
        if name in seen:
            dupes.add(name)
        seen.add(name)
    if dupes:
        return "duplicate tool name(s): " + ", ".join(sorted(dupes))
    return None


def assert_unique(names: list[str], path: Path) -> None:
    err = uniqueness_error(names)
    if err:
        _fail(f"{path}: {err}")


def _tool_entry(name: str, mutation: bool, sensitive: bool, deprecated: bool) -> dict[str, Any]:
    return {
        "name": name,
        "mutation": mutation,
        "sensitive": sensitive,
        "deprecated": deprecated,
    }


def _flags(mutation: bool, sensitive: bool, deprecated: bool) -> dict[str, bool]:
    return {"mutation": mutation, "sensitive": sensitive, "deprecated": deprecated}


def _index_after(tools: list[Any], after: str | None) -> int:
    if after is None:
        return len(tools)
    for index, tool in enumerate(tools):
        if isinstance(tool, dict) and tool.get("name") == after:
            return index + 1
    _fail(f"--after tool not found in manifest: {after}")
    return len(tools)


def manifest_names(tools: list[Any]) -> list[str]:
    return [str(t.get("name", "")) for t in tools if isinstance(t, dict)]


def insert_manifest_list_entry(
    document: dict[str, Any],
    *,
    name: str,
    mutation: bool,
    sensitive: bool,
    deprecated: bool,
    after: str | None,
) -> str:
    """Insert into mcp-tool-manifest.json tools array."""
    tools = document.get("tools")
    if not isinstance(tools, list):
        _fail("manifest tools must be a JSON array")
    names = manifest_names(tools)
    assert_unique(names, Path("manifest"))
    if name in names:
        return "skipped"
    tools.insert(_index_after(tools, after), _tool_entry(name, mutation, sensitive, deprecated))
    assert_unique(manifest_names(tools), Path("manifest"))
    return "inserted"


def insert_overlay_entry(
    document: dict[str, Any],
    *,
    name: str,
    mutation: bool,
    sensitive: bool,
    deprecated: bool,
) -> str:
    """Insert into tool-index-overlay.json tools object."""
    tools = document.get("tools")
    if not isinstance(tools, dict):
        _fail("overlay tools must be a JSON object keyed by tool name")
    assert_unique(list(tools.keys()), Path("overlay"))
    if name in tools:
        return "skipped"
    tools[name] = _flags(mutation, sensitive, deprecated)
    assert_unique(list(tools.keys()), Path("overlay"))
    return "inserted"


def write_json(path: Path, document: Any) -> None:
    path.write_text(json.dumps(document, indent=2) + "\n", encoding="utf-8")


def check_manifest(document: Any, path: Path) -> None:
    tools = document.get("tools") if isinstance(document, dict) else None
    if not isinstance(tools, list):
        _fail(f"{path}: tools must be a JSON array")
    assert_unique(manifest_names(tools), path)


def check_overlay(document: Any, path: Path) -> None:
    tools = document.get("tools") if isinstance(document, dict) else None
    if not isinstance(tools, dict):
        _fail(f"{path}: tools must be a JSON object")
    assert_unique(list(tools.keys()), path)


def apply_manifest(
    path: Path,
    *,
    name: str,
    mutation: bool,
    sensitive: bool,
    deprecated: bool,
    after: str | None,
    check_only: bool,
) -> str:
    document = load_json(path)
    if check_only:
        check_manifest(document, path)
        return "ok"
    if not isinstance(document, dict):
        _fail(f"{path}: expected object root")
    status = insert_manifest_list_entry(
        document,
        name=name,
        mutation=mutation,
        sensitive=sensitive,
        deprecated=deprecated,
        after=after,
    )
    write_json(path, document)
    return status


def apply_overlay(
    path: Path,
    *,
    name: str,
    mutation: bool,
    sensitive: bool,
    deprecated: bool,
    check_only: bool,
) -> str:
    document = load_json(path)
    if check_only:
        check_overlay(document, path)
        return "ok"
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
    return status


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        description="Idempotently insert an MCP tool into catalog fixtures (#6000)."
    )
    parser.add_argument("--name", required=True, help="MCP tool name (unique key)")
    parser.add_argument("--mutation", choices=("true", "false"), required=True)
    parser.add_argument("--sensitive", choices=("true", "false"), required=True)
    parser.add_argument("--deprecated", choices=("true", "false"), default="false")
    parser.add_argument("--after", help="Insert after this neighbor in the list fixture")
    parser.add_argument("--manifest", type=Path, default=DEFAULT_MANIFEST)
    parser.add_argument("--overlay", type=Path, default=None)
    parser.add_argument("--also-overlay", action="store_true")
    parser.add_argument("--check-only", action="store_true")
    return parser


def resolve_overlay(args: argparse.Namespace) -> Path | None:
    if args.overlay is not None:
        return args.overlay
    if args.also_overlay:
        return DEFAULT_OVERLAY
    return None


def main(argv: list[str] | None = None) -> int:
    args = build_parser().parse_args(argv)
    name = args.name.strip()
    if not name:
        _fail("--name must be non-empty")
    mutation = args.mutation == "true"
    sensitive = args.sensitive == "true"
    deprecated = args.deprecated == "true"
    results = {
        "manifest": apply_manifest(
            args.manifest,
            name=name,
            mutation=mutation,
            sensitive=sensitive,
            deprecated=deprecated,
            after=args.after,
            check_only=args.check_only,
        )
    }
    overlay = resolve_overlay(args)
    if overlay is not None:
        results["overlay"] = apply_overlay(
            overlay,
            name=name,
            mutation=mutation,
            sensitive=sensitive,
            deprecated=deprecated,
            check_only=args.check_only,
        )
    print(json.dumps({"name": name, "results": results}, sort_keys=True))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
