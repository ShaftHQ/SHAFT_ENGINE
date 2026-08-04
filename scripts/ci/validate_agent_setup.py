#!/usr/bin/env python3
"""Validate agent guidance, repository memory, and their deterministic checks."""

from __future__ import annotations

import argparse
import json
import re
import shutil
import subprocess
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
if str(ROOT) not in sys.path:
    sys.path.insert(0, str(ROOT))

from scripts.ci.validate_agent_guidance import (  # noqa: E402
    always_loaded_body_chars,
    expand_globs,
    load_budget,
    skill_listing_chars,
    validate_repository as validate_guidance,
)
from scripts.ci.validate_documentation_boundaries import (  # noqa: E402
    validate_repository as validate_documentation,
)
from scripts.ci.validate_skills import validate_repository as validate_skill_hygiene  # noqa: E402
from scripts.ci.worktree_hygiene import (  # noqa: E402
    collect_worktree_report,
    format_advisories,
)

MEMORY_PACKAGE = "@aictx/memory@0.1.55"
MEMORY_TOOLS = {
    "inspect_memory",
    "load_memory",
    "remember_memory",
    "search_memory",
}
GENERATED_MEMORY_PATHS = {
    ".memory/index/",
    ".memory/context/",
    ".memory/exports/",
    ".memory/recovery/",
    ".memory/.backup/",
    ".memory/.lock",
}
# The Memory CLI derives relation filenames by concatenating the "from" and
# "to" object ids around the predicate, with no length cap of its own. A
# single overlong pair once produced a 220-character filename that broke
# Windows checkouts (MAX_PATH) even with a short workspace prefix. Cap
# canonical memory basenames well above the longest legitimate name on record
# (133 characters) so new drift is caught before it reaches a clone.
MAX_MEMORY_BASENAME_LENGTH = 160
MEMORY_CANONICAL_GLOBS = ("memory/**/*.json", "memory/**/*.md", "relations/*.json")
# The Aictx Memory CLI's `openai_api_key` block-rule pattern
# (`sk-[A-Za-z0-9_-]{20,}`, dist/cli/main.js:3223 in @aictx/memory@0.1.55) has
# no `\b` anchor before `sk-`, so it matches mid-word inside an ordinary
# hyphenated slug -- e.g. "...mid-task-while-still-actively-working..." is
# read as "ta" + "sk-while-still-actively-working", tripping
# MemorySecretDetected and hard-failing `memory check` (exit 1). Confirmed
# live (issue #4005) with a from-scratch `npm install @aictx/memory@0.1.55`
# (i.e. without this machine's local patch) against a disposable copy of this
# repo's real `.memory/` store. The upstream fix (anchor with `\b`) is
# TDD-verified against upstream's own suite on
# https://github.com/MohabMohie/memory/tree/fix/openai-key-pattern-word-boundary
# but not merged/released, so this landmine is still live on any unpatched
# machine, CI runner, or fresh clone. This check catches new occurrences
# before they reach the store; it is not a general secret scanner and does
# not replace one.
SECRET_SCANNER_LANDMINE_PATTERN = re.compile(r"sk-[A-Za-z0-9_-]{20,}")
# Grandfathered: committed before this check existed. Do not add to this set
# for new entries -- rename the memory title/slug instead (avoid a hyphenated
# word ending in "sk" -- task-, risk-, disk-, desk-, mask-, kiosk-, etc. --
# immediately followed by 20+ word/hyphen characters).
KNOWN_SECRET_SCANNER_LANDMINE_FILES = {
    ".memory/memory/gotchas/memory-secret-scanner-local-patch-is-fragile-reinstall-update-silently-reverts-it.md",
    ".memory/memory/gotchas/worktree-isolated-agents-can-be-reclaimed-mid-task-while-still-actively-working-with-no-committed-diff.json",
}
RELATION_GLOB = "relations/*.json"
# `create_relation` (patch.schema.json #/$defs/createRelation) accepts an
# optional, caller-supplied `id` -- confirmed live against the real Memory
# CLI (v0.1.55): a `create_relation` change with no `id` derives one by
# concatenating both full endpoint ids (no length cap), while the same
# change with an explicit short `id` writes a short basename and stays
# fully readable by `memory graph`/kg_query, since traversal reads the
# `from`/`to` fields from file content, never the filename (issue #4110).
RELATION_LENGTH_HINT = (
    " `create_relation` accepts an explicit short `id` in the patch -- pass "
    "one instead of leaving it to auto-derive from the concatenated "
    "endpoint ids; `from`/`to` keep the full descriptive ids either way."
)


def issue(code: str, path: str, message: str) -> dict[str, str]:
    """Create a stable setup issue."""
    return {"code": code, "path": path, "message": message}


def read_json(path: Path) -> dict:
    """Read one UTF-8 JSON object."""
    return json.loads(path.read_text(encoding="utf-8"))


def toml_section(content: str, name: str) -> str:
    """Return one top-level TOML table body from the small project config."""
    match = re.search(
        rf"(?ms)^\[{re.escape(name)}\]\s*$\n(.*?)(?=^\[|\Z)", content
    )
    return match.group(1) if match else ""


def validate_memory_setup(root: Path = ROOT) -> list[dict[str, str]]:
    """Validate pinned Aictx storage and the restricted Codex MCP surface."""
    errors: list[dict[str, str]] = []
    required_files = [
        ".memory/config.json",
        ".memory/events.jsonl",
        ".memory/schema/config.schema.json",
        ".memory/schema/event.schema.json",
        ".memory/schema/object.schema.json",
        ".memory/schema/patch.schema.json",
        ".memory/schema/relation.schema.json",
        ".memory/memory/project.md",
        ".memory/memory/project.json",
        ".memory/memory/architecture.md",
        ".memory/memory/architecture.json",
        ".codex/config.toml",
    ]
    for configured_path in required_files:
        if not (root / configured_path).is_file():
            errors.append(issue("memory-file", configured_path, "required file is missing"))
    if errors:
        return errors

    try:
        config = read_json(root / ".memory/config.json")
    except (OSError, json.JSONDecodeError) as error:
        return [issue("memory-config", ".memory/config.json", str(error))]

    expected_config = {
        "version": 4,
        "project": {"id": "project.shaft-engine", "name": "Shaft Engine"},
        "memory": {
            "autoIndex": True,
            "defaultTokenBudget": 600,
            "saveContextPacks": False,
        },
        "git": {"trackContextPacks": False},
    }
    if config != expected_config:
        errors.append(
            issue(
                "memory-config",
                ".memory/config.json",
                "configuration must use schema v4, project.shaft-engine, and a 600-token budget",
            )
        )

    ignored = {
        line.strip()
        for line in (root / ".gitignore").read_text(encoding="utf-8").splitlines()
        if line.strip()
    }
    for generated_path in sorted(GENERATED_MEMORY_PATHS - ignored):
        errors.append(
            issue("memory-ignore", ".gitignore", f"generated path is not ignored: {generated_path}")
        )

    memory_dir = root / ".memory"
    for pattern in MEMORY_CANONICAL_GLOBS:
        for memory_path in sorted(memory_dir.glob(pattern)):
            if len(memory_path.name) > MAX_MEMORY_BASENAME_LENGTH:
                hint = RELATION_LENGTH_HINT if pattern == RELATION_GLOB else ""
                errors.append(
                    issue(
                        "memory-filename-length",
                        str(memory_path.relative_to(root)),
                        f"basename exceeds {MAX_MEMORY_BASENAME_LENGTH} characters "
                        "(risks Windows MAX_PATH on checkout); shorten the object/relation id."
                        + hint,
                    )
                )
            relative_posix = memory_path.relative_to(root).as_posix()
            if relative_posix in KNOWN_SECRET_SCANNER_LANDMINE_FILES:
                continue
            landmine = SECRET_SCANNER_LANDMINE_PATTERN.search(
                memory_path.read_text(encoding="utf-8")
            )
            if landmine:
                errors.append(
                    issue(
                        "memory-secret-landmine",
                        relative_posix,
                        f"content matches {landmine.group(0)[:32]!r}, which the unpatched "
                        "Aictx Memory CLI's unanchored openai_api_key rule "
                        "(`sk-[A-Za-z0-9_-]{20,}`, no \\b before `sk-`) reads as an API "
                        "key and hard-fails `memory check` on (issue #4005). Rename the "
                        'title/slug to avoid a hyphenated word ending in "sk" '
                        "(task-, risk-, disk-, desk-, mask-, kiosk-, ...) followed by "
                        "20+ word/hyphen characters.",
                    )
                )

    codex_content = (root / ".codex/config.toml").read_text(encoding="utf-8")
    server_content = toml_section(codex_content, "mcp_servers.shaft-memory")
    remember_content = toml_section(
        codex_content, "mcp_servers.shaft-memory.tools.remember_memory"
    )
    list_values: dict[str, list[str]] = {}
    for key in ("args", "enabled_tools"):
        match = re.search(rf"(?m)^{key}\s*=\s*(\[[^\n]+\])\s*$", server_content)
        if match:
            try:
                list_values[key] = json.loads(match.group(1))
            except json.JSONDecodeError:
                list_values[key] = []
    checks = {
        "server section": bool(server_content),
        "command": re.search(r'(?m)^command\s*=\s*"npx"\s*$', server_content)
        is not None,
        "args": list_values.get("args")
        == ["--yes", "--package", MEMORY_PACKAGE, "--", "memory-mcp"],
        "cwd": re.search(r'(?m)^cwd\s*=\s*"\.\."\s*$', server_content)
        is not None,
        "enabled_tools": set(list_values.get("enabled_tools", [])) == MEMORY_TOOLS,
        "default_tools_approval_mode": re.search(
            r'(?m)^default_tools_approval_mode\s*=\s*"auto"\s*$', server_content
        )
        is not None,
        "required": re.search(r"(?m)^required\s*=\s*false\s*$", server_content)
        is not None,
        "remember approval": (
            bool(remember_content)
            and re.search(r'(?m)^approval_mode\s*=\s*"prompt"\s*$', remember_content)
            is not None
        ),
    }
    for name, valid in checks.items():
        if not valid:
            errors.append(
                issue("memory-mcp", ".codex/config.toml", f"invalid shaft-memory {name}")
            )
    return errors


def validate_host_parity(root: Path = ROOT) -> list[dict[str, str]]:
    """Validate the executable Claude/Codex capability map and its evidence."""
    relative = Path("scripts/ci/agent_harness_parity.json")
    try:
        matrix = read_json(root / relative)
    except (OSError, json.JSONDecodeError) as error:
        return [issue("host-parity", relative.as_posix(), str(error))]
    if not isinstance(matrix, dict):
        return [issue("host-parity-schema", relative.as_posix(), "top level must be an object")]
    errors: list[dict[str, str]] = []
    workflow_path = root / ".github/workflows/pr-gate.yml"
    workflow = workflow_path.read_text(encoding="utf-8") if workflow_path.is_file() else ""
    raw_capabilities = matrix.get("capabilities", [])
    capabilities = raw_capabilities if isinstance(raw_capabilities, list) else []
    valid_rows = [
        item
        for item in capabilities
        if isinstance(item, dict) and isinstance(item.get("id"), str)
    ]
    row_ids = [item["id"] for item in valid_rows]
    if matrix.get("version") != 1 or matrix.get("hosts") != ["claude", "codex"]:
        errors.append(issue("host-parity-schema", relative.as_posix(), "invalid version or hosts"))
    if len(valid_rows) != len(capabilities):
        errors.append(issue("host-parity-schema", relative.as_posix(), "capabilities must be objects with string ids"))
    if not valid_rows or len(row_ids) != len(set(row_ids)):
        errors.append(issue("host-parity-schema", relative.as_posix(), "capability ids must be nonempty and unique"))
    for item in valid_rows:
        if not re.fullmatch(r"[a-z][a-z0-9_]*", item["id"]):
            errors.append(issue("host-parity-schema", relative.as_posix(), f"invalid capability id: {item['id']!r}"))
        errors.extend(parity_evidence_errors(item, root, relative))
        errors.extend(parity_check_errors(item, root, relative, workflow))
        if item.get("mode") not in {"shared", "equivalent", "substitution"}:
            errors.append(issue("host-parity-schema", relative.as_posix(), f"{item.get('id')}.mode is invalid"))
        if item.get("mode") == "substitution" and not item.get("note"):
            errors.append(issue("host-parity-schema", relative.as_posix(), f"{item.get('id')} substitution needs a note"))
    return errors


def parity_evidence_errors(item: dict, root: Path, relative: Path) -> list[dict[str, str]]:
    """Check that one capability row's evidence paths are relative and present."""
    errors: list[dict[str, str]] = []
    for field in ("owner", "claude", "codex"):
        values = item.get(field, [])
        values = values if isinstance(values, list) else [values]
        if not values:
            errors.append(issue("host-parity-path", relative.as_posix(), f"{item.get('id')}.{field} is empty"))
        for value in values:
            path = Path(value) if isinstance(value, str) else Path()
            if not value or path.is_absolute() or ".." in path.parts or not (root / path).is_file():
                errors.append(
                    issue(
                        "host-parity-path",
                        relative.as_posix(),
                        f"{item.get('id')}.{field} has invalid evidence path: {value!r}",
                    )
                )
    return errors


def parity_check_errors(
    item: dict, root: Path, relative: Path, workflow: str
) -> list[dict[str, str]]:
    """Check that one capability row names a real test that PR Gate actually runs."""
    check = item.get("check")
    if not isinstance(check, str) or check.count("::") != 1:
        return [issue("host-parity-path", relative.as_posix(), f"{item['id']}.check must name file.py::test_method")]
    check_path_text, check_name = check.split("::")
    check_path = Path(check_path_text)
    valid_check = (
        not check_path.is_absolute()
        and ".." not in check_path.parts
        and check_path.suffix == ".py"
        and check_name.startswith("test_")
        and (root / check_path).is_file()
    )
    source = (root / check_path).read_text(encoding="utf-8") if valid_check else ""
    if not valid_check or not re.search(rf"(?m)^\s+def {re.escape(check_name)}\(", source):
        return [issue("host-parity-path", relative.as_posix(), f"{item['id']}.check is not a runnable test: {check!r}")]
    if ".".join(check_path.with_suffix("").parts) not in workflow:
        return [issue("host-parity-ci", relative.as_posix(), f"{item['id']}.check is not run by PR Gate: {check!r}")]
    return []


def run_memory_check(root: Path) -> list[dict[str, str]]:
    """Run `memory check` against the PATH-resolved Memory CLI.

    Agents invoke `memory` from PATH (AGENTS.md, "Memory & Learning Loop"),
    so this validates the toolchain copy actually in use rather than an
    `npx --package`-cached copy nobody runs. Fails loudly with an actionable
    message when the binary is missing instead of falling back to a download.
    """
    executable = shutil.which("memory")
    if executable is None:
        return [
            issue(
                "memory-check",
                "memory",
                "memory CLI not found on PATH. Install it globally with "
                f"`npm install -g {MEMORY_PACKAGE}` and ensure the npm global "
                "bin directory is on PATH, then retry.",
            )
        ]
    completed = subprocess.run(
        [executable, "check"],
        cwd=root,
        capture_output=True,
        text=True,
        timeout=120,
        check=False,
    )
    if completed.returncode == 0:
        return []
    detail = (completed.stderr or completed.stdout).strip().replace("\n", " ")
    return [issue("memory-check", "memory", detail[:500] or f"exit code {completed.returncode}")]


def run_command(root: Path, command: list[str], code: str) -> list[dict[str, str]]:
    """Run one external check and return a concise issue on failure."""
    executable = shutil.which(command[0])
    if executable is None:
        return [issue(code, command[0], "required executable is unavailable")]
    completed = subprocess.run(
        [executable, *command[1:]],
        cwd=root,
        capture_output=True,
        text=True,
        timeout=120,
        check=False,
    )
    if completed.returncode == 0:
        return []
    detail = (completed.stderr or completed.stdout).strip().replace("\n", " ")
    return [issue(code, command[0], detail[:500] or f"exit code {completed.returncode}")]


def collect_metrics(root: Path = ROOT) -> dict:
    """Collect stable context and memory size metrics."""
    budget = load_budget(root / "scripts/ci/agent_guidance_budget.json")
    guidance_paths = expand_globs(root, budget.get("total_guidance_globs", []))
    # Two surfaces, two documented limits -- see limit_sources in the budget.
    host_body_chars: dict[str, int] = {}
    for host, configured_paths in budget.get("host_contexts", {}).items():
        host_body_chars[host] = always_loaded_body_chars(root, configured_paths)[0]
    host_listing_chars = {
        host: skill_listing_chars(root, patterns)
        for host, patterns in budget.get("host_skill_metadata_globs", {}).items()
    }
    baseline = budget.get("reduction_baseline_bytes", 0)
    # LF-normalized to match validate_total_reduction and the LF blobs CI sees.
    guidance_bytes = sum(
        len(path.read_text(encoding="utf-8").encode("utf-8")) for path in guidance_paths
    )
    reduction = 0 if not baseline else round((1 - guidance_bytes / baseline) * 100, 2)
    memory_config = root / ".memory/config.json"
    memory_budget = None
    if memory_config.is_file():
        try:
            memory_budget = read_json(memory_config).get("memory", {}).get(
                "defaultTokenBudget"
            )
        except (OSError, json.JSONDecodeError):
            pass
    memory_root = root / ".memory/memory"
    return {
        "guidance_bytes": guidance_bytes,
        "guidance_reduction_percent": reduction,
        "always_loaded_body_chars": host_body_chars,
        "skill_listing_chars": host_listing_chars,
        "memory_objects": len(list(memory_root.rglob("*.json")))
        if memory_root.is_dir()
        else 0,
        "memory_default_token_budget": memory_budget,
        "codex_memory_tools": sorted(MEMORY_TOOLS),
    }


def collect_worktree_metrics(root: Path = ROOT, *, run_external: bool = True) -> dict:
    """Describe worktrees holding pending, superseded, or corrupt work."""
    # Reported, never fatal (issue #4437). Concurrent sessions each own a
    # worktree, so a dirty one is normal and must not fail the gate agents run
    # constantly -- but a worktree that is corrupt, already upstream, or
    # holding uncommitted work nobody will return to has to be visible
    # somewhere an agent already looks.
    #
    # Local git only, in both modes. Asking GitHub about each branch would add
    # one network round trip per worktree to a command contributors run by
    # hand; `scripts/ci/worktree_hygiene.py --check-pull-requests` owns that
    # lookup. `run_external` is accepted so this reads like its sibling
    # collectors and can gain an external check without a signature change.
    del run_external
    report = collect_worktree_report(root)
    return {"worktrees": report, "worktree_advisories": format_advisories(report)}


def validate_repository(
    root: Path = ROOT, *, run_external: bool = True
) -> tuple[list[dict[str, str]], dict]:
    """Run all agent setup checks."""
    errors = [
        *validate_guidance(root),
        *[
            issue("documentation-boundary", "documentation", message)
            for message in validate_documentation(root)
        ],
        *validate_memory_setup(root),
        *validate_host_parity(root),
        *validate_skill_hygiene(root),
    ]
    if run_external:
        errors.extend(run_memory_check(root))
        errors.extend(run_command(root, ["git", "diff", "--check"], "diff-check"))
    metrics = collect_metrics(root)
    metrics.update(collect_worktree_metrics(root, run_external=run_external))
    return (
        sorted(errors, key=lambda item: (item["path"], item["code"], item["message"])),
        metrics,
    )


def build_parser() -> argparse.ArgumentParser:
    """Build the command-line parser."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--root", type=Path, default=ROOT)
    parser.add_argument("--format", choices=("text", "json"), default="text")
    parser.add_argument(
        "--skip-external",
        action="store_true",
        help="Skip pinned Memory CLI and git diff checks.",
    )
    return parser


def main() -> int:
    """Run the CLI."""
    args = build_parser().parse_args()
    errors, metrics = validate_repository(
        args.root.resolve(), run_external=not args.skip_external
    )
    if args.format == "json":
        print(json.dumps({"valid": not errors, "errors": errors, "metrics": metrics}, indent=2))
        return 1 if errors else 0

    if errors:
        for error in errors:
            print(f"{error['code']}: {error['path']}: {error['message']}", file=sys.stderr)
    else:
        print(
            "Agent setup is valid: "
            f"{metrics['guidance_bytes']} guidance bytes, "
            f"{metrics['guidance_reduction_percent']}% reduction, "
            f"{metrics['memory_objects']} memory objects."
        )
    # Advisories print whether or not the gate passed: they describe work that
    # is at risk, not a broken setup, and a passing run is exactly when an
    # agent would otherwise stop reading.
    for advisory in metrics.get("worktree_advisories", []):
        print(advisory)
    return 1 if errors else 0


if __name__ == "__main__":
    raise SystemExit(main())
