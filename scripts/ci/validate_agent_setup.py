#!/usr/bin/env python3
"""Validate agent guidance, repository memory, and their deterministic checks."""

from __future__ import annotations

import argparse
import fnmatch
import hashlib
import json
import re
import shutil
import subprocess  # nosec B404 - runs memory check CLI and fixed, read-only git diff --check.
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
if str(ROOT) not in sys.path:
    sys.path.insert(0, str(ROOT))

# Below the sys.path insert, with the sibling imports, not above it. Placing
# this at the top passed the whole unittest suite -- which runs from the
# repository root, where `scripts` is already importable -- and broke the
# command outright when an agent ran it as a script from anywhere else. That
# is the one invocation this function exists to serve.
from scripts.ci.harness_reachability import harness_report  # noqa: E402
from scripts.ci.validate_agent_guidance import (  # noqa: E402
    always_loaded_body_chars,
    expand_globs,
    load_budget,
    skill_listing_chars,
    validate_repository as validate_guidance,
)
from scripts.ci.validate_agent_ownership import validate as validate_ownership  # noqa: E402
from scripts.ci.validate_chaos_engine_readme import validate as validate_chaos_readme  # noqa: E402
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
# Secrets and machine-local notes stay here and are never source-controlled.
# Canonical objects under `.memory/memory` and `.memory/relations` must be.
PRIVATE_MEMORY_PATHS = {
    ".memory/private/",
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
# Event names that record a change to an existing object. `memory.created`
# is excluded on purpose: it is what the four objects in #4465 already had,
# and accepting it would let a creation launder every later silent edit.
MEMORY_UPDATE_EVENTS = frozenset(
    {"memory.updated", "memory.marked_stale", "memory.superseded"}
)
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
        "version": 5,
        "project": {"id": "project.shaft-engine", "name": "Shaft Engine"},
        "memory": {
            "autoIndex": True,
            "defaultTokenBudget": 600,
        },
    }
    if config != expected_config:
        errors.append(
            issue(
                "memory-config",
                ".memory/config.json",
                "configuration must use schema v5, project.shaft-engine, and a 600-token budget",
            )
        )

    ignored = {
        line.strip()
        for line in (root / ".gitignore").read_text(encoding="utf-8").splitlines()
        if line.strip()
    }
    for generated_path in sorted((GENERATED_MEMORY_PATHS | PRIVATE_MEMORY_PATHS) - ignored):
        errors.append(
            issue("memory-ignore", ".gitignore", f"generated path is not ignored: {generated_path}")
        )

    memory_root = root / ".memory/memory"
    _landed, untracked = memory_object_counts(root, memory_root)
    if untracked:
        errors.append(
            issue(
                "memory-untracked",
                ".memory/memory",
                f"{untracked} Memory object(s) are not source-controlled. "
                "Commit non-private objects on the task branch; keep secrets "
                "in .memory/private/.",
            )
        )
    relations_root = root / ".memory/relations"
    _landed_relations, untracked_relations = memory_object_counts(root, relations_root)
    if untracked_relations:
        errors.append(
            issue(
                "memory-untracked",
                ".memory/relations",
                f"{untracked_relations} Memory relation(s) are not source-controlled. "
                "Commit non-private relations on the task branch.",
            )
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


def memory_content_hash(sidecar: dict, body: str) -> str:
    """Recompute one object's `content_hash` from its sidecar and body.

    Ported from the pinned Memory CLI's own `src/storage/hashes.ts`:
    sha256 over the canonical sidecar with `content_hash` removed, a newline,
    then the LF-normalized body. Canonical JSON is sorted-key and compact,
    and is emitted with `ensure_ascii=False` because the CLI's
    `JSON.stringify` leaves non-ASCII characters unescaped.

    Reproduced against every stored hash in this repository rather than
    inferred: 337 of 337 objects match, which is what makes a mismatch
    evidence of a hand edit rather than of a recipe this got wrong. If a CLI
    upgrade ever changes the recipe, this fails loudly on the whole store at
    once instead of drifting on one object.
    """
    payload = {key: value for key, value in sidecar.items() if key != "content_hash"}
    canonical = json.dumps(
        payload, sort_keys=True, separators=(",", ":"), ensure_ascii=False
    )
    blob = canonical + "\n" + body.replace("\r\n", "\n")
    return "sha256:" + hashlib.sha256(blob.encode("utf-8")).hexdigest()


def memory_update_timestamps(root: Path) -> dict[str, set[str]]:
    """Map each object id to the timestamps of its update-class events.

    A malformed line is skipped rather than fatal: `.memory/events.jsonl`
    merges with `merge=union` (`.gitattributes`), so this must stay readable
    on a tree mid-merge instead of turning a merge artifact into a gate
    failure that hides the real problem.
    """
    stamps: dict[str, set[str]] = {}
    events_path = root / ".memory/events.jsonl"
    if not events_path.is_file():
        return stamps
    for line in events_path.read_text(encoding="utf-8").splitlines():
        line = line.strip()
        if not line:
            continue
        try:
            event = json.loads(line)
        except json.JSONDecodeError:
            continue
        if not isinstance(event, dict) or event.get("event") not in MEMORY_UPDATE_EVENTS:
            continue
        identifier, timestamp = event.get("id"), event.get("timestamp")
        if isinstance(identifier, str) and isinstance(timestamp, str):
            stamps.setdefault(identifier, set()).add(timestamp)
    return stamps


def validate_memory_integrity(root: Path = ROOT) -> list[dict[str, str]]:
    """Enforce the store signals `--skip-external` must still catch.

    `memory check` reports a stale `content_hash` as a *warning* and still
    exits 0 (#4460), so `run_memory_check` sees success; and nothing at all
    compares `updated_at` against the event log (#4465). Both are recomputed
    here, in-process, so they run under `--skip-external` -- the command
    AGENTS.md prescribes for memory work, and the one CI runs without the
    pinned CLI on PATH. Project-scoped objects that set `scope.task` or
    `scope.branch` are the same class of hole: the CLI fail-closes with
    `ObjectScopeInvalid` (#5136) while this validator used to exit 0 (#5156).

    Deliberately no grandfathering set. Every one of the 245 live objects
    with a bumped `updated_at` and an event carries an event whose timestamp
    equals that `updated_at` exactly, and none carries an update event
    without the bump, so the strict rule costs nothing today and an
    exemption list would only hide the next hole.
    """
    errors: list[dict[str, str]] = []
    memory_dir = root / ".memory"
    update_stamps = memory_update_timestamps(root)
    for sidecar_path in sorted(memory_dir.glob("memory/**/*.json")):
        relative = sidecar_path.relative_to(root).as_posix()
        try:
            sidecar = read_json(sidecar_path)
        except (OSError, json.JSONDecodeError) as error:
            errors.append(issue("memory-object", relative, str(error)))
            continue
        scope = sidecar.get("scope")
        if isinstance(scope, dict) and scope.get("kind") == "project":
            set_fields = [
                field for field in ("branch", "task") if scope.get(field) is not None
            ]
            if set_fields:
                errors.append(
                    issue(
                        "memory-project-scope",
                        relative,
                        "Project-scoped memory must not set "
                        f"{' or '.join(set_fields)} (issue #5136).",
                    )
                )
        body_path = sidecar.get("body_path")
        if not isinstance(body_path, str) or not body_path:
            # Not a body-backed object; nothing to hash against.
            continue
        body_file = memory_dir / body_path
        if not body_file.is_file():
            errors.append(
                issue(
                    "memory-body-missing",
                    relative,
                    f"body_path points at {body_path!r}, which does not exist.",
                )
            )
            continue
        recorded = sidecar.get("content_hash")
        # read_text applies universal newlines, so a CRLF checkout already
        # arrives LF-normalized; memory_content_hash normalizes again for a
        # body that reached us some other way.
        actual = memory_content_hash(sidecar, body_file.read_text(encoding="utf-8"))
        if recorded != actual:
            errors.append(
                issue(
                    "memory-content-hash",
                    relative,
                    f"content_hash {recorded!r} does not match the current body and "
                    f"metadata (recomputed {actual!r}). A hand-edited body must be "
                    "written back with a recomputed hash; `memory check` only warns "
                    "about this (issue #4460).",
                )
            )
        identifier = sidecar.get("id")
        created_at, updated_at = sidecar.get("created_at"), sidecar.get("updated_at")
        bumped = (
            isinstance(identifier, str)
            and isinstance(created_at, str)
            and isinstance(updated_at, str)
            and updated_at > created_at
        )
        if bumped and updated_at not in update_stamps.get(identifier, set()):
            errors.append(
                issue(
                    "memory-update-event",
                    relative,
                    f"updated_at {updated_at!r} is later than created_at but no "
                    f"{'/'.join(sorted(MEMORY_UPDATE_EVENTS))} event for {identifier!r} "
                    "carries that timestamp. Append one line to "
                    '.memory/events.jsonl: {"actor":"agent","event":"memory.updated",'
                    f'"id":"{identifier}","timestamp":"{updated_at}"}} (issue #4465).',
                )
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
    workflow_paths = (
        root / ".github/workflows/pr-gate.yml",
        root / ".github/workflows/agent-plugin-acceptance.yml",
    )
    workflows = "\n".join(
        path.read_text(encoding="utf-8") for path in workflow_paths if path.is_file()
    )
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
        errors.extend(parity_check_errors(item, root, relative, workflows))
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
    item: dict, root: Path, relative: Path, workflows: str
) -> list[dict[str, str]]:
    """Check that a capability names a real PR-focused or scheduled-full test."""
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
    module = ".".join(check_path.with_suffix("").parts)
    discovery_patterns = re.findall(
        r"unittest\s+discover[^\n]*?-p\s+['\"]([^'\"]+)['\"]", workflows
    )
    discovered = any(fnmatch.fnmatchcase(check_path.name, pattern) for pattern in discovery_patterns)
    if module not in workflows and not discovered:
        return [
            issue(
                "host-parity-ci",
                relative.as_posix(),
                f"{item['id']}.check is not run by focused PR or scheduled full gate: {check!r}",
            )
        ]
    return []


def validate_harness_reachability(root: Path) -> list[dict[str, str]]:
    """Report every orphaned harness element, for the local gate as well as CI.

    #4531 gap 2. `AGENTS.md` names this command for guidance work, and it is
    the one an agent on a machine with no CI actually runs -- yet it exited 0
    on a fully orphaned harness, because only the unittest module ran the
    walk. A guarantee enforced solely by a GitHub Actions job is not a
    guarantee for a machine that never opens a pull request, and the owner
    requirement is explicitly about any machine.

    Every defect the walk reports is surfaced, not only orphans: a broken link
    reads as coverage while providing none, a named path that no longer
    resolves is the #4481 fail-open shape, and a reasonless exemption is an
    orphan wearing a hat. Reporting one category locally and the rest only in
    CI would rebuild the same asymmetry one level down.

    Fails closed on a walk that cannot run. An unreadable budget file or a
    tree `git` will not answer for is a setup defect in its own right, and
    silently returning no errors is exactly the vacuous green this function
    exists to remove.
    """
    try:
        report = harness_report(root)
    except (OSError, ValueError, KeyError, subprocess.SubprocessError) as error:
        return [issue("harness-reachability", "harness", f"walk failed: {error}")]
    categories = (
        ("orphans", "unreachable from the entrypoint"),
        ("broken_links", "broken link in the reachable graph"),
        ("stale_named_paths", "named path resolves to nothing tracked"),
        ("exemption_problems", "invalid exemption"),
    )
    return [
        issue("harness-reachability", "harness", f"{label}: {detail}")
        for key, label in categories
        for detail in report.get(key, [])
    ]


def run_memory_check(root: Path) -> list[dict[str, str]]:
    """Run `memory check` against the PATH-resolved Memory CLI.

    Agents invoke `memory` from PATH (AGENTS.md, "Memory & Learning Session"),
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


def reduction_percent(guidance_bytes: int, baseline: int | None) -> float | None:
    """Return the reduction against a configured baseline, or None if there is none.

    #3745 retired the global reduction floor deliberately, so no baseline is
    configured and this returns None on every real run. It must stay None rather
    than 0: a percentage is a claim about a measurement, and reporting 0 for
    "nobody measured" reads as "measured, and nothing was saved" -- the worse of
    the two, and the one that sent issue #4452 looking for a leak that is not
    there.
    """
    if not baseline:
        return None
    return round((1 - guidance_bytes / baseline) * 100, 2)


def _git_paths(root: Path, *arguments: str) -> set[str] | None:
    """Return repository-relative paths from a read-only git query, or None.

    Every query is NUL-delimited (`-z`). Without it git applies `core.quotepath`
    and renders a non-ASCII path as a quoted C-style string --
    `".memory/.../caf\\303\\251.json"`, trailing quote included -- which then
    fails a `.endswith(".json")` match and drops the object from the count
    entirely. A newline in a filename splits one path into two lines for the
    same reason. `-z` emits raw bytes with an unambiguous separator, so neither
    shape can misparse.

    None means git could not answer -- no binary, or not a repository -- and is
    deliberately distinct from the empty set, which means git answered "none".
    """
    executable = shutil.which("git")
    if executable is None:
        return None
    try:
        completed = subprocess.run(  # nosec B603 - fixed, read-only git queries.
            [executable, *arguments],
            cwd=root,
            capture_output=True,
            text=True,
            timeout=30,
            check=False,
        )
    except (OSError, subprocess.SubprocessError):
        return None
    if completed.returncode != 0:
        return None
    return {entry for entry in completed.stdout.split("\0") if entry}


def guidance_byte_counts(root: Path, paths: list[Path]) -> tuple[int, int | None]:
    """Return (bytes of landed guidance, bytes present but not landed) for `paths`.

    #4509: this was a bare sum over `expand_globs`, a filesystem walk with no
    git view, so a guidance file that was never `git add`ed weighed exactly as
    much as a committed one -- the defect #4495 reported for the object count
    printed beside it on the same banner line, and #4507 fixed there. Same two
    git views, same meaning of None, deliberately: half a verified banner reads
    as a wholly verified one.

    Bytes are read from the worktree, not from the HEAD blob. The figure has to
    describe what an author just wrote, or the validator stops answering the
    question it is run to answer -- whether the edit in hand fits the budget.
    What git decides is which files are part of the surface, not what they say.

    Untracked bytes are returned rather than discarded, because for this metric
    the uncommitted figure is the interesting one: `active_guidance_globs`
    covers `.agents/skills/*/SKILL.md` and `.claude/agents/*.md`, so a new skill
    is measured before it is committed, every time. Disclosed and labelled, it
    informs; folded into the total, it inflates a number agents quote as proof.
    """
    relative = {path.relative_to(root).as_posix(): path for path in paths}

    def measured(names: set[str]) -> int:
        # LF-normalized to match validate_total_reduction and the LF blobs CI
        # sees. Read through the mapping, never re-joined from the name, so a
        # path git reports but the glob did not produce cannot be read at all.
        return sum(
            len(relative[name].read_text(encoding="utf-8").encode("utf-8")) for name in names
        )

    if not relative:
        # An empty pathspec list means "everything" to git, not "nothing", so
        # both queries would answer about the whole repository. The intersection
        # below would still be empty, but the queries are pointless and slow.
        return 0, 0
    # `-z` must precede the `--`, or git reads it as a pathspec and matches
    # nothing -- silently, with exit code 0 and an empty result.
    landed = _git_paths(
        root, "ls-tree", "-r", "-z", "--name-only", "HEAD", "--", *sorted(relative)
    )
    visible = _git_paths(
        root,
        "ls-files",
        "-z",
        "--cached",
        "--others",
        "--exclude-standard",
        "--",
        *sorted(relative),
    )
    if landed is None or visible is None:
        return measured(set(relative)), None
    tracked = set(relative) & landed
    return measured(tracked), measured((set(relative) & visible) - landed)


def memory_object_counts(root: Path, memory_root: Path) -> tuple[int, int | None]:
    """Return (landed objects, objects present but not landed) under `memory_root`.

    #4495: this was `memory_root.rglob("*.json")`, a filesystem walk that never
    consulted git, so an object that was never `git add`ed was counted exactly
    like a committed one -- and this banner is what agents in this repository
    are told to quote as proof of their change.

    Two git views, because #4497 found the mirror defect by reading only one.
    "Has this landed" is a question about HEAD and nothing else, so a staged
    file is not landed. "Does this exist and is nobody hiding it" is a question
    about the worktree minus ignored files, which is
    `--cached --others --exclude-standard`. The index alone answers neither: it
    is neither what is there nor what has landed.

    The second element is None when git could not answer at all -- outside a
    repository there is no "landed" to speak of, and reporting 0 there would
    publish a non-measurement in the units of a measurement (see
    `reduction_percent`).
    """
    # No special case for a missing directory, deliberately. Two earlier
    # attempts both put a false statement in the banner: `(0, None)` said
    # "unverified: git could not answer" when git was never asked, and `(0, 0)`
    # said "zero, verified" for a tree where git could still see committed
    # objects. Both fired BEFORE either query, which is what made them wrong.
    #
    # Reachable here rather than hypothetical: R9 exists because plain `git
    # worktree add` aborts part-way through checking out over-long `.memory/**`
    # paths (#4126), leaving exactly that state -- no store on disk, a HEAD full
    # of objects. A sparse checkout does the same.
    #
    # Letting the queries answer needs no guard. `relative_to` is a pure path
    # operation, and git accepts a pathspec matching nothing with exit 0 and
    # empty output, so `landed` is what HEAD holds and `present` is empty. The
    # fallback below still covers the genuinely unanswerable case.
    relative = memory_root.relative_to(root).as_posix()
    # `-z` must precede the `--`, or git reads it as a pathspec and matches
    # nothing -- silently, with exit code 0 and an empty result.
    landed = _git_paths(
        root, "ls-tree", "-r", "-z", "--name-only", "HEAD", "--", relative
    )
    visible = _git_paths(
        root,
        "ls-files",
        "-z",
        "--cached",
        "--others",
        "--exclude-standard",
        "--",
        relative,
    )
    if landed is None or visible is None:
        walked = len(list(memory_root.rglob("*.json"))) if memory_root.is_dir() else 0
        return walked, None
    objects = {path for path in landed if path.endswith(".json")}
    present = {path for path in visible if path.endswith(".json")}
    return len(objects), len(present - objects)


def format_banner(metrics: dict) -> str:
    """Render the one-line summary a passing run prints, omitting what was not measured."""
    # Both halves of this line carry the same three states, or the line is half
    # verified and reads as wholly verified -- #4490's shape, and the reason
    # #4509 was filed the day after #4507 landed the labelling next door.
    # Indexed, not `.get`, for the reason spelled out below the object count.
    guidance = f"{metrics['guidance_bytes']} guidance bytes"
    untracked_bytes = metrics["guidance_bytes_untracked"]
    if untracked_bytes is None:
        guidance += " (unverified: git could not answer)"
    elif untracked_bytes:
        # `+`, because these bytes are additional to the figure rather than
        # part of it. The object count's parenthetical reads the same way.
        guidance += f" (+{untracked_bytes} untracked)"
    parts = [guidance]
    if metrics.get("guidance_reduction_percent") is not None:
        parts.append(f"{metrics['guidance_reduction_percent']}% reduction")
    objects = f"{metrics['memory_objects']} memory objects"
    # Three states, and the bare wording belongs to exactly one of them.
    # `338 memory objects` is quotable as a claim about main, so it is reserved
    # for a git-verified count with nothing outstanding. A count git could not
    # verify says so: the fallback is the pre-fix filesystem walk, and letting
    # it print the quotable wording restores #4495's hole in the one path where
    # the fix does not apply.
    # Indexed, not `.get`: a caller that omits the key would otherwise receive
    # the bare, quotable wording by default, which is the one thing this
    # labelling exists to prevent. `collect_metrics` always sets it, so a
    # KeyError here means a new caller has to make the choice deliberately.
    untracked = metrics["memory_objects_untracked"]
    if untracked is None:
        objects += " (unverified: git could not answer)"
    elif untracked:
        objects += f" ({untracked} untracked)"
    parts.append(objects)
    return f"Agent setup is valid: {', '.join(parts)}."


def collect_metrics(root: Path = ROOT) -> dict:
    """Collect stable context and memory size metrics."""
    budget = load_budget(root / "scripts/ci/agent_guidance_budget.json")
    guidance_paths = expand_globs(root, budget.get("total_guidance_globs", []))
    # Two surfaces, two documented limits -- see limit_sources in the budget.
    host_body_chars: dict[str, int] = {}
    host_body_bytes: dict[str, int] = {}
    for host, configured_paths in budget.get("host_contexts", {}).items():
        host_body_chars[host] = sum(
            len(path.read_text(encoding="utf-8"))
            for configured_path in configured_paths
            if (path := root / configured_path).is_file()
        )
        host_body_bytes[host] = always_loaded_body_chars(root, configured_paths)[0]
    host_listing_chars = {
        host: skill_listing_chars(root, patterns)
        for host, patterns in budget.get("host_skill_metadata_globs", {}).items()
    }
    baseline = budget.get("reduction_baseline_bytes")
    guidance_bytes, untracked_guidance_bytes = guidance_byte_counts(root, guidance_paths)
    # Derived from the landed figure, so a percentage cannot be moved by bytes
    # nobody else can see.
    reduction = reduction_percent(guidance_bytes, baseline)
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
    landed_objects, unlanded_objects = memory_object_counts(root, memory_root)
    return {
        "guidance_bytes": guidance_bytes,
        "guidance_bytes_untracked": untracked_guidance_bytes,
        "guidance_reduction_percent": reduction,
        # Keep the legacy character report alongside the byte budget metric.
        "always_loaded_body_chars": host_body_chars,
        "always_loaded_body_bytes": host_body_bytes,
        "skill_listing_chars": host_listing_chars,
        "memory_objects": landed_objects,
        "memory_objects_untracked": unlanded_objects,
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
    return {
        "worktrees": report,
        "worktree_advisories": format_advisories(
            report,
            check_pull_requests_command=(
                "py -3 scripts/ci/worktree_hygiene.py --check-pull-requests"
            ),
        ),
    }


def validate_repository(
    root: Path = ROOT, *, run_external: bool = True
) -> tuple[list[dict[str, str]], dict]:
    """Run all agent setup checks."""
    errors = [
        *validate_guidance(root),
        *[issue("semantic-owner", "scripts/ci/agent_ownership.json", message)
          for message in validate_ownership(root)],
        *[
            issue("documentation-boundary", "documentation", message)
            for message in validate_documentation(root)
        ],
        *(
            [
                issue("chaos-engine-readme", "chaos-engine/README.md", message)
                for message in validate_chaos_readme(root)
            ]
            if (root / "chaos-engine").is_dir()
            else []
        ),
        *validate_memory_setup(root),
        *validate_memory_integrity(root),
        *validate_host_parity(root),
        *validate_skill_hygiene(root),
        *validate_harness_reachability(root),
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
        print(format_banner(metrics))
    # Advisories print whether or not the gate passed: they describe work that
    # is at risk, not a broken setup, and a passing run is exactly when an
    # agent would otherwise stop reading.
    for advisory in metrics.get("worktree_advisories", []):
        print(advisory)
    return 1 if errors else 0


if __name__ == "__main__":
    raise SystemExit(main())
