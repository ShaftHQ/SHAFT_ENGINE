#!/usr/bin/env python3
"""Plan and verify ChaosEngine-owned runtime dependencies."""

from __future__ import annotations

import argparse
from contextlib import contextmanager, nullcontext
import hashlib
import json
import os
import re
import secrets
import shutil
import stat
import subprocess  # nosec B404 - fixed list-form dependency commands from tracked spec.
import sys
from datetime import datetime, timedelta, timezone
from pathlib import Path


RECEIPT_SCHEMA = 1
RUNTIME_CONTRACT_VERSION = 2
STALE_AFTER = timedelta(hours=24)
RECEIPT_NAME = "receipt.json"
LOCK_MAGIC = b"chaos-engine-dependencies-lock-v1\n"
BUILD_MARKER_MAGIC = "chaos-engine-dependencies-build-v1\n"
BUILD_MARKER_OWNED_SUFFIX = ".owned"
POINTER_NAME = ".chaos-engine-runtime-current.json"
POINTER_SCHEMA = 1
GENERATIONS_NAME = ".chaos-engine-runtime-generations"
MAX_CONTROL_BYTES = 4 * 1024 * 1024
HEX_ID = re.compile(r"[0-9a-f]{32}")
HEX_DIGEST = re.compile(r"[0-9a-f]{64}")
PYTHON_DISPATCH = (
    "import importlib.metadata as m,sys;"
    "e=next(e for e in m.distribution(sys.argv[1]).entry_points "
    "if e.group=='console_scripts' and e.name==sys.argv[2]);"
    "sys.argv=[sys.argv[2],*sys.argv[3:]];raise SystemExit(e.load()())"
)


class DanglingRuntimeLink(ValueError):
    pass


def is_link_or_reparse(path: Path) -> bool:
    if path.is_symlink():
        return True
    try:
        attributes = getattr(os.lstat(path), "st_file_attributes", 0)
    except FileNotFoundError:
        return False
    return bool(attributes & 0x400)


def runtime_entries(runtime: Path) -> list[Path]:
    """List a runtime tree without traversing links or reparse points."""
    entries: list[Path] = []
    pending = [runtime]
    while pending:
        directory = pending.pop()
        with os.scandir(directory) as children:
            paths = sorted((Path(child.path) for child in children), reverse=True)
        for path in paths:
            entries.append(path)
            if not is_link_or_reparse(path) and path.is_dir():
                pending.append(path)
    return sorted(entries)


def executable(directory: Path, name: str) -> str:
    suffix = ".exe" if os.name == "nt" else ""
    return str(directory / f"{name}{suffix}")


def npm_executable(directory: Path, name: str) -> str:
    suffix = ".cmd" if os.name == "nt" else ""
    return str(directory / f"{name}{suffix}")


@contextmanager
def runtime_lock(runtime: Path):
    lock = runtime.with_name(f"{runtime.name}.lock")
    flags = os.O_RDWR | getattr(os, "O_BINARY", 0)
    created = False
    try:
        descriptor = os.open(lock, flags | os.O_CREAT | os.O_EXCL, 0o600)
        created = True
    except FileExistsError:
        if is_link_or_reparse(lock):
            raise ValueError(f"dependency lock is a link or reparse point: {lock}")
        descriptor = os.open(lock, flags)
    try:
        stream = os.fdopen(descriptor, "r+b", closefd=True)
    except BaseException:
        os.close(descriptor)
        raise
    try:
        opened = os.fstat(stream.fileno())
        named = os.stat(lock, follow_symlinks=False)
        if (opened.st_dev, opened.st_ino) != (named.st_dev, named.st_ino):
            raise ValueError(f"dependency lock collision: {lock}")
        if os.name == "nt":
            import msvcrt

            msvcrt.locking(stream.fileno(), msvcrt.LK_NBLCK, 1)
        else:
            import fcntl

            fcntl.flock(stream.fileno(), fcntl.LOCK_EX | fcntl.LOCK_NB)
    except (OSError, BlockingIOError) as error:
        stream.close()
        raise RuntimeError(f"dependency runtime is locked: {runtime}") from error
    except BaseException:
        stream.close()
        raise
    try:
        stream.seek(0)
        existing = stream.read()
        if created:
            stream.seek(0)
            stream.write(LOCK_MAGIC)
            stream.truncate()
            stream.flush()
        elif existing != LOCK_MAGIC:
            raise ValueError(f"dependency lock is not ChaosEngine-owned: {lock}")
        yield
    finally:
        try:
            if os.name == "nt":
                stream.seek(0)
                msvcrt.locking(stream.fileno(), msvcrt.LK_UNLCK, 1)
            else:
                fcntl.flock(stream.fileno(), fcntl.LOCK_UN)
        finally:
            stream.close()


def sha256(path: Path) -> str:
    return hashlib.sha256(path.read_bytes()).hexdigest()


def json_integrity(value: dict[str, object]) -> str:
    body = {
        key: item
        for key, item in value.items()
        if key not in {"integritySha256", "receiptIntegritySha256"}
    }
    encoded = json.dumps(body, sort_keys=True, separators=(",", ":")).encode()
    return hashlib.sha256(encoded).hexdigest()


def _validate_generation_record(value: object) -> dict[str, str]:
    fields = {
        "generationId",
        "specificationSha256",
        "coreSha256",
        "receiptSha256",
    }
    if not isinstance(value, dict) or set(value) != fields:
        raise ValueError("dependency generation record is invalid")
    record = {key: item for key, item in value.items() if isinstance(item, str)}
    if set(record) != fields or HEX_ID.fullmatch(record["generationId"]) is None:
        raise ValueError("dependency generation identifier is invalid")
    if any(
        HEX_DIGEST.fullmatch(record[name]) is None
        for name in fields - {"generationId"}
    ):
        raise ValueError("dependency generation digest is invalid")
    return record


def publish_pointer(
    project: Path,
    active: dict[str, str],
    previous: dict[str, str] | None,
    *,
    transaction_id: str | None = None,
) -> dict[str, object]:
    """Atomically select immutable generations using identifiers, never paths."""
    project = project.absolute()
    active = _validate_generation_record(active)
    previous = _validate_generation_record(previous) if previous is not None else None
    transaction = transaction_id or secrets.token_hex(16)
    if HEX_ID.fullmatch(transaction) is None:
        raise ValueError("dependency transaction identifier is invalid")
    pointer: dict[str, object] = {
        "schemaVersion": POINTER_SCHEMA,
        "transactionId": transaction,
        "active": active,
        "previous": previous,
    }
    pointer["integritySha256"] = json_integrity(pointer)
    path = project / POINTER_NAME
    temporary = project / f"{POINTER_NAME}.tmp.{transaction}.{secrets.token_hex(8)}"
    if is_link_or_reparse(path):
        raise ValueError("dependency pointer is a link or reparse point")
    descriptor = os.open(
        temporary,
        os.O_WRONLY | os.O_CREAT | os.O_EXCL | getattr(os, "O_BINARY", 0),
        0o600,
    )
    try:
        with os.fdopen(descriptor, "w", encoding="utf-8") as stream:
            json.dump(pointer, stream, indent=2, sort_keys=True)
            stream.write("\n")
            stream.flush()
            os.fsync(stream.fileno())
        temporary.replace(path)
        if os.name != "nt":
            directory = os.open(project, os.O_RDONLY | getattr(os, "O_DIRECTORY", 0))
            try:
                os.fsync(directory)
            finally:
                os.close(directory)
    finally:
        if temporary.exists() and not is_link_or_reparse(temporary):
            temporary.unlink()
    return pointer


def _relative_parts(relative: str, label: str) -> tuple[str, ...]:
    path = Path(relative)
    if path.is_absolute() or not path.parts or ".." in path.parts:
        raise ValueError(f"dependency {label} path is unsafe")
    return path.parts


def _open_regular_relative(root: Path, relative: str, label: str) -> int:
    """Open a regular descendant without following POSIX links in any component."""
    parts = _relative_parts(relative, label)
    nofollow = getattr(os, "O_NOFOLLOW", 0)
    binary = getattr(os, "O_BINARY", 0)
    if os.name != "nt" and os.open in os.supports_dir_fd:
        directory = os.open(
            root,
            os.O_RDONLY | getattr(os, "O_DIRECTORY", 0) | nofollow,
        )
        try:
            for part in parts[:-1]:
                child = os.open(
                    part,
                    os.O_RDONLY | getattr(os, "O_DIRECTORY", 0) | nofollow,
                    dir_fd=directory,
                )
                os.close(directory)
                directory = child
            descriptor = os.open(
                parts[-1], os.O_RDONLY | binary | nofollow, dir_fd=directory
            )
        except OSError as error:
            raise ValueError(f"dependency {label} has an unsafe ancestor or link") from error
        finally:
            os.close(directory)
    else:
        current = root
        for part in parts:
            current /= part
            if is_link_or_reparse(current):
                raise ValueError(f"dependency {label} has an unsafe ancestor or link")
        try:
            descriptor = os.open(current, os.O_RDONLY | binary | nofollow)
        except OSError as error:
            raise ValueError(f"dependency {label} is missing or unsafe") from error
    opened = os.fstat(descriptor)
    if not stat.S_ISREG(opened.st_mode):
        os.close(descriptor)
        raise ValueError(f"dependency {label} is not a regular file")
    return descriptor


def _read_regular_relative(
    root: Path, relative: str, label: str, limit: int | None = None
) -> bytes:
    descriptor = _open_regular_relative(root, relative, label)
    try:
        opened = os.fstat(descriptor)
        if limit is not None and opened.st_size > limit:
            raise ValueError(f"dependency {label} is too large")
        chunks: list[bytes] = []
        remaining = None if limit is None else limit + 1
        while remaining is None or remaining > 0:
            chunk = os.read(descriptor, 1024 * 1024 if remaining is None else min(1024 * 1024, remaining))
            if not chunk:
                break
            chunks.append(chunk)
            if remaining is not None:
                remaining -= len(chunk)
        data = b"".join(chunks)
        if limit is not None and len(data) > limit:
            raise ValueError(f"dependency {label} is too large")
        return data
    finally:
        os.close(descriptor)


def _bounded_json(root: Path, relative: str, label: str) -> tuple[dict[str, object], bytes]:
    try:
        data = _read_regular_relative(root, relative, label, MAX_CONTROL_BYTES)
        value = json.loads(data)
    except (OSError, UnicodeDecodeError, json.JSONDecodeError) as error:
        raise ValueError(f"dependency {label} is missing or invalid") from error
    if not isinstance(value, dict):
        raise ValueError(f"dependency {label} is invalid")
    return value, data


def active_generation(project: Path) -> tuple[Path, dict[str, object]]:
    project = project.absolute()
    pointer, _ = _bounded_json(project, POINTER_NAME, "pointer")
    if (
        pointer.get("schemaVersion") != POINTER_SCHEMA
        or HEX_ID.fullmatch(str(pointer.get("transactionId", ""))) is None
        or pointer.get("integritySha256") != json_integrity(pointer)
    ):
        raise ValueError("dependency pointer schema or integrity is invalid")
    active = _validate_generation_record(pointer.get("active"))
    if pointer.get("previous") is not None:
        _validate_generation_record(pointer["previous"])
    generation = project / GENERATIONS_NAME / active["generationId"]
    receipt, receipt_bytes = _bounded_json(
        project,
        f"{GENERATIONS_NAME}/{active['generationId']}/{RECEIPT_NAME}",
        "generation receipt",
    )
    if hashlib.sha256(receipt_bytes).hexdigest() != active["receiptSha256"]:
        raise ValueError("dependency generation receipt digest drift detected")
    if receipt.get("receiptIntegritySha256") != json_integrity(receipt):
        raise ValueError("dependency generation receipt integrity drift detected")
    if receipt.get("specificationSha256") != active["specificationSha256"]:
        raise ValueError("dependency generation specification digest drift detected")
    if receipt.get("coreSha256") != active["coreSha256"]:
        raise ValueError("dependency generation core digest drift detected")
    core = _read_regular_relative(
        project, ".chaos-engine/manifest.json", "installed core manifest", MAX_CONTROL_BYTES
    )
    if hashlib.sha256(core).hexdigest() != active["coreSha256"]:
        raise ValueError("dependency installed core digest drift detected")
    return generation, pointer


def dispatch_command(
    generation: Path,
    receipt: dict[str, object],
    tool: str,
    arguments: list[str],
) -> list[str]:
    tools = receipt.get("tools")
    record = tools.get(tool) if isinstance(tools, dict) else None
    dispatch = record.get("dispatch") if isinstance(record, dict) else None
    if not isinstance(dispatch, dict):
        raise ValueError(f"dependency tool dispatch is missing: {tool}")
    if dispatch.get("kind") == "python":
        relative = dispatch.get("interpreter")
        distribution = dispatch.get("distribution")
        entrypoint = dispatch.get("entrypoint")
        expected_digest = dispatch.get("interpreterSha256")
        if not all(
            isinstance(item, str) and item
            for item in (relative, distribution, entrypoint, expected_digest)
        ) or HEX_DIGEST.fullmatch(str(expected_digest)) is None:
            raise ValueError(f"dependency Python dispatch is invalid: {tool}")
        interpreter = generation / str(relative)
        try:
            actual_digest = hashlib.sha256(
                _read_regular_relative(
                    generation, str(relative), f"Python interpreter for {tool}"
                )
            ).hexdigest()
        except ValueError as error:
            raise ValueError(
                f"dependency Python interpreter is unsafe or a link: {tool}"
            ) from error
        if actual_digest != expected_digest:
            raise ValueError(f"dependency Python interpreter drift detected: {tool}")
        return [
            str(interpreter),
            "-c",
            PYTHON_DISPATCH,
            str(distribution),
            str(entrypoint),
            *arguments,
        ]
    raise ValueError(f"dependency tool dispatch kind is unsupported: {tool}")


def unlink_owned_marker(path: Path, identity: tuple[int, int]) -> None:
    if is_link_or_reparse(path):
        raise ValueError("dependency build marker ownership drift detected")
    scratch = path.with_name(f"{path.name}{BUILD_MARKER_OWNED_SUFFIX}")
    if scratch.exists() or is_link_or_reparse(scratch):
        raise ValueError(f"dependency build marker scratch collision: {scratch}")
    path.replace(scratch)
    try:
        named = os.stat(scratch, follow_symlinks=False)
        if (named.st_dev, named.st_ino) != identity:
            raise ValueError("dependency build marker ownership drift detected")
        if scratch.read_text(encoding="utf-8") != BUILD_MARKER_MAGIC:
            raise ValueError("dependency build marker ownership drift detected")
        scratch.unlink()
    except BaseException:
        if scratch.exists() and not path.exists():
            scratch.replace(path)
        raise


def specification_digest(specification: dict[str, object]) -> str:
    tool_specification = {key: value for key, value in specification.items() if key != "components"}
    encoded = json.dumps(tool_specification, sort_keys=True, separators=(",", ":")).encode()
    return hashlib.sha256(encoded).hexdigest()


def capability_policy_digest(specification: dict[str, object]) -> str:
    components = specification.get("components")
    if not isinstance(components, dict):
        raise ValueError("dependency capability policy is missing or invalid")
    encoded = json.dumps(components, sort_keys=True, separators=(",", ":")).encode()
    return hashlib.sha256(encoded).hexdigest()


def is_generated_python_cache(relative: str, *, directory: bool = False) -> bool:
    parts = relative.split("/")
    if directory:
        return bool(parts) and parts[-1] == "__pycache__"
    return len(parts) >= 2 and parts[-2] == "__pycache__" and parts[-1].endswith(".pyc")


def ownership_digest(
    files: dict[str, str], links: list[dict[str, str]] | None = None
) -> str:
    digest = hashlib.sha256()
    for relative, file_digest in sorted(files.items()):
        digest.update(relative.encode())
        digest.update(b"\0")
        digest.update(bytes.fromhex(file_digest))
    for link in sorted(links or [], key=lambda item: item["path"]):
        digest.update(b"link\0")
        digest.update(link["path"].encode())
        digest.update(b"\0")
        digest.update(link["target"].encode())
    return digest.hexdigest()


def valid_link_records(value: object) -> bool:
    return isinstance(value, list) and all(
        isinstance(link, dict)
        and set(link) == {"path", "target"}
        and isinstance(link["path"], str)
        and isinstance(link["target"], str)
        for link in value
    )


def _lexical_path(path: Path) -> Path:
    value = os.path.abspath(path)
    if os.name == "nt" and value.startswith("\\\\?\\UNC\\"):
        value = f"\\\\{value[8:]}"
    elif os.name == "nt" and value.startswith("\\\\?\\"):
        value = value[4:]
    return Path(value)


def _managed_link_target(
    runtime: Path, path: Path, active: set[Path] | None = None
) -> Path:
    root = _lexical_path(runtime)
    candidate = _lexical_path(path)
    if not candidate.is_relative_to(root):
        raise ValueError(f"dependency runtime link escapes the runtime: {path}")
    seen = active or set()
    current = root
    for part in candidate.relative_to(root).parts:
        current /= part
        if current.is_symlink():
            identity = _lexical_path(current)
            if identity in seen:
                raise ValueError(f"dependency runtime link cycle detected: {path}")
            raw_target = Path(os.readlink(current))
            target = _lexical_path(
                raw_target if raw_target.is_absolute() else current.parent / raw_target
            )
            if not target.is_relative_to(root):
                raise ValueError(f"dependency runtime link escapes the runtime: {current}")
            current = _managed_link_target(root, target, seen | {identity})
        elif is_link_or_reparse(current):
            raise ValueError(f"dependency runtime contains an unsupported reparse point: {current}")
        elif not current.exists():
            raise DanglingRuntimeLink(f"dependency runtime link is dangling: {path}")
    return current


def canonicalize_runtime_links(runtime: Path) -> None:
    """Validate every managed link, then rewrite safe targets for relocation."""
    runtime = _lexical_path(runtime)
    links: list[tuple[Path, str, Path]] = []
    for path in runtime_entries(runtime):
        if path.is_symlink():
            resolved = _managed_link_target(runtime, path)
            raw = Path(os.readlink(path))
            immediate = _lexical_path(raw if raw.is_absolute() else path.parent / raw)
            relative = os.path.relpath(immediate, path.parent)
            links.append((path, relative, resolved))
        elif is_link_or_reparse(path):
            raise ValueError(f"dependency runtime contains an unsupported reparse point: {path}")
    for path, target, resolved in links:
        if os.readlink(path) == target:
            continue
        path.unlink()
        path.symlink_to(target, target_is_directory=resolved.is_dir())
    for path, _, _ in links:
        _managed_link_target(runtime, path)


def normalized_ownership_record(ownership: object) -> object:
    if not isinstance(ownership, dict):
        return ownership
    directories = ownership.get("directories")
    files = ownership.get("files")
    if (
        not isinstance(directories, list)
        or not all(isinstance(path, str) for path in directories)
        or not isinstance(files, dict)
        or not all(isinstance(path, str) and isinstance(digest, str) for path, digest in files.items())
    ):
        return ownership
    normalized = dict(ownership)
    links = ownership.get("links", [])
    if not valid_link_records(links):
        return ownership
    normalized["directories"] = [
        path for path in directories if not is_generated_python_cache(path, directory=True)
    ]
    normalized_files = {
        path: digest
        for path, digest in files.items()
        if not is_generated_python_cache(path)
    }
    normalized["files"] = normalized_files
    normalized["links"] = links
    normalized["sha256"] = ownership_digest(normalized_files, links)
    return normalized


def ownership_record(
    runtime: Path, expected_links: dict[str, str] | None = None
) -> dict[str, object]:
    if is_link_or_reparse(runtime):
        raise ValueError(f"dependency runtime is a link or reparse point: {runtime}")
    files: dict[str, str] = {}
    directories: list[str] = []
    links: list[dict[str, str]] = []
    for path in runtime_entries(runtime):
        relative = path.relative_to(runtime).as_posix()
        if path.is_symlink():
            target = os.readlink(path)
            if Path(target).is_absolute():
                raise ValueError(f"dependency runtime link target is not relative: {relative}")
            try:
                _managed_link_target(runtime, path)
            except DanglingRuntimeLink:
                if expected_links is None or expected_links.get(relative) != target:
                    raise
            links.append({"path": relative, "target": target})
        elif is_link_or_reparse(path):
            raise ValueError(f"dependency runtime contains an unsupported reparse point: {relative}")
        elif path.is_dir():
            if not is_generated_python_cache(relative, directory=True):
                directories.append(relative)
        elif path.is_file() and relative != RECEIPT_NAME:
            if not is_generated_python_cache(relative):
                files[relative] = sha256(path)
    return {
        "directories": directories,
        "files": files,
        "links": links,
        "sha256": ownership_digest(files, links),
    }


def install_plan(runtime: Path, specification: dict[str, object]) -> dict[str, list[list[str]]]:
    tools = specification.get("tools")
    if specification.get("schemaVersion") != 1 or not isinstance(tools, dict):
        raise ValueError("dependency specification schema is unsupported")
    environment = runtime / "bootstrap"
    scripts = environment / ("Scripts" if os.name == "nt" else "bin")
    uv = executable(scripts, "uv")
    npm_prefix = runtime / "npm"
    npm = shutil.which("npm")
    if npm is None:
        raise ValueError("Node.js npm is required to install the Memory tool")
    graphify = tools["graphify"]
    if not isinstance(graphify, dict):
        raise ValueError("graphify dependency specification is invalid")
    return {
        "uv": [
            [sys.executable, "-m", "venv", "--copies", str(environment)],
            [executable(scripts, "python"), "-m", "pip", "install", "--upgrade", str(tools["uv"]["package"])],  # type: ignore[index]
        ],
        "mempalace": [
            [uv, "tool", "install", "--managed-python", "--link-mode", "copy", str(tools["mempalace"]["package"])],  # type: ignore[index]
        ],
        "graphify": [
            [uv, "tool", "install", "--managed-python", "--link-mode", "copy", "--with", str(graphify["with"][0]), str(graphify["package"])],  # type: ignore[index]
        ],
        "memory": [
            [npm, "install", "--prefix", str(npm_prefix), str(tools["memory"]["package"])],  # type: ignore[index]
        ],
    }


def tool_environment(runtime: Path) -> dict[str, str]:
    return {
        "UV_TOOL_DIR": str(runtime / "uv-tools"),
        "UV_TOOL_BIN_DIR": str(runtime / "bin"),
        "UV_CACHE_DIR": str(runtime / "uv-cache"),
        "UV_PYTHON_INSTALL_DIR": str(runtime / "uv-python"),
        "UV_PYTHON_BIN_DIR": str(runtime / "python-bin"),
        "UV_LINK_MODE": "copy",
        "NPM_CONFIG_PREFIX": str(runtime / "npm"),
        "PYTHONDONTWRITEBYTECODE": "1",
    }


def freshness(receipt: dict[str, object], now: datetime | None = None) -> str:
    current = now or datetime.now(timezone.utc)
    try:
        checked = datetime.fromisoformat(str(receipt["checkedAt"]))
    except (KeyError, ValueError) as error:
        raise ValueError("dependency receipt has no valid checkedAt timestamp") from error
    if checked.tzinfo is None:
        raise ValueError("dependency receipt timestamp must include a timezone")
    if checked > current + timedelta(minutes=5):
        raise ValueError("dependency receipt timestamp is in the future")
    return "stale" if current - checked > STALE_AFTER else "fresh"


def load_specification(path: Path) -> dict[str, object]:
    value = json.loads(path.read_text(encoding="utf-8"))
    if not isinstance(value, dict):
        raise ValueError("dependency specification must be an object")
    return value


def probe_plan(runtime: Path) -> dict[str, list[list[str]]]:
    bootstrap = runtime / "bootstrap" / ("Scripts" if os.name == "nt" else "bin")
    bin_dir = runtime / "bin"
    npm_bin = runtime / "npm/node_modules/.bin"
    return {
        "uv": [[executable(bootstrap, "uv"), "--version"]],
        "mempalace": [
            [executable(bin_dir, "mempalace"), "--version"],
            [executable(bin_dir, "mempalace-mcp"), "--help"],
        ],
        "graphify": [[executable(bin_dir, "graphify"), "--version"]],
        "memory": [
            [npm_executable(npm_bin, "memory"), "--help"],
            [npm_executable(npm_bin, "memory-mcp"), "--help"],
        ],
    }


def run_command(command: list[str], environment: dict[str, str]) -> subprocess.CompletedProcess[str]:
    merged = os.environ.copy()
    merged.update(environment)
    return subprocess.run(  # nosec B603
        command, check=True, capture_output=True, text=True, env=merged, timeout=300
    )


def execute_plan(
    runtime: Path,
    specification: dict[str, object],
    runner=run_command,
    now: datetime | None = None,
) -> dict[str, object]:
    environment = tool_environment(runtime)
    completed: dict[str, list[str]] = {}
    for tool, commands in install_plan(runtime, specification).items():
        completed[tool] = []
        for command in commands:
            try:
                result = runner(command, environment)
            except (OSError, subprocess.SubprocessError) as error:
                raise RuntimeError(f"{tool} install command failed: {command[0]}") from error
            completed[tool].append((result.stdout or result.stderr).strip())
    canonicalize_runtime_links(runtime)
    probes: dict[str, list[str]] = {}
    for tool, commands in probe_plan(runtime).items():
        probes[tool] = []
        for command in commands:
            try:
                result = runner(command, environment)
            except (OSError, subprocess.SubprocessError) as error:
                raise RuntimeError(f"{tool} entrypoint probe failed: {command[0]}") from error
            probes[tool].append((result.stdout or result.stderr).strip())
    records = {}
    tools = specification["tools"]  # validated by install_plan
    for tool, commands in probe_plan(runtime).items():
        records[tool] = {
            "requested": tools[tool]["package"],  # type: ignore[index]
            "commands": [relative_command(runtime, command) for command in commands],
            "entrypoints": {
                Path(command[0]).relative_to(runtime).as_posix(): sha256(Path(command[0]))
                for command in commands
            },
            "resolved": probes[tool],
        }
    receipt: dict[str, object] = {
        "schemaVersion": RECEIPT_SCHEMA,
        "runtimeContractVersion": RUNTIME_CONTRACT_VERSION,
        "checkedAt": (now or datetime.now(timezone.utc)).isoformat(),
        "specificationSha256": specification_digest(specification),
        "capabilityPolicySha256": capability_policy_digest(specification),
        "environment": {
            key: value
            if key in {"PYTHONDONTWRITEBYTECODE", "UV_LINK_MODE"}
            else Path(value).relative_to(runtime).as_posix()
            for key, value in environment.items()
        },
        "installed": completed,
        "tools": records,
    }
    metadata = json.dumps(receipt, sort_keys=True, separators=(",", ":")).encode()
    ownership = ownership_record(runtime)
    if not ownership["links"]:
        del ownership["links"]
    ownership["metadataSha256"] = hashlib.sha256(metadata).hexdigest()
    receipt["ownership"] = ownership
    integrity = json.dumps(receipt, sort_keys=True, separators=(",", ":")).encode()
    receipt["receiptIntegritySha256"] = hashlib.sha256(integrity).hexdigest()
    (runtime / RECEIPT_NAME).write_text(
        json.dumps(receipt, indent=2, sort_keys=True) + "\n", encoding="utf-8"
    )
    return receipt


def relative_command(runtime: Path, command: list[str]) -> list[str]:
    result = command.copy()
    try:
        result[0] = Path(command[0]).relative_to(runtime).as_posix()
    except ValueError:
        # External system executables remain absolute; only runtime-owned paths relocate.
        pass
    return result


def runtime_ownership_state(
    runtime: Path,
    receipt: dict[str, object],
    specification: dict[str, object],
) -> str:
    """Classify authenticated runtime content without following foreign links."""
    verify_receipt_integrity(receipt)
    ownership = normalized_ownership_record(receipt.get("ownership"))
    if not isinstance(ownership, dict):
        raise ValueError("dependency runtime ownership record is invalid")
    metadata_receipt = {
        key: value
        for key, value in receipt.items()
        if key not in {"ownership", "receiptIntegritySha256"}
    }
    metadata = json.dumps(metadata_receipt, sort_keys=True, separators=(",", ":")).encode()
    expected_files = ownership.get("files")
    expected_directories = ownership.get("directories")
    expected_links = ownership.get("links", [])
    if (
        not isinstance(expected_files, dict)
        or not isinstance(expected_directories, list)
        or not valid_link_records(expected_links)
    ):
        raise ValueError("dependency runtime ownership record is invalid")
    expected_link_map = {item["path"]: item["target"] for item in expected_links}
    actual = ownership_record(runtime, expected_links=expected_link_map)
    actual["metadataSha256"] = hashlib.sha256(metadata).hexdigest()
    if actual == ownership:
        specification_changed = receipt.get("specificationSha256") != specification_digest(
            specification
        )
        capability_changed = (
            "capabilityPolicySha256" in receipt
            and receipt.get("capabilityPolicySha256")
            != capability_policy_digest(specification)
        )
        contract_changed = receipt.get("runtimeContractVersion") != RUNTIME_CONTRACT_VERSION
        return (
            "specification-stale"
            if specification_changed or capability_changed or contract_changed
            else "healthy"
        )
    actual_files = actual["files"]
    actual_directories = actual["directories"]
    actual_links = actual["links"]
    actual_link_map = {item["path"]: item["target"] for item in actual_links}
    if (
        not set(actual_files) <= set(expected_files)
        or not set(actual_directories) <= set(expected_directories)
        or not set(actual_link_map) <= set(expected_link_map)
    ):
        raise ValueError("dependency runtime contains foreign content")
    if (
        any(expected_files[path] != digest for path, digest in actual_files.items())
        or any(expected_link_map[path] != target for path, target in actual_link_map.items())
        or actual.get("metadataSha256") != ownership.get("metadataSha256")
    ):
        raise ValueError("dependency runtime ownership drift detected")
    return "missing-only"


def repair(  # noqa: MC0001 - one locked transaction keeps recovery and publication atomic.
    runtime: Path,
    specification: dict[str, object],
    runner=run_command,
    now: datetime | None = None,
    force: bool = False,
) -> dict[str, object]:
    runtime = runtime.absolute()
    if is_link_or_reparse(runtime):
        raise ValueError(f"dependency runtime is a link or reparse point: {runtime}")
    runtime.parent.mkdir(parents=True, exist_ok=True)
    with runtime_lock(runtime):
        backup = runtime.with_name(f"{runtime.name}.backup")
        removing = runtime.with_name(f"{runtime.name}.removing")
        building = runtime.with_name(f"{runtime.name}.building")
        building_owned = building.with_name(f"{building.name}{BUILD_MARKER_OWNED_SUFFIX}")
        if any(
            is_link_or_reparse(path)
            for path in (backup, removing, building, building_owned)
        ):
            raise ValueError("dependency recovery path is a link or reparse point")
        if building_owned.exists():
            if building.exists():
                raise ValueError("dependency build marker recovery collision")
            with building_owned.open("r", encoding="utf-8") as marker:
                if marker.read() != BUILD_MARKER_MAGIC:
                    raise ValueError(
                        f"dependency build marker scratch collision: {building_owned}"
                    )
                opened = os.fstat(marker.fileno())
                named = os.stat(building_owned, follow_symlinks=False)
                owned_identity = (opened.st_dev, opened.st_ino)
                if owned_identity != (named.st_dev, named.st_ino):
                    raise ValueError(
                        "dependency build marker scratch ownership drift detected"
                    )
            building_owned.replace(building)
            named = os.stat(building, follow_symlinks=False)
            if (named.st_dev, named.st_ino) != owned_identity:
                building.replace(building_owned)
                raise ValueError("dependency build marker scratch ownership drift detected")
        if removing.exists():
            if runtime.exists():
                remove(runtime, specification, removal_path=removing, already_locked=True)
            else:
                raise ValueError("dependency removal recovery is required before repair")
        if building.exists():
            if building.read_text(encoding="utf-8") != BUILD_MARKER_MAGIC:
                raise ValueError(f"dependency build marker collision: {building}")
            if runtime.exists() and not backup.exists():
                try:
                    verify_receipt(runtime, read_receipt(runtime), specification)
                except (OSError, ValueError):
                    shutil.rmtree(runtime)
                else:
                    building.unlink()
                    return read_receipt(runtime)
            elif runtime.exists():
                shutil.rmtree(runtime)
            if backup.exists():
                verify_receipt(backup, read_receipt(backup))
                backup.replace(runtime)
            building.unlink()
        if backup.exists() and not runtime.exists():
            verify_receipt(backup, read_receipt(backup))
            backup.replace(runtime)
        elif backup.exists():
            verify_receipt(runtime, read_receipt(runtime))
            backup.replace(removing)
            remove(runtime, specification, removal_path=removing, already_locked=True)
        previous = runtime.exists()
        if previous:
            current = read_receipt(runtime)
            state = runtime_ownership_state(runtime, current, specification)
            if state == "healthy" and not force:
                doctor(runtime, runner=runner, now=now, specification=specification)
                return upgrade_capability_receipt(runtime, current, specification)
        marker_created = False
        marker_identity: tuple[int, int] | None = None
        try:
            if previous:
                runtime.replace(backup)
            descriptor = os.open(
                building,
                os.O_WRONLY | os.O_CREAT | os.O_EXCL | getattr(os, "O_BINARY", 0),
                0o600,
            )
            marker_created = True
            with os.fdopen(descriptor, "w", encoding="utf-8") as marker:
                marker.write(BUILD_MARKER_MAGIC)
                marker.flush()
                os.fsync(marker.fileno())
                opened = os.fstat(marker.fileno())
                marker_identity = (opened.st_dev, opened.st_ino)
            runtime.mkdir()
            receipt = execute_plan(runtime, specification, runner=runner, now=now)
        except BaseException:
            if runtime.exists():
                shutil.rmtree(runtime, ignore_errors=True)
            if previous and backup.exists() and not runtime.exists():
                backup.replace(runtime)
            if marker_created and building.exists() and not backup.exists():
                unlink_owned_marker(building, marker_identity)  # type: ignore[arg-type]
            raise
        else:
            unlink_owned_marker(building, marker_identity)  # type: ignore[arg-type]
            if backup.exists():
                backup.replace(removing)
                try:
                    remove(runtime, specification, removal_path=removing, already_locked=True)
                except OSError:
                    # The verified tombstone is durable recovery state for the next call.
                    pass
            return receipt


def read_receipt(runtime: Path) -> dict[str, object]:
    try:
        receipt = json.loads((runtime / RECEIPT_NAME).read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError) as error:
        raise ValueError("ChaosEngine dependency receipt is missing or invalid") from error
    required = {"schemaVersion", "checkedAt", "specificationSha256", "environment", "installed", "tools", "ownership", "receiptIntegritySha256"}
    if (
        not isinstance(receipt, dict)
        or receipt.get("schemaVersion") != RECEIPT_SCHEMA
        or not required <= set(receipt)
        or set(receipt.get("tools", {})) != {"uv", "mempalace", "graphify", "memory"}
    ):
        raise ValueError("ChaosEngine dependency receipt schema is unsupported")
    return receipt


def verify_receipt(
    runtime: Path,
    receipt: dict[str, object],
    specification: dict[str, object] | None = None,
) -> None:
    verify_receipt_integrity(receipt)
    if specification is not None and receipt["specificationSha256"] != specification_digest(specification):
        raise ValueError("dependency runtime specification drift detected")
    if (
        specification is not None
        and receipt.get("runtimeContractVersion") != RUNTIME_CONTRACT_VERSION
    ):
        raise ValueError("dependency runtime contract drift detected")
    if (
        specification is not None
        and "capabilityPolicySha256" in receipt
        and receipt["capabilityPolicySha256"] != capability_policy_digest(specification)
    ):
        raise ValueError("dependency capability policy drift detected")
    ownership = normalized_ownership_record(receipt["ownership"])
    metadata_receipt = {
        key: value
        for key, value in receipt.items()
        if key not in {"ownership", "receiptIntegritySha256"}
    }
    metadata = json.dumps(metadata_receipt, sort_keys=True, separators=(",", ":")).encode()
    expected = ownership_record(runtime)
    expected["metadataSha256"] = hashlib.sha256(metadata).hexdigest()
    if not isinstance(ownership, dict) or ownership != expected:
        raise ValueError("dependency runtime ownership drift detected")
    tools = receipt["tools"]
    if not isinstance(tools, dict):
        raise ValueError("dependency tool receipt is invalid")
    for record in tools.values():
        if not isinstance(record, dict) or not {"requested", "commands", "entrypoints", "resolved"} <= set(record):
            raise ValueError("dependency tool receipt is invalid")
        entrypoints = record["entrypoints"]
        if not isinstance(entrypoints, dict):
            raise ValueError("dependency entrypoint receipt is invalid")
        for path, expected_digest in entrypoints.items():
            candidate = runtime / path if isinstance(path, str) else runtime
            if not isinstance(path, str) or not isinstance(expected_digest, str) or sha256(candidate) != expected_digest:
                raise ValueError("dependency entrypoint drift detected")


def verify_receipt_integrity(receipt: dict[str, object]) -> None:
    integrity_receipt = {key: value for key, value in receipt.items() if key != "receiptIntegritySha256"}
    integrity = json.dumps(integrity_receipt, sort_keys=True, separators=(",", ":")).encode()
    if receipt["receiptIntegritySha256"] != hashlib.sha256(integrity).hexdigest():
        raise ValueError("dependency receipt integrity drift detected")


def upgrade_capability_receipt(
    runtime: Path, receipt: dict[str, object], specification: dict[str, object]
) -> dict[str, object]:
    if "capabilityPolicySha256" in receipt:
        return receipt
    upgraded = dict(receipt)
    upgraded["capabilityPolicySha256"] = capability_policy_digest(specification)
    ownership = dict(upgraded["ownership"])  # type: ignore[arg-type]
    upgraded["ownership"] = ownership
    metadata_receipt = {
        key: value
        for key, value in upgraded.items()
        if key not in {"ownership", "receiptIntegritySha256"}
    }
    metadata = json.dumps(metadata_receipt, sort_keys=True, separators=(",", ":")).encode()
    ownership["metadataSha256"] = hashlib.sha256(metadata).hexdigest()
    integrity_receipt = {
        key: value for key, value in upgraded.items() if key != "receiptIntegritySha256"
    }
    integrity = json.dumps(integrity_receipt, sort_keys=True, separators=(",", ":")).encode()
    upgraded["receiptIntegritySha256"] = hashlib.sha256(integrity).hexdigest()
    receipt_path = runtime / RECEIPT_NAME
    temporary = runtime / f".{RECEIPT_NAME}.capability-upgrade"
    if temporary.exists() or is_link_or_reparse(temporary):
        raise ValueError("dependency capability receipt upgrade collision")
    descriptor = os.open(
        temporary,
        os.O_WRONLY | os.O_CREAT | os.O_EXCL | getattr(os, "O_BINARY", 0),
        0o600,
    )
    try:
        with os.fdopen(descriptor, "w", encoding="utf-8") as stream:
            json.dump(upgraded, stream, indent=2, sort_keys=True)
            stream.write("\n")
            stream.flush()
            os.fsync(stream.fileno())
        temporary.replace(receipt_path)
    finally:
        if temporary.exists() and not is_link_or_reparse(temporary):
            temporary.unlink()
    verify_receipt(runtime, upgraded, specification)
    return upgraded


def doctor(
    runtime: Path,
    runner=run_command,
    now: datetime | None = None,
    specification: dict[str, object] | None = None,
) -> dict[str, object]:
    receipt = read_receipt(runtime)
    verify_receipt(runtime, receipt, specification)
    environment = tool_environment(runtime)
    for commands in probe_plan(runtime).values():
        for command in commands:
            runner(command, environment)
    return {
        "status": "healthy",
        "freshness": freshness(receipt, now),
        "checkedAt": receipt["checkedAt"],
    }


def status(
    runtime: Path,
    specification: dict[str, object],
    now: datetime | None = None,
) -> dict[str, object]:
    receipt = read_receipt(runtime)
    verify_receipt(runtime, receipt, specification)
    return {
        "status": "healthy",
        "freshness": freshness(receipt, now),
        "checkedAt": receipt["checkedAt"],
    }


def remove(
    runtime: Path,
    specification: dict[str, object],
    removal_path: Path | None = None,
    already_locked: bool = False,
) -> None:
    runtime = runtime.absolute()
    removing = removal_path or runtime.with_name(f"{runtime.name}.removing")
    with (nullcontext() if already_locked else runtime_lock(runtime)):
        if is_link_or_reparse(runtime) or is_link_or_reparse(removing):
            raise ValueError("dependency removal path is a link or reparse point")
        if removal_path is None and runtime.exists() and removing.exists():
            raise ValueError(f"dependency removal collision: {removing}")
        if removal_path is None and runtime.exists():
            receipt = read_receipt(runtime)
            verify_receipt(runtime, receipt, specification)
            runtime.replace(removing)
        elif not removing.exists():
            return
        if not any(removing.iterdir()):
            removing.rmdir()
            return
        receipt = read_receipt(removing)
        if removal_path is not None or not runtime.exists():
            verify_receipt_integrity(receipt)
        else:
            verify_receipt(removing, receipt, specification)
        ownership = normalized_ownership_record(receipt["ownership"])
        files = ownership.get("files") if isinstance(ownership, dict) else None
        directories = ownership.get("directories") if isinstance(ownership, dict) else None
        links = ownership.get("links", []) if isinstance(ownership, dict) else None
        if (
            not isinstance(files, dict)
            or not isinstance(directories, list)
            or not valid_link_records(links)
        ):
            raise ValueError("dependency removal ownership record is invalid")
        expected_links = {item["path"]: item["target"] for item in links}
        entries = runtime_entries(removing)
        present_links = {
            path.relative_to(removing).as_posix(): os.readlink(path)
            for path in entries
            if path.is_symlink()
        }
        if not set(present_links) <= set(expected_links) or any(
            expected_links[path] != target for path, target in present_links.items()
        ):
            raise ValueError("dependency removal link ownership drift detected")
        present = {
            path.relative_to(removing).as_posix()
            for path in entries
            if not is_link_or_reparse(path) and path.is_file()
        }
        generated_files = {
            relative for relative in present if is_generated_python_cache(relative)
        }
        allowed = set(files) | generated_files | {RECEIPT_NAME}
        if not present <= allowed:
            raise ValueError("dependency removal contains an unowned file")
        for relative in sorted(present - {RECEIPT_NAME}):
            path = removing / relative
            if relative not in generated_files and sha256(path) != files[relative]:
                raise ValueError("dependency removal ownership drift detected")
            path.unlink()
        for relative in sorted(present_links):
            (removing / relative).unlink()
        present_directories = {
            path.relative_to(removing).as_posix()
            for path in entries
            if not is_link_or_reparse(path) and path.is_dir()
        }
        expected_directories = set(directories) | {
            relative
            for relative in present_directories
            if is_generated_python_cache(relative, directory=True)
        }
        if not present_directories <= expected_directories:
            raise ValueError("dependency removal directory ownership drift detected")
        for directory in sorted(
            (
                path
                for path in entries
                if not is_link_or_reparse(path) and path.is_dir()
            ),
            key=lambda path: len(path.parts),
            reverse=True,
        ):
            directory.rmdir()
        (removing / RECEIPT_NAME).unlink()
        removing.rmdir()


def prepare_remove(runtime: Path, specification: dict[str, object]) -> Path:
    runtime = runtime.absolute()
    removing = runtime.with_name(f"{runtime.name}.removing")
    with runtime_lock(runtime):
        if is_link_or_reparse(runtime) or is_link_or_reparse(removing):
            raise ValueError("dependency removal path is a link or reparse point")
        if removing.exists():
            raise ValueError(f"dependency removal collision: {removing}")
        verify_receipt(runtime, read_receipt(runtime), specification)
        runtime.replace(removing)
    return removing


def cancel_remove(runtime: Path) -> None:
    runtime = runtime.absolute()
    removing = runtime.with_name(f"{runtime.name}.removing")
    with runtime_lock(runtime):
        if runtime.exists() or not removing.exists():
            raise ValueError("dependency removal cannot be cancelled")
        verify_receipt(removing, read_receipt(removing))
        removing.replace(runtime)


def finalize_remove(runtime: Path, specification: dict[str, object]) -> None:
    removing = runtime.absolute().with_name(f"{runtime.name}.removing")
    remove(runtime, specification, removal_path=removing)


def parser() -> argparse.ArgumentParser:
    result = argparse.ArgumentParser(description=__doc__)
    result.add_argument("command", choices=("doctor", "repair", "upgrade"))
    result.add_argument("--runtime", required=True, type=Path)
    result.add_argument(
        "--specification", type=Path, default=Path(__file__).with_name("dependencies.json")
    )
    return result


def main() -> int:
    args = parser().parse_args()
    try:
        if args.command == "doctor":
            with runtime_lock(args.runtime.absolute()):
                result = doctor(
                    args.runtime,
                    specification=load_specification(args.specification),
                )
        else:
            result = repair(
                args.runtime,
                load_specification(args.specification),
                force=args.command == "upgrade",
            )
    except (OSError, RuntimeError, ValueError, subprocess.SubprocessError) as error:
        print(str(error), file=sys.stderr)
        return 1
    print(json.dumps(result, sort_keys=True))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
