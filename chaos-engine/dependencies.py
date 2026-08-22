#!/usr/bin/env python3
"""Plan and verify ChaosEngine-owned runtime dependencies."""

from __future__ import annotations

import argparse
from contextlib import ExitStack, contextmanager, nullcontext
import ctypes.wintypes
import errno
import hashlib
import json
import os
import re
import secrets
import shutil
import stat
import subprocess  # nosec B404 - fixed list-form dependency commands from tracked spec.
import sys
import time
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
POINTER_REMOVING_NAME = f"{POINTER_NAME}.removing"
POINTER_SCHEMA = 1
GENERATIONS_NAME = ".chaos-engine-runtime-generations"
TRANSACTIONS_NAME = ".chaos-engine-runtime-transactions"
MAX_CONTROL_BYTES = 4 * 1024 * 1024
# Pinned uv 0.11.29 + Graphify/MemPalace ownership is ~6.74 MiB compact.
MAX_RECEIPT_BYTES = 8 * 1024 * 1024
MAX_EXECUTABLE_BYTES = 256 * 1024 * 1024
HEX_ID = re.compile(r"[0-9a-f]{32}")
HEX_DIGEST = re.compile(r"[0-9a-f]{64}")
REQUIRED_DISPATCHES = {
    "uv",
    "mempalace",
    "mempalace-mcp",
    "graphify",
    "memory",
    "memory-mcp",
}
WINDOWS_UV_JUNCTION_TAG = 0xA0000003
WINDOWS_UV_ALIAS = re.compile(
    r"uv-python/cpython-3\.10-windows-(?P<arch>x86_64|aarch64)-none"
)
CANDIDATE_TRUST_BOUNDARY = (
    "A same-user trusted subprocess has ambient write authority and this stdlib "
    "controller cannot sandbox it. Held handles and no-follow operations contain "
    "concurrent path substitution by other actors; command output remains untrusted."
)
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
    except (Exception, KeyboardInterrupt, SystemExit):
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
    except (Exception, KeyboardInterrupt, SystemExit):
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
    *,
    transaction_id: str | None = None,
    expected_specification_sha256: str,
    expected_core_sha256: str,
) -> dict[str, object]:
    """Atomically select immutable generations using identifiers, never paths."""
    project = project.absolute()
    active = _validate_generation_record(active)
    if (
        active["specificationSha256"] != expected_specification_sha256
        or active["coreSha256"] != expected_core_sha256
    ):
        raise ValueError("dependency candidate does not match requested specification or core")
    _validate_selected_generation(
        project,
        active,
        expected_specification_sha256,
        expected_core_sha256,
        verify_installed_core=True,
    )
    previous = None
    pointer_path = project / POINTER_NAME
    if pointer_path.exists() or is_link_or_reparse(pointer_path):
        current = _read_pointer(project)
        current_active = _validate_generation_record(current["active"])
        current_valid = True
        try:
            _validate_selected_generation(
                project,
                current_active,
                current_active["specificationSha256"],
                current_active["coreSha256"],
                verify_installed_core=False,
            )
        except (OSError, ValueError):
            current_valid = False
        if current_valid and current_active != active:
            previous = current_active
        elif current.get("previous") is not None:
            tracked_previous = _validate_generation_record(current["previous"])
            try:
                _validate_selected_generation(
                    project,
                    tracked_previous,
                    tracked_previous["specificationSha256"],
                    tracked_previous["coreSha256"],
                    verify_installed_core=False,
                )
            except (OSError, ValueError):
                previous = None
            else:
                previous = tracked_previous
    transaction = transaction_id or secrets.token_hex(16)
    if HEX_ID.fullmatch(transaction) is None:
        raise ValueError("dependency transaction identifier is invalid")
    persisted_pointer: dict[str, object] = {
        "schemaVersion": POINTER_SCHEMA,
        "transactionId": transaction,
        "active": active,
        "previous": previous,
    }
    persisted_pointer["integritySha256"] = json_integrity(persisted_pointer)
    path = project / POINTER_NAME
    temporary = project / f"{POINTER_NAME}.tmp.{transaction}.{secrets.token_hex(8)}"
    if is_link_or_reparse(path):
        raise ValueError("dependency pointer is a link or reparse point")
    directory = None
    if os.name != "nt":
        directory = os.open(project, os.O_RDONLY | getattr(os, "O_DIRECTORY", 0))
    descriptor = os.open(
        temporary,
        os.O_WRONLY | os.O_CREAT | os.O_EXCL | getattr(os, "O_BINARY", 0),
        0o600,
    )
    try:
        with os.fdopen(descriptor, "w", encoding="utf-8") as stream:
            json.dump(persisted_pointer, stream, indent=2, sort_keys=True)
            stream.write("\n")
            stream.flush()
            os.fsync(stream.fileno())
        temporary.replace(path)
        if directory is not None:
            try:
                os.fsync(directory)
            except OSError:
                # Replacement is commit point; a durability warning cannot be rolled back.
                persisted, _ = _bounded_json(project, POINTER_NAME, "pointer")
                if persisted != persisted_pointer:
                    raise
                result = dict(persisted_pointer)
                result["publicationStatus"] = "committed-not-durable"
                return result
    finally:
        if temporary.exists() and not is_link_or_reparse(temporary):
            temporary.unlink()
        if directory is not None:
            os.close(directory)
    result = dict(persisted_pointer)
    result["publicationStatus"] = "durable"
    return result


def _relative_parts(relative: str, label: str) -> tuple[str, ...]:
    path = Path(relative)
    if (
        path.is_absolute()
        or path.anchor
        or path.drive
        or path.root
        or not path.parts
        or ".." in path.parts
    ):
        raise ValueError(f"dependency {label} path is unsafe")
    return path.parts


def _trusted_root(root: Path, label: str) -> Path:
    lexical = _lexical_path(root)
    try:
        resolved = root.resolve(strict=True)
    except OSError as error:
        raise ValueError(f"dependency {label} root is missing or unsafe") from error
    if is_link_or_reparse(lexical) or os.path.normcase(str(lexical)) != os.path.normcase(
        str(resolved)
    ):
        raise ValueError(f"dependency {label} root has an unsafe ancestor or link")
    return lexical


@contextmanager
def _hold_directory(path: Path, label: str):
    """Hold directory identity; Windows handle denies rename/delete while in use."""
    path = _trusted_root(path, label)
    if os.name == "nt":
        class FILE_ATTRIBUTE_TAG_INFO(ctypes.Structure):
            _fields_ = [
                ("FileAttributes", ctypes.wintypes.DWORD),
                ("ReparseTag", ctypes.wintypes.DWORD),
            ]

        kernel32 = ctypes.WinDLL("kernel32", use_last_error=True)
        create_file = kernel32.CreateFileW
        create_file.argtypes = [
            ctypes.wintypes.LPCWSTR,
            ctypes.wintypes.DWORD,
            ctypes.wintypes.DWORD,
            ctypes.wintypes.LPVOID,
            ctypes.wintypes.DWORD,
            ctypes.wintypes.DWORD,
            ctypes.wintypes.HANDLE,
        ]
        create_file.restype = ctypes.wintypes.HANDLE
        handle = create_file(
            str(path),
            0x80000000,
            0x00000001 | 0x00000002,
            None,
            3,
            0x00200000 | 0x02000000,
            None,
        )
        if handle == ctypes.c_void_p(-1).value:
            raise ValueError(f"dependency {label} directory cannot be held safely")
        try:
            attributes = FILE_ATTRIBUTE_TAG_INFO()
            if not kernel32.GetFileInformationByHandleEx(
                handle, 9, ctypes.byref(attributes), ctypes.sizeof(attributes)
            ) or attributes.FileAttributes & 0x400:
                raise ValueError(f"dependency {label} directory is a reparse point")
            value = os.stat(path, follow_symlinks=False)
            yield (None, (value.st_dev, value.st_ino))
        finally:
            kernel32.CloseHandle(handle)
        return
    descriptor = os.open(
        path,
        os.O_RDONLY | getattr(os, "O_DIRECTORY", 0) | getattr(os, "O_NOFOLLOW", 0),
    )
    try:
        value = os.fstat(descriptor)
        yield (descriptor, (value.st_dev, value.st_ino))
    finally:
        os.close(descriptor)


def _assert_held_directory(path: Path, held: tuple[object, tuple[int, int]], label: str) -> None:
    if is_link_or_reparse(path):
        raise ValueError(f"dependency {label} directory became unsafe")
    try:
        value = os.stat(path, follow_symlinks=False)
    except OSError as error:
        raise ValueError(f"dependency {label} directory identity changed") from error
    if (value.st_dev, value.st_ino) != held[1]:
        raise ValueError(f"dependency {label} directory identity changed")


def _delete_fd_contents(directory: int) -> None:
    for name in os.listdir(directory):
        value = os.stat(name, dir_fd=directory, follow_symlinks=False)
        if stat.S_ISDIR(value.st_mode):
            child = os.open(
                name,
                os.O_RDONLY
                | getattr(os, "O_DIRECTORY", 0)
                | getattr(os, "O_NOFOLLOW", 0),
                dir_fd=directory,
            )
            try:
                opened = os.fstat(child)
                if (opened.st_dev, opened.st_ino) != (value.st_dev, value.st_ino):
                    raise ValueError("dependency cleanup directory identity changed")
                _delete_fd_contents(child)
                after = os.fstat(child)
                if (after.st_dev, after.st_ino) != (opened.st_dev, opened.st_ino):
                    raise ValueError("dependency cleanup directory identity changed")
            finally:
                os.close(child)
            os.rmdir(name, dir_fd=directory)
        else:
            os.unlink(name, dir_fd=directory)


def _delete_held_child(
    parent: tuple[object, tuple[int, int]],
    name: str,
    child: tuple[object, tuple[int, int]],
    expected: tuple[int, int],
) -> None:
    parent_fd, child_identity = parent[0], child[1]
    child_fd = child[0]
    if not isinstance(parent_fd, int) or not isinstance(child_fd, int):
        raise ValueError("descriptor-relative cleanup is unavailable")
    named = os.stat(name, dir_fd=parent_fd, follow_symlinks=False)
    opened = os.fstat(child_fd)
    identities = {
        (named.st_dev, named.st_ino),
        (opened.st_dev, opened.st_ino),
        child_identity,
    }
    if identities != {expected}:
        raise ValueError("dependency cleanup identity changed; quarantine retained")
    _delete_fd_contents(child_fd)
    os.rmdir(name, dir_fd=parent_fd)


def _delete_windows_tree(
    path: Path, expected: tuple[int, int], runtime: Path | None = None
) -> None:
    runtime = runtime or path
    if is_link_or_reparse(path):
        raise ValueError("dependency cleanup path is unsafe; quarantine retained")
    value = os.stat(path, follow_symlinks=False)
    if (value.st_dev, value.st_ino) != expected:
        raise ValueError("dependency cleanup identity changed; quarantine retained")
    children = sorted(path.iterdir(), reverse=True)
    for child in children:
        if is_link_or_reparse(child):
            if child.is_symlink():
                child.unlink()
                continue
            _windows_uv_junction_record(runtime, child)
            child.rmdir()
    for child in children:
        if not child.exists() and not is_link_or_reparse(child):
            continue
        if child.is_dir():
            state = os.stat(child, follow_symlinks=False)
            _delete_windows_tree(child, (state.st_dev, state.st_ino), runtime)
        else:
            child.unlink()
    path.rmdir()


def _open_regular_relative(root: Path, relative: str, label: str) -> int:
    """Open a regular descendant without following POSIX links in any component."""
    root = _trusted_root(root, label)
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
                previous = directory
                directory = child
                os.close(previous)
            descriptor = os.open(
                parts[-1], os.O_RDONLY | binary | nofollow, dir_fd=directory
            )
        except OSError as error:
            raise ValueError(f"dependency {label} has an unsafe ancestor or link") from error
        finally:
            os.close(directory)
    elif os.name == "nt":
        current = root
        for part in parts:
            current /= part
            if is_link_or_reparse(current):
                raise ValueError(f"dependency {label} has an unsafe ancestor or link")
        descriptor = _open_windows_regular(root, current, label)
    else:
        raise ValueError("safe no-follow file traversal is unavailable on this platform")
    try:
        opened = os.fstat(descriptor)
        if not stat.S_ISREG(opened.st_mode):
            raise ValueError(f"dependency {label} is not a regular file")
    except (Exception, KeyboardInterrupt, SystemExit):
        os.close(descriptor)
        raise
    return descriptor


def _open_windows_regular(root: Path, path: Path, label: str) -> int:
    """Open final Windows file without following it; validate kernel-resolved location."""
    import msvcrt

    class FILE_ATTRIBUTE_TAG_INFO(ctypes.Structure):
        _fields_ = [
            ("FileAttributes", ctypes.wintypes.DWORD),
            ("ReparseTag", ctypes.wintypes.DWORD),
        ]

    kernel32 = ctypes.WinDLL("kernel32", use_last_error=True)
    create_file = kernel32.CreateFileW
    create_file.argtypes = [
        ctypes.wintypes.LPCWSTR,
        ctypes.wintypes.DWORD,
        ctypes.wintypes.DWORD,
        ctypes.wintypes.LPVOID,
        ctypes.wintypes.DWORD,
        ctypes.wintypes.DWORD,
        ctypes.wintypes.HANDLE,
    ]
    create_file.restype = ctypes.wintypes.HANDLE
    invalid = ctypes.c_void_p(-1).value
    handle = invalid
    error_number = 0
    for delay in (0.0, 0.02, 0.05, 0.1, 0.2):
        if delay:
            time.sleep(delay)
        handle = create_file(
            str(path),
            0x80000000,
            0x00000001 | 0x00000002 | 0x00000004,
            None,
            3,
            0x00200000 | 0x08000000,
            None,
        )
        if handle != invalid:
            break
        error_number = ctypes.get_last_error()
        if error_number not in {5, 32, 33}:
            break
    if handle == invalid:
        raise ValueError(
            f"dependency {label} is missing or unsafe (Windows error {error_number})"
        ) from OSError(error_number, ctypes.FormatError(error_number))
    try:
        attributes = FILE_ATTRIBUTE_TAG_INFO()
        if not kernel32.GetFileInformationByHandleEx(
            handle, 9, ctypes.byref(attributes), ctypes.sizeof(attributes)
        ):
            raise ValueError(f"dependency {label} identity cannot be verified")
        if attributes.FileAttributes & 0x400:
            raise ValueError(f"dependency {label} is a link or reparse point")
        size = kernel32.GetFinalPathNameByHandleW(handle, None, 0, 0)
        if not size:
            raise ValueError(f"dependency {label} final path cannot be verified")
        buffer = ctypes.create_unicode_buffer(size + 1)
        if not kernel32.GetFinalPathNameByHandleW(handle, buffer, len(buffer), 0):
            raise ValueError(f"dependency {label} final path cannot be verified")
        final_path = _lexical_path(Path(buffer.value))
        if not final_path.is_relative_to(root):
            raise ValueError(f"dependency {label} escaped its trusted root")
        named = os.stat(path, follow_symlinks=False)
        descriptor = msvcrt.open_osfhandle(handle, os.O_RDONLY | getattr(os, "O_BINARY", 0))
        handle = None
        opened = os.fstat(descriptor)
        if (opened.st_dev, opened.st_ino) != (named.st_dev, named.st_ino):
            os.close(descriptor)
            raise ValueError(f"dependency {label} identity changed during open")
        return descriptor
    finally:
        if handle is not None:
            kernel32.CloseHandle(handle)


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


def _capture_regular_relative(
    root: Path,
    relative: str,
    label: str,
    expected_size: int | None = None,
) -> tuple[str, dict[str, int]]:
    if expected_size is not None and (
        not isinstance(expected_size, int) or not 0 <= expected_size <= MAX_EXECUTABLE_BYTES
    ):
        raise ValueError(f"dependency {label} size is invalid")
    descriptor = _open_regular_relative(root, relative, label)
    digest = hashlib.sha256()
    try:
        before = os.fstat(descriptor)
        if expected_size is not None and before.st_size != expected_size:
            raise ValueError(f"dependency {label} size drift detected")
        while chunk := os.read(descriptor, 1024 * 1024):
            digest.update(chunk)
        after = os.fstat(descriptor)
        identity = lambda item: (
            item.st_dev,
            item.st_ino,
            item.st_size,
            item.st_mtime_ns,
            item.st_ctime_ns,
            item.st_mode,
        )
        if identity(before) != identity(after):
            raise ValueError(f"dependency {label} changed while hashing")
    finally:
        os.close(descriptor)
    return digest.hexdigest(), _identity_from_stat(after)


def _digest_regular_relative(
    root: Path, relative: str, label: str, expected_size: int
) -> str:
    return _capture_regular_relative(root, relative, label, expected_size)[0]


def _bounded_json(
    root: Path,
    relative: str,
    label: str,
    limit: int = MAX_CONTROL_BYTES,
) -> tuple[dict[str, object], bytes]:
    try:
        data = _read_regular_relative(root, relative, label, limit)
        value = json.loads(data)
    except (OSError, UnicodeDecodeError, json.JSONDecodeError) as error:
        raise ValueError(f"dependency {label} is missing or invalid") from error
    if not isinstance(value, dict):
        raise ValueError(f"dependency {label} is invalid")
    return value, data


def _validate_generation_receipt(receipt: dict[str, object]) -> None:
    required = {
        "schemaVersion",
        "runtimeContractVersion",
        "checkedAt",
        "specificationSha256",
        "coreSha256",
        "environment",
        "installed",
        "tools",
        "ownership",
        "receiptIntegritySha256",
    }
    if (
        receipt.get("schemaVersion") != 2
        or receipt.get("runtimeContractVersion") != 3
        or set(receipt) != required
        or HEX_DIGEST.fullmatch(str(receipt.get("specificationSha256", ""))) is None
        or HEX_DIGEST.fullmatch(str(receipt.get("coreSha256", ""))) is None
        or not isinstance(receipt.get("environment"), dict)
        or not isinstance(receipt.get("installed"), dict)
        or not isinstance(receipt.get("ownership"), dict)
    ):
        raise ValueError("dependency generation receipt schema is invalid")
    ownership = receipt["ownership"]
    if (
        set(ownership) != {"directories", "files", "links", "sha256", "identities"}
        or not isinstance(ownership.get("directories"), list)
        or not isinstance(ownership.get("files"), dict)
        or not isinstance(ownership.get("links"), list)
        or not isinstance(ownership.get("identities"), dict)
        or HEX_DIGEST.fullmatch(str(ownership.get("sha256", ""))) is None
    ):
        raise ValueError("dependency generation receipt ownership is invalid")
    try:
        checked = datetime.fromisoformat(str(receipt["checkedAt"]))
    except ValueError as error:
        raise ValueError("dependency generation receipt timestamp is invalid") from error
    if checked.tzinfo is None:
        raise ValueError("dependency generation receipt timestamp is invalid")
    tools = receipt.get("tools")
    if not isinstance(tools, dict) or set(tools) != REQUIRED_DISPATCHES:
        raise ValueError("dependency generation tool metadata is invalid")
    for name, record in tools.items():
        dispatch = record.get("dispatch") if isinstance(record, dict) else None
        _validate_dispatch_metadata(name, dispatch)


def _validate_dispatch_metadata(name: str, value: object) -> dict[str, object]:
    if not isinstance(value, dict):
        raise ValueError(f"dependency generation tool metadata is invalid: {name}")
    scripts = "Scripts" if os.name == "nt" else "bin"
    python_name = "python.exe" if os.name == "nt" else "python"
    uv_name = "uv.exe" if os.name == "nt" else "uv"
    if name == "uv":
        expected = {
            "kind": "executable",
            "path": f"bootstrap/{scripts}/{uv_name}",
        }
        fields = {**expected, "sha256": value.get("sha256"), "size": value.get("size")}
        if set(value) != set(fields) or any(value.get(key) != item for key, item in expected.items()):
            raise ValueError("dependency generation tool metadata is invalid: uv")
        digest, size = value.get("sha256"), value.get("size")
    elif name in {"mempalace", "mempalace-mcp", "graphify"}:
        environment = "graphifyy" if name == "graphify" else "mempalace"
        expected = {
            "kind": "python",
            "interpreter": f"uv-tools/{environment}/{scripts}/{python_name}",
            "distribution": environment,
            "entrypoint": name,
        }
        required = set(expected) | {"interpreterSha256", "interpreterSize"}
        linked = {"interpreterLinkTarget", "interpreterTarget"}
        if set(value) not in {frozenset(required), frozenset(required | linked)} or any(
            value.get(key) != item for key, item in expected.items()
        ):
            raise ValueError(f"dependency generation tool metadata is invalid: {name}")
        if linked <= set(value) and (
            not all(isinstance(value.get(key), str) and value.get(key) for key in linked)
            or Path(str(value["interpreterLinkTarget"])).is_absolute()
            or Path(str(value["interpreterTarget"])).is_absolute()
        ):
            raise ValueError(f"dependency generation tool metadata is invalid: {name}")
        digest, size = value.get("interpreterSha256"), value.get("interpreterSize")
    elif name in {"memory", "memory-mcp"}:
        suffix = "dist/cli/main.js" if name == "memory" else "dist/mcp/server.js"
        expected = {
            "kind": "npm",
            "script": f"npm/node_modules/@aictx/memory/{suffix}",
            "entrypoint": name,
        }
        required = set(expected) | {"scriptSha256", "scriptSize"}
        if set(value) != required or any(value.get(key) != item for key, item in expected.items()):
            raise ValueError(f"dependency generation tool metadata is invalid: {name}")
        digest, size = value.get("scriptSha256"), value.get("scriptSize")
    else:
        raise ValueError(f"dependency generation tool metadata is invalid: {name}")
    if (
        not isinstance(digest, str)
        or HEX_DIGEST.fullmatch(digest) is None
        or not isinstance(size, int)
        or not 0 < size <= MAX_EXECUTABLE_BYTES
    ):
        raise ValueError(f"dependency generation tool metadata is invalid: {name}")
    return value


def _read_pointer_at(project: Path, relative: str) -> dict[str, object]:
    pointer, _ = _bounded_json(project, relative, "pointer")
    if (
        pointer.get("schemaVersion") != POINTER_SCHEMA
        or HEX_ID.fullmatch(str(pointer.get("transactionId", ""))) is None
        or pointer.get("integritySha256") != json_integrity(pointer)
        or set(pointer) != {
            "schemaVersion",
            "transactionId",
            "active",
            "previous",
            "integritySha256",
        }
    ):
        raise ValueError("dependency pointer schema or integrity is invalid")
    _validate_generation_record(pointer.get("active"))
    if pointer.get("previous") is not None:
        _validate_generation_record(pointer["previous"])
    return pointer


def _read_pointer(project: Path) -> dict[str, object]:
    return _read_pointer_at(project, POINTER_NAME)


def _validate_selected_generation(
    project: Path,
    active: dict[str, str],
    expected_specification_sha256: str,
    expected_core_sha256: str,
    *,
    verify_installed_core: bool,
) -> Path:
    if (
        HEX_DIGEST.fullmatch(expected_specification_sha256) is None
        or HEX_DIGEST.fullmatch(expected_core_sha256) is None
        or active["specificationSha256"] != expected_specification_sha256
        or active["coreSha256"] != expected_core_sha256
    ):
        raise ValueError("dependency generation does not match tracked specification or core")
    generation = project / GENERATIONS_NAME / active["generationId"]
    receipt, receipt_bytes = _bounded_json(
        project,
        f"{GENERATIONS_NAME}/{active['generationId']}/{RECEIPT_NAME}",
        "generation receipt",
        MAX_RECEIPT_BYTES,
    )
    if hashlib.sha256(receipt_bytes).hexdigest() != active["receiptSha256"]:
        raise ValueError("dependency generation receipt digest drift detected")
    _validate_generation_receipt(receipt)
    if receipt.get("receiptIntegritySha256") != json_integrity(receipt):
        raise ValueError("dependency generation receipt integrity drift detected")
    if receipt.get("specificationSha256") != expected_specification_sha256:
        raise ValueError("dependency generation specification digest drift detected")
    if receipt.get("coreSha256") != expected_core_sha256:
        raise ValueError("dependency generation core digest drift detected")
    verify_sealed_ownership(generation, receipt["ownership"])
    if verify_installed_core:
        core = _read_regular_relative(
            project,
            ".chaos-engine/manifest.json",
            "installed core manifest",
            MAX_CONTROL_BYTES,
        )
        if hashlib.sha256(core).hexdigest() != expected_core_sha256:
            raise ValueError("dependency installed core digest drift detected")
    return generation


def active_generation(
    project: Path,
    *,
    expected_specification_sha256: str,
    expected_core_sha256: str,
) -> tuple[Path, dict[str, object]]:
    project = project.absolute()
    pointer = _read_pointer(project)
    active = _validate_generation_record(pointer.get("active"))
    generation = _validate_selected_generation(
        project,
        active,
        expected_specification_sha256,
        expected_core_sha256,
        verify_installed_core=True,
    )
    return generation, pointer


def pointer_records(project: Path) -> dict[str, object]:
    """Return authenticated active/previous records without accepting paths."""
    return _read_pointer(project.absolute())


def validated_previous(
    project: Path,
    expected_specification_sha256: str,
    expected_core_sha256: str,
    *,
    runner=None,
) -> dict[str, str]:
    """Validate and probe the compatible previous generation before rollback."""
    project = project.absolute()
    pointer = _read_pointer(project)
    previous = _validate_generation_record(pointer.get("previous"))
    generation = _validate_selected_generation(
        project,
        previous,
        expected_specification_sha256,
        expected_core_sha256,
        verify_installed_core=False,
    )
    receipt, _ = _bounded_json(
        project,
        f"{GENERATIONS_NAME}/{previous['generationId']}/{RECEIPT_NAME}",
        "generation receipt",
        MAX_RECEIPT_BYTES,
    )
    environment = os.environ.copy()
    environment["PYTHONDONTWRITEBYTECODE"] = "1"
    execute = runner or run_command
    probes = {
        "uv": ["--version"],
        "mempalace": ["--version"],
        "mempalace-mcp": ["--help"],
        "graphify": ["--version"],
        "memory": ["--help"],
        "memory-mcp": ["--help"],
    }
    for name, arguments in probes.items():
        execute(dispatch_command(generation, receipt, name, arguments), environment)
    return previous


def probe_active(
    project: Path,
    expected_specification_sha256: str,
    expected_core_sha256: str,
    *,
    runner=None,
) -> None:
    """Actively probe every exact dispatch from the selected generation."""
    project = project.absolute()
    generation, pointer = active_generation(
        project,
        expected_specification_sha256=expected_specification_sha256,
        expected_core_sha256=expected_core_sha256,
    )
    active = _validate_generation_record(pointer["active"])
    receipt, _ = _bounded_json(
        project,
        f"{GENERATIONS_NAME}/{active['generationId']}/{RECEIPT_NAME}",
        "generation receipt",
        MAX_RECEIPT_BYTES,
    )
    environment = os.environ.copy()
    environment["PYTHONDONTWRITEBYTECODE"] = "1"
    execute = runner or run_command
    for name, arguments in {
        "uv": ["--version"],
        "mempalace": ["--version"],
        "mempalace-mcp": ["--help"],
        "graphify": ["--version"],
        "memory": ["--help"],
        "memory-mcp": ["--help"],
    }.items():
        execute(dispatch_command(generation, receipt, name, arguments), environment)


def active_dispatch(project: Path, tool: str, arguments: list[str]) -> list[str]:
    """Resolve one exact dispatch from the authenticated active generation."""
    project = project.absolute()
    pointer = _read_pointer(project)
    active = _validate_generation_record(pointer.get("active"))
    generation = _validate_selected_generation(
        project,
        active,
        active["specificationSha256"],
        active["coreSha256"],
        verify_installed_core=False,
    )
    receipt, _ = _bounded_json(
        project,
        f"{GENERATIONS_NAME}/{active['generationId']}/{RECEIPT_NAME}",
        "generation receipt",
        MAX_RECEIPT_BYTES,
    )
    return dispatch_command(generation, receipt, tool, arguments)


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
    dispatch = _validate_dispatch_metadata(tool, dispatch)
    if dispatch.get("kind") == "python":
        relative = dispatch.get("interpreter")
        distribution = dispatch.get("distribution")
        entrypoint = dispatch.get("entrypoint")
        expected_digest = dispatch.get("interpreterSha256")
        expected_size = dispatch.get("interpreterSize")
        if not all(
            isinstance(item, str) and item
            for item in (relative, distribution, entrypoint, expected_digest)
        ) or HEX_DIGEST.fullmatch(str(expected_digest)) is None:
            raise ValueError(f"dependency Python dispatch is invalid: {tool}")
        interpreter = generation / str(relative)
        try:
            target = str(relative)
            if "interpreterLinkTarget" in dispatch:
                if not interpreter.is_symlink():
                    raise ValueError("recorded interpreter link is missing")
                link_target = os.readlink(interpreter)
                if link_target != dispatch["interpreterLinkTarget"]:
                    raise ValueError("recorded interpreter link target drift detected")
                resolved = _managed_link_target(generation, interpreter)
                target = resolved.relative_to(generation).as_posix()
                if target != dispatch["interpreterTarget"]:
                    raise ValueError("recorded interpreter terminal target drift detected")
            actual_digest = _digest_regular_relative(
                generation, target, f"Python interpreter for {tool}", expected_size  # type: ignore[arg-type]
            )
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
    if dispatch.get("kind") == "executable":
        relative = str(dispatch["path"])
        digest = _digest_regular_relative(
            generation,
            relative,
            f"executable for {tool}",
            dispatch["size"],  # type: ignore[arg-type]
        )
        if digest != dispatch["sha256"]:
            raise ValueError(f"dependency executable drift detected: {tool}")
        return [str(generation / relative), *arguments]
    if dispatch.get("kind") == "npm":
        relative = str(dispatch["script"])
        digest = _digest_regular_relative(
            generation,
            relative,
            f"npm script for {tool}",
            dispatch["scriptSize"],  # type: ignore[arg-type]
        )
        if digest != dispatch["scriptSha256"]:
            raise ValueError(f"dependency npm script drift detected: {tool}")
        node = shutil.which("node") or "node"
        return [node, str(generation / relative), *arguments]
    raise ValueError(f"dependency tool dispatch kind is unsupported: {tool}")


def generation_environment(generation: Path, transaction: Path) -> dict[str, str]:
    return {
        "UV_TOOL_DIR": str(generation / "uv-tools"),
        "UV_TOOL_BIN_DIR": str(transaction / "bin"),
        "UV_CACHE_DIR": str(transaction / "uv-cache"),
        "UV_PYTHON_INSTALL_DIR": str(generation / "uv-python"),
        "UV_PYTHON_BIN_DIR": str(transaction / "python-bin"),
        "UV_LINK_MODE": "copy",
        "NPM_CONFIG_PREFIX": str(generation / "npm"),
        "NPM_CONFIG_CACHE": str(transaction / "npm-cache"),
        "PIP_NO_CACHE_DIR": "1",
        "PYTHONDONTWRITEBYTECODE": "1",
    }


def generation_install_plan(
    generation: Path, specification: dict[str, object]
) -> dict[str, list[list[str]]]:
    tools = specification.get("tools")
    if specification.get("schemaVersion") != 1 or not isinstance(tools, dict):
        raise ValueError("dependency specification schema is unsupported")
    scripts = "Scripts" if os.name == "nt" else "bin"
    bootstrap = generation / "bootstrap"
    python = executable(bootstrap / scripts, "python")
    uv = executable(bootstrap / scripts, "uv")
    npm = shutil.which("npm")
    if npm is None:
        raise ValueError("Node.js npm is required to install the Memory tool")
    graphify = tools.get("graphify")
    if not isinstance(graphify, dict):
        raise ValueError("graphify dependency specification is invalid")
    return {
        "uv": [
            [sys.executable, "-m", "venv", "--copies", str(bootstrap)],
            [python, "-m", "pip", "install", "--no-cache-dir", "--upgrade", str(tools["uv"]["package"])],  # type: ignore[index]
        ],
        "mempalace": [[
            uv,
            "tool",
            "install",
            "--no-cache",
            "--managed-python",
            "--python",
            "3.10",
            "--link-mode",
            "copy",
            str(tools["mempalace"]["package"]),  # type: ignore[index]
        ]],
        "graphify": [[
            uv,
            "tool",
            "install",
            "--no-cache",
            "--managed-python",
            "--python",
            "3.10",
            "--link-mode",
            "copy",
            "--with",
            str(graphify["with"][0]),  # type: ignore[index]
            str(graphify["package"]),
        ]],
        "memory": [[
            npm,
            "install",
            "--ignore-scripts",
            "--prefix",
            str(generation / "npm"),
            str(tools["memory"]["package"]),  # type: ignore[index]
        ]],
    }


def _generation_dispatches(generation: Path) -> dict[str, dict[str, object]]:
    scripts = "Scripts" if os.name == "nt" else "bin"
    python_name = "python.exe" if os.name == "nt" else "python"
    uv_name = "uv.exe" if os.name == "nt" else "uv"

    def file_record(path: Path) -> tuple[str, int]:
        if is_link_or_reparse(path) or not path.is_file():
            raise ValueError(f"dependency entrypoint is missing or unsafe: {path}")
        return sha256(path), path.stat().st_size

    uv = generation / f"bootstrap/{scripts}/{uv_name}"
    uv_digest, uv_size = file_record(uv)
    records: dict[str, dict[str, object]] = {
        "uv": {"dispatch": {"kind": "executable", "path": uv.relative_to(generation).as_posix(), "sha256": uv_digest, "size": uv_size}}
    }
    for name, environment, distribution in (
        ("mempalace", "mempalace", "mempalace"),
        ("mempalace-mcp", "mempalace", "mempalace"),
        ("graphify", "graphifyy", "graphifyy"),
    ):
        interpreter = generation / f"uv-tools/{environment}/{scripts}/{python_name}"
        link_metadata: dict[str, object] = {}
        if interpreter.is_symlink():
            link_target = os.readlink(interpreter)
            if Path(link_target).is_absolute():
                raise ValueError(f"dependency entrypoint link is not canonical: {interpreter}")
            target = _managed_link_target(generation, interpreter)
            digest, size = file_record(target)
            link_metadata = {
                "interpreterLinkTarget": link_target,
                "interpreterTarget": target.relative_to(generation).as_posix(),
            }
        else:
            digest, size = file_record(interpreter)
        records[name] = {"dispatch": {
            "kind": "python",
            "interpreter": interpreter.relative_to(generation).as_posix(),
            "interpreterSha256": digest,
            "interpreterSize": size,
            "distribution": distribution,
            "entrypoint": name,
            **link_metadata,
        }}
    for name, suffix in (
        ("memory", "dist/cli/main.js"),
        ("memory-mcp", "dist/mcp/server.js"),
    ):
        script = generation / f"npm/node_modules/@aictx/memory/{suffix}"
        digest, size = file_record(script)
        records[name] = {"dispatch": {
            "kind": "npm",
            "script": script.relative_to(generation).as_posix(),
            "scriptSha256": digest,
            "scriptSize": size,
            "entrypoint": name,
        }}
    return records


def _verify_dispatch_set(
    generation: Path, records: dict[str, dict[str, object]]
) -> None:
    for name in REQUIRED_DISPATCHES:
        dispatch = _validate_dispatch_metadata(name, records[name]["dispatch"])
        if dispatch["kind"] == "python":
            path, digest, size = (
                dispatch.get("interpreterTarget", dispatch["interpreter"]),
                dispatch["interpreterSha256"],
                dispatch["interpreterSize"],
            )
        elif dispatch["kind"] == "npm":
            path, digest, size = (
                dispatch["script"],
                dispatch["scriptSha256"],
                dispatch["scriptSize"],
            )
        else:
            path, digest, size = dispatch["path"], dispatch["sha256"], dispatch["size"]
        actual = _digest_regular_relative(
            generation,
            str(path),
            f"dispatch target for {name}",
            size,  # type: ignore[arg-type]
        )
        if actual != digest:
            raise ValueError(f"dependency dispatch digest drift detected: {name}")


def _crosscheck_dispatch_ownership(
    ownership: dict[str, object], records: dict[str, dict[str, object]]
) -> None:
    files = ownership.get("files")
    links = ownership.get("links")
    if not isinstance(files, dict) or not valid_link_records(links):
        raise ValueError("dependency sealed ownership files are invalid")
    for name, record in records.items():
        dispatch = record["dispatch"]
        kind = dispatch["kind"]
        if kind == "python":
            path = dispatch.get("interpreterTarget", dispatch["interpreter"])
            digest = dispatch["interpreterSha256"]
            if "interpreterTarget" in dispatch and not any(
                link.get("path") == dispatch["interpreter"]
                and link.get("target") == dispatch["interpreterLinkTarget"]
                for link in links
            ):
                raise ValueError(f"dependency dispatch ownership link drift detected: {name}")
        elif kind == "npm":
            path, digest = dispatch["script"], dispatch["scriptSha256"]
        else:
            path, digest = dispatch["path"], dispatch["sha256"]
        if files.get(path) != digest:
            raise ValueError(f"dependency dispatch ownership digest drift detected: {name}")


def prepare_candidate(
    project: Path,
    specification: dict[str, object],
    core_sha256: str,
    *,
    runner=None,
    now: datetime | None = None,
    generation_id: str | None = None,
    transaction_id: str | None = None,
) -> dict[str, str]:
    """Build once at final path.

    Concurrent path substitution is contained with held no-follow identities. A
    same-user installer subprocess remains trusted: ambient write authority can
    mutate any user-owned path and cannot be sandboxed by this stdlib controller.
    """
    project = _trusted_root(project.absolute(), "project")
    command_runner = runner or run_command
    generation_name = generation_id or secrets.token_hex(16)
    transaction_name = transaction_id or secrets.token_hex(16)
    if HEX_ID.fullmatch(generation_name) is None or HEX_ID.fullmatch(transaction_name) is None:
        raise ValueError("dependency generation or transaction identifier is invalid")
    if HEX_DIGEST.fullmatch(core_sha256) is None:
        raise ValueError("dependency core digest is invalid")
    core = _read_regular_relative(
        project, ".chaos-engine/manifest.json", "installed core manifest", MAX_CONTROL_BYTES
    )
    if hashlib.sha256(core).hexdigest() != core_sha256:
        raise ValueError("dependency installed core digest drift detected")
    generations = project / GENERATIONS_NAME
    transactions = project / ".chaos-engine-runtime-transactions"
    generation = generations / generation_name
    transaction = transactions / transaction_name
    holds = ExitStack()
    created_generation: tuple[int, int] | None = None
    created_transaction: tuple[int, int] | None = None
    generation_hold = None
    transaction_hold = None
    candidate_hold = None
    candidate_transaction_hold = None
    try:
        project_hold = holds.enter_context(_hold_directory(project, "project"))
        project_descriptor = project_hold[0]
        container_holds = []
        for path in (generations, transactions):
            if os.name != "nt" and project_descriptor is not None:
                try:
                    os.mkdir(path.name, dir_fd=project_descriptor)
                except FileExistsError:
                    # Re-entry authenticates the existing directory immediately below.
                    pass
                expected = os.stat(
                    path.name, dir_fd=project_descriptor, follow_symlinks=False
                )
            else:
                path.mkdir(exist_ok=True)
                expected = os.stat(path, follow_symlinks=False)
            held = holds.enter_context(_hold_directory(path, path.name))
            if held[1] != (expected.st_dev, expected.st_ino):
                raise ValueError("dependency container identity changed during open")
            container_holds.append((path, held, path.name))
        generation_hold = container_holds[0][1]
        transaction_hold = container_holds[1][1]
        if os.name != "nt":
            os.mkdir(generation_name, dir_fd=generation_hold[0])
            created = os.stat(
                generation_name, dir_fd=generation_hold[0], follow_symlinks=False
            )
            created_generation = (created.st_dev, created.st_ino)
            os.mkdir(transaction_name, dir_fd=transaction_hold[0])
            created = os.stat(
                transaction_name, dir_fd=transaction_hold[0], follow_symlinks=False
            )
            created_transaction = (created.st_dev, created.st_ino)
        else:
            generation.mkdir()
            created = os.stat(generation, follow_symlinks=False)
            created_generation = (created.st_dev, created.st_ino)
            transaction.mkdir()
            created = os.stat(transaction, follow_symlinks=False)
            created_transaction = (created.st_dev, created.st_ino)
        candidate_hold = holds.enter_context(
            _hold_directory(generation, "candidate generation")
        )
        candidate_transaction_hold = holds.enter_context(
            _hold_directory(transaction, "candidate transaction")
        )
        if candidate_hold[1] != created_generation or candidate_transaction_hold[1] != created_transaction:
            raise ValueError("dependency candidate identity changed during open")
        held_paths = [
            (project, project_hold, "project"),
            *container_holds,
            (generation, candidate_hold, "candidate generation"),
            (transaction, candidate_transaction_hold, "candidate transaction"),
        ]

        def validate_holds() -> None:
            for path, held, label in held_paths:
                _assert_held_directory(path, held, label)

        environment = generation_environment(generation, transaction)
        completed: dict[str, list[str]] = {}
        for tool, commands in generation_install_plan(generation, specification).items():
            completed[tool] = []
            for command in commands:
                validate_holds()
                try:
                    result = command_runner(command, environment)
                except (OSError, subprocess.SubprocessError) as error:
                    raise RuntimeError(f"{tool} install command failed: {command[0]}") from error
                validate_holds()
                completed[tool].append((result.stdout or result.stderr).strip())
        canonicalize_runtime_links(generation)
        records = _generation_dispatches(generation)
        probes = {
            "uv": ["--version"],
            "mempalace": ["--version"],
            "mempalace-mcp": ["--help"],
            "graphify": ["--version"],
            "memory": ["--help"],
            "memory-mcp": ["--help"],
        }
        for name, arguments in probes.items():
            validate_holds()
            try:
                result = command_runner(
                    dispatch_command(generation, {"tools": records}, name, arguments),
                    environment,
                )
            except (OSError, subprocess.SubprocessError) as error:
                raise RuntimeError(f"{name} entrypoint probe failed") from error
            validate_holds()
            _verify_dispatch_set(generation, records)
            records[name]["resolved"] = (result.stdout or result.stderr).strip()
        ownership = sealed_ownership_record(generation)
        _crosscheck_dispatch_ownership(ownership, records)
        receipt: dict[str, object] = {
            "schemaVersion": 2,
            "runtimeContractVersion": 3,
            "checkedAt": (now or datetime.now(timezone.utc)).isoformat(),
            "specificationSha256": specification_digest(specification),
            "coreSha256": core_sha256,
            "environment": {
                key: (
                    value
                    if key in {"UV_LINK_MODE", "PIP_NO_CACHE_DIR", "PYTHONDONTWRITEBYTECODE"}
                    else Path(value).relative_to(generation).as_posix()
                )
                for key, value in environment.items()
                if key
                not in {"UV_CACHE_DIR", "UV_TOOL_BIN_DIR", "UV_PYTHON_BIN_DIR", "NPM_CONFIG_CACHE"}
            },
            "installed": completed,
            "tools": records,
            "ownership": ownership,
        }
        receipt["receiptIntegritySha256"] = json_integrity(receipt)
        receipt_path = generation / RECEIPT_NAME
        receipt_path.write_text(
            json.dumps(receipt, sort_keys=True, separators=(",", ":")) + "\n",
            encoding="utf-8",
        )
        record = {
            "generationId": generation_name,
            "specificationSha256": specification_digest(specification),
            "coreSha256": core_sha256,
            "receiptSha256": sha256(receipt_path),
        }
        _validate_selected_generation(
            project,
            record,
            record["specificationSha256"],
            core_sha256,
            verify_installed_core=True,
        )
        return record
    finally:
        failed = sys.exc_info()[0] is not None
        removals = (
            (
                transaction,
                transaction_name,
                transaction_hold,
                candidate_transaction_hold,
                created_transaction,
            ),
            (
                generation,
                generation_name,
                generation_hold,
                candidate_hold,
                created_generation if failed else None,
            ),
        )
        try:
            if os.name != "nt":
                try:
                    for _, name, parent_hold, child_hold, identity in removals:
                        if identity is None or parent_hold is None:
                            continue
                        opened_here = None
                        if child_hold is None:
                            descriptor = os.open(
                                name,
                                os.O_RDONLY
                                | getattr(os, "O_DIRECTORY", 0)
                                | getattr(os, "O_NOFOLLOW", 0),
                                dir_fd=parent_hold[0],
                            )
                            state = os.fstat(descriptor)
                            child_hold = (descriptor, (state.st_dev, state.st_ino))
                            opened_here = descriptor
                        try:
                            _delete_held_child(parent_hold, name, child_hold, identity)
                        finally:
                            if opened_here is not None:
                                os.close(opened_here)
                finally:
                    holds.close()
            else:
                holds.close()
                for path, _, _, _, identity in removals:
                    if identity is not None:
                        _delete_windows_tree(path, identity)
        except Exception:
            if not failed:
                raise


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
    except (Exception, KeyboardInterrupt, SystemExit):
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
        digest.update(b"\0")
        digest.update(str(link.get("type", "symlink")).encode())
        digest.update(b"\0")
        digest.update(str(link.get("tag", "")).encode())
    return digest.hexdigest()


def valid_link_records(value: object) -> bool:
    return isinstance(value, list) and all(
        isinstance(link, dict)
        and set(link) in (
            {"path", "target"},
            {"path", "target", "type", "tag"},
        )
        and isinstance(link["path"], str)
        and isinstance(link["target"], str)
        and (
            set(link) == {"path", "target"}
            or (link["type"] == "junction" and link["tag"] == "0xa0000003")
        )
        for link in value
    )


def _lexical_path(path: Path) -> Path:
    value = os.path.abspath(path)
    if os.name == "nt" and value.startswith("\\\\?\\UNC\\"):
        value = f"\\\\{value[8:]}"
    elif os.name == "nt" and value.startswith("\\\\?\\"):
        value = value[4:]
    return Path(value)


def _windows_uv_junction_record(runtime: Path, path: Path) -> dict[str, str]:
    if os.name != "nt":
        raise ValueError("dependency runtime contains an unsupported reparse point")
    relative = path.relative_to(runtime).as_posix()
    alias = WINDOWS_UV_ALIAS.fullmatch(relative)
    state = os.lstat(path)
    if alias is None or getattr(state, "st_reparse_tag", 0) != WINDOWS_UV_JUNCTION_TAG:
        raise ValueError(f"dependency runtime contains an unsupported reparse point: {relative}")
    try:
        target = _lexical_path(Path(os.readlink(path)))
    except OSError as error:
        raise ValueError(f"dependency runtime junction is unreadable: {relative}") from error
    root = _lexical_path(runtime)
    if not target.is_relative_to(root) or target.parent != _lexical_path(path.parent):
        raise ValueError(f"dependency runtime junction escapes the runtime: {relative}")
    expected = re.fullmatch(
        rf"cpython-3\.10\.\d+-windows-{re.escape(alias.group('arch'))}-none",
        target.name,
    )
    if expected is None or is_link_or_reparse(target) or not target.is_dir():
        raise ValueError(f"dependency runtime junction target is invalid: {relative}")
    return {
        "path": relative,
        "target": target.relative_to(root).as_posix(),
        "type": "junction",
        "tag": "0xa0000003",
    }


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
            _windows_uv_junction_record(runtime, path)
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
            links.append(_windows_uv_junction_record(runtime, path))
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


def _identity_from_stat(value) -> dict[str, int]:
    return {
        "size": value.st_size,
        "mtimeNs": value.st_mtime_ns,
        "ctimeNs": value.st_ctime_ns,
        "mode": value.st_mode,
        "device": value.st_dev,
        "inode": value.st_ino,
    }


def _file_identity(path: Path) -> dict[str, int]:
    return _identity_from_stat(os.stat(path, follow_symlinks=False))


def sealed_ownership_record(runtime: Path) -> dict[str, object]:
    if is_link_or_reparse(runtime):
        raise ValueError(f"dependency runtime is a link or reparse point: {runtime}")
    files: dict[str, str] = {}
    identities: dict[str, dict[str, int]] = {}
    directories: list[str] = []
    links: list[dict[str, str]] = []
    for path in runtime_entries(runtime):
        relative = path.relative_to(runtime).as_posix()
        if path.is_symlink():
            target = os.readlink(path)
            if Path(target).is_absolute():
                raise ValueError(f"dependency runtime link target is not relative: {relative}")
            _managed_link_target(runtime, path)
            links.append({"path": relative, "target": target})
        elif is_link_or_reparse(path):
            links.append(_windows_uv_junction_record(runtime, path))
        elif path.is_dir():
            if not is_generated_python_cache(relative, directory=True):
                directories.append(relative)
        elif path.is_file() and relative != RECEIPT_NAME:
            if not is_generated_python_cache(relative):
                digest, identity = _capture_regular_relative(
                    runtime, relative, f"sealed generation file {relative}"
                )
                files[relative] = digest
                identities[relative] = identity
    return {
        "directories": directories,
        "files": files,
        "links": links,
        "sha256": ownership_digest(files, links),
        "identities": identities,
    }


def verify_sealed_ownership(
    runtime: Path, expected: object, *, full: bool = False
) -> None:
    if not isinstance(expected, dict):
        raise ValueError("dependency sealed generation ownership is invalid")
    files = expected.get("files")
    identities = expected.get("identities")
    directories = expected.get("directories")
    links = expected.get("links")
    if (
        not isinstance(files, dict)
        or not isinstance(identities, dict)
        or set(files) != set(identities)
        or not isinstance(directories, list)
        or not valid_link_records(links)
    ):
        raise ValueError("dependency sealed generation ownership is invalid")
    actual_files: set[str] = set()
    actual_directories: list[str] = []
    actual_links: list[dict[str, str]] = []
    for path in runtime_entries(runtime):
        relative = path.relative_to(runtime).as_posix()
        if path.is_symlink():
            target = os.readlink(path)
            _managed_link_target(runtime, path)
            actual_links.append({"path": relative, "target": target})
        elif is_link_or_reparse(path):
            actual_links.append(_windows_uv_junction_record(runtime, path))
        elif path.is_dir() and not is_generated_python_cache(relative, directory=True):
            actual_directories.append(relative)
        elif path.is_file() and relative != RECEIPT_NAME and not is_generated_python_cache(relative):
            actual_files.add(relative)
    if (
        actual_directories != directories
        or actual_links != links
        or actual_files != set(files)
    ):
        raise ValueError("dependency sealed generation contains unexpected or missing content")
    for relative, expected_identity in identities.items():
        if not isinstance(expected_identity, dict):
            raise ValueError("dependency sealed generation identity is invalid")
        current = _file_identity(runtime / relative)
        if not full and os.name != "nt" and current == expected_identity:
            continue
        digest, captured = _capture_regular_relative(
            runtime,
            relative,
            f"sealed generation file {relative}",
            current["size"],
        )
        immutable = {"size", "mode", "device", "inode"}
        if any(captured.get(key) != expected_identity.get(key) for key in immutable):
            raise ValueError("dependency sealed generation identity drift detected")
        if digest != files[relative]:
            raise ValueError("dependency sealed generation content drift detected")


def _remove_sealed_generation_contents(
    generation: Path,
    ownership: dict[str, object],
    receipt_sha256: str,
) -> bool:
    """Remove only receipt-owned entries; retain a nonempty generation as quarantine."""
    files = ownership["files"]
    identities = ownership["identities"]
    directories = ownership["directories"]
    links = ownership["links"]
    if not isinstance(files, dict) or not isinstance(identities, dict):
        raise ValueError("dependency sealed generation ownership is invalid")
    if not isinstance(directories, list) or not valid_link_records(links):
        raise ValueError("dependency sealed generation ownership is invalid")

    captured: dict[str, dict[str, int]] = {}
    for relative, expected_digest in files.items():
        expected_identity = identities.get(relative)
        if not isinstance(relative, str) or not isinstance(expected_identity, dict):
            raise ValueError("dependency sealed generation ownership is invalid")
        digest, identity = _capture_regular_relative(
            generation,
            relative,
            f"sealed generation removal file {relative}",
            expected_identity.get("size"),
        )
        immutable = {"size", "mode", "device", "inode"}
        if digest != expected_digest or any(
            identity.get(key) != expected_identity.get(key) for key in immutable
        ):
            raise ValueError("dependency sealed generation changed before removal")
        captured[relative] = identity

    receipt_bytes = _read_regular_relative(
        generation, RECEIPT_NAME, "generation removal receipt", MAX_RECEIPT_BYTES
    )
    if hashlib.sha256(receipt_bytes).hexdigest() != receipt_sha256:
        raise ValueError("dependency generation receipt changed before removal")
    receipt_identity = _file_identity(generation / RECEIPT_NAME)

    for link in links:
        relative = str(link["path"])
        path = generation / relative
        if path.is_symlink():
            if os.readlink(path) != link["target"]:
                raise ValueError("dependency sealed generation link changed before removal")
            _managed_link_target(generation, path)
        elif is_link_or_reparse(path):
            if _windows_uv_junction_record(generation, path) != link:
                raise ValueError("dependency sealed generation link changed before removal")
        else:
            raise ValueError("dependency sealed generation link is missing before removal")

    for relative, identity in captured.items():
        path = generation / relative
        current = _file_identity(path)
        if any(current.get(key) != identity.get(key) for key in ("size", "device", "inode")):
            raise ValueError("dependency sealed generation changed during removal")
        path.unlink()

    for link in sorted(links, key=lambda item: str(item["path"]), reverse=True):
        path = generation / str(link["path"])
        if path.is_symlink():
            path.unlink()
        else:
            path.rmdir()

    if _file_identity(generation / RECEIPT_NAME) != receipt_identity:
        raise ValueError("dependency generation receipt changed during removal")
    (generation / RECEIPT_NAME).unlink()

    generated_directories: set[str] = set()
    for path in reversed(runtime_entries(generation)):
        relative = path.relative_to(generation).as_posix()
        if is_link_or_reparse(path):
            continue
        if path.is_file() and is_generated_python_cache(relative):
            path.unlink()
        elif path.is_dir() and is_generated_python_cache(relative, directory=True):
            generated_directories.add(relative)

    removable_directories = {
        str(relative) for relative in directories if isinstance(relative, str)
    } | generated_directories
    for relative in sorted(
        removable_directories, key=lambda value: (value.count("/"), value), reverse=True
    ):
        path = generation / relative
        if path.is_dir() and not is_link_or_reparse(path):
            try:
                path.rmdir()
            except OSError as error:
                if error.errno not in {errno.ENOTEMPTY, errno.EEXIST}:
                    raise
    try:
        generation.rmdir()
    except OSError as error:
        if error.errno not in {errno.ENOTEMPTY, errno.EEXIST}:
            raise
        return False
    return True


def remove_generation(project: Path, record: dict[str, str]) -> None:
    """Delete one unselected verified generation without following path links."""
    project = _trusted_root(project.absolute(), "project")
    record = _validate_generation_record(record)
    pointer_path = project / POINTER_NAME
    if pointer_path.exists() or is_link_or_reparse(pointer_path):
        pointer = _read_pointer(project)
        if record in (pointer.get("active"), pointer.get("previous")):
            raise ValueError("dependency selected generation cannot be removed")
    generation = _validate_selected_generation(
        project,
        record,
        record["specificationSha256"],
        record["coreSha256"],
        verify_installed_core=False,
    )
    receipt, _ = _bounded_json(
        project,
        f"{GENERATIONS_NAME}/{record['generationId']}/{RECEIPT_NAME}",
        "generation receipt",
        MAX_RECEIPT_BYTES,
    )
    generations = project / GENERATIONS_NAME
    with ExitStack() as holds:
        parent = holds.enter_context(_hold_directory(generations, "generation container"))
        child = holds.enter_context(_hold_directory(generation, "retired generation"))
        expected = child[1]
        named = os.stat(record["generationId"], dir_fd=parent[0], follow_symlinks=False) if (
            os.name != "nt" and isinstance(parent[0], int)
        ) else os.stat(generation, follow_symlinks=False)
        if (named.st_dev, named.st_ino) != expected:
            raise ValueError("dependency generation identity changed before removal")
        verify_sealed_ownership(generation, receipt["ownership"], full=True)
    _remove_sealed_generation_contents(
        generation,
        receipt["ownership"],
        record["receiptSha256"],
    )


def _pointer_generation_records(pointer: dict[str, object]) -> list[dict[str, str]]:
    records = [_validate_generation_record(pointer["active"])]
    if pointer.get("previous") is not None:
        previous = _validate_generation_record(pointer["previous"])
        if previous not in records:
            records.append(previous)
    return records


def prepare_generation_remove(
    project: Path,
    *,
    expected_specification_sha256: str,
    expected_core_sha256: str,
) -> list[dict[str, str]]:
    """Authenticate selected generations and tombstone the active pointer."""
    project = _trusted_root(project.absolute(), "project")
    pointer_path = project / POINTER_NAME
    removing_path = project / POINTER_REMOVING_NAME
    if is_link_or_reparse(pointer_path) or is_link_or_reparse(removing_path):
        raise ValueError("dependency pointer removal path is a link or reparse point")
    if removing_path.exists():
        raise ValueError("dependency generation removal recovery is required")
    pointer = _read_pointer(project)
    records = _pointer_generation_records(pointer)
    for record in records:
        generation = _validate_selected_generation(
            project,
            record,
            record["specificationSha256"],
            record["coreSha256"],
            verify_installed_core=False,
        )
        receipt, _ = _bounded_json(
            project,
            f"{GENERATIONS_NAME}/{record['generationId']}/{RECEIPT_NAME}",
            "generation receipt",
            MAX_RECEIPT_BYTES,
        )
        verify_sealed_ownership(generation, receipt["ownership"], full=True)
    active = records[0]
    if (
        active["specificationSha256"] != expected_specification_sha256
        or active["coreSha256"] != expected_core_sha256
    ):
        raise ValueError("dependency active generation does not match installed core")
    pointer_path.replace(removing_path)
    return records


def cancel_generation_remove(project: Path) -> None:
    """Restore a prepared pointer after another uninstall resource failed."""
    project = _trusted_root(project.absolute(), "project")
    pointer_path = project / POINTER_NAME
    removing_path = project / POINTER_REMOVING_NAME
    if pointer_path.exists() or is_link_or_reparse(pointer_path):
        raise ValueError("dependency generation removal cannot be cancelled")
    pointer = _read_pointer_at(project, POINTER_REMOVING_NAME)
    for record in _pointer_generation_records(pointer):
        _validate_selected_generation(
            project,
            record,
            record["specificationSha256"],
            record["coreSha256"],
            verify_installed_core=False,
        )
    removing_path.replace(pointer_path)


def finalize_generation_remove(project: Path) -> None:
    """Remove only authenticated selected generations, leaving foreign files intact."""
    project = _trusted_root(project.absolute(), "project")
    pointer_path = project / POINTER_NAME
    removing_path = project / POINTER_REMOVING_NAME
    if pointer_path.exists() or is_link_or_reparse(pointer_path):
        raise ValueError("dependency generation removal is not prepared")
    if not removing_path.exists():
        return
    pointer = _read_pointer_at(project, POINTER_REMOVING_NAME)
    for record in _pointer_generation_records(pointer):
        generation = project / GENERATIONS_NAME / record["generationId"]
        if generation.exists() or is_link_or_reparse(generation):
            remove_generation(project, record)
    generations = project / GENERATIONS_NAME
    transactions = project / TRANSACTIONS_NAME
    for container in (generations, transactions):
        if is_link_or_reparse(container):
            raise ValueError("dependency runtime container is a link or reparse point")
        if container.is_dir() and not any(container.iterdir()):
            container.rmdir()
    removing_path.unlink()


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
        except (Exception, KeyboardInterrupt, SystemExit):
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
