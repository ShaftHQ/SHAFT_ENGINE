#!/usr/bin/env python3
"""Plan and verify ChaosEngine-owned runtime dependencies."""

from __future__ import annotations

import argparse
from contextlib import contextmanager, nullcontext
import hashlib
import json
import os
import shutil
import subprocess  # nosec B404 - fixed list-form dependency commands from tracked spec.
import sys
from datetime import datetime, timedelta, timezone
from pathlib import Path


RECEIPT_SCHEMA = 1
STALE_AFTER = timedelta(hours=24)
RECEIPT_NAME = "receipt.json"
LOCK_MAGIC = b"chaos-engine-dependencies-lock-v1\n"
BUILD_MARKER_MAGIC = "chaos-engine-dependencies-build-v1\n"
BUILD_MARKER_OWNED_SUFFIX = ".owned"


def is_link_or_reparse(path: Path) -> bool:
    if path.is_symlink():
        return True
    try:
        attributes = getattr(os.lstat(path), "st_file_attributes", 0)
    except FileNotFoundError:
        return False
    return bool(attributes & 0x400)


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
    encoded = json.dumps(specification, sort_keys=True, separators=(",", ":")).encode()
    return hashlib.sha256(encoded).hexdigest()


def ownership_record(runtime: Path) -> dict[str, object]:
    if is_link_or_reparse(runtime):
        raise ValueError(f"dependency runtime is a link or reparse point: {runtime}")
    digest = hashlib.sha256()
    files: dict[str, str] = {}
    directories: list[str] = []
    for path in sorted(runtime.rglob("*")):
        relative = path.relative_to(runtime).as_posix()
        if is_link_or_reparse(path):
            raise ValueError(f"dependency runtime contains a link: {relative}")
        if path.is_dir():
            directories.append(relative)
        elif path.is_file() and relative != RECEIPT_NAME:
            digest.update(relative.encode())
            digest.update(b"\0")
            file_digest = sha256(path)
            digest.update(bytes.fromhex(file_digest))
            files[relative] = file_digest
    return {"directories": directories, "files": files, "sha256": digest.hexdigest()}


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
            [sys.executable, "-m", "venv", str(environment)],
            [executable(scripts, "python"), "-m", "pip", "install", "--upgrade", str(tools["uv"]["package"])],  # type: ignore[index]
        ],
        "mempalace": [
            [uv, "tool", "install", str(tools["mempalace"]["package"])],  # type: ignore[index]
        ],
        "graphify": [
            [uv, "tool", "install", "--with", str(graphify["with"][0]), str(graphify["package"])],  # type: ignore[index]
        ],
        "memory": [
            [npm, "install", "--prefix", str(npm_prefix), str(tools["memory"]["package"])],  # type: ignore[index]
        ],
    }


def tool_environment(runtime: Path) -> dict[str, str]:
    return {
        "UV_TOOL_DIR": str(runtime / "uv-tools"),
        "UV_TOOL_BIN_DIR": str(runtime / "bin"),
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
        "checkedAt": (now or datetime.now(timezone.utc)).isoformat(),
        "specificationSha256": specification_digest(specification),
        "environment": {
            key: (
                value
                if key == "PYTHONDONTWRITEBYTECODE"
                else Path(value).relative_to(runtime).as_posix()
            )
            for key, value in environment.items()
        },
        "installed": completed,
        "tools": records,
    }
    metadata = json.dumps(receipt, sort_keys=True, separators=(",", ":")).encode()
    ownership = ownership_record(runtime)
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
            verify_receipt(runtime, current)
            if not force:
                doctor(runtime, runner=runner, now=now, specification=specification)
                return current
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
                if runtime.exists():
                    verify_receipt(runtime, read_receipt(runtime))
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
    ownership = receipt["ownership"]
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
        ownership = receipt["ownership"]
        files = ownership.get("files") if isinstance(ownership, dict) else None
        directories = ownership.get("directories") if isinstance(ownership, dict) else None
        if not isinstance(files, dict) or not isinstance(directories, list):
            raise ValueError("dependency removal ownership record is invalid")
        allowed = set(files) | {RECEIPT_NAME}
        present = {
            path.relative_to(removing).as_posix()
            for path in removing.rglob("*")
            if path.is_file()
        }
        if not present <= allowed:
            raise ValueError("dependency removal contains an unowned file")
        for relative in sorted(present - {RECEIPT_NAME}):
            path = removing / relative
            if sha256(path) != files[relative]:
                raise ValueError("dependency removal ownership drift detected")
            path.unlink()
        present_directories = {
            path.relative_to(removing).as_posix()
            for path in removing.rglob("*")
            if path.is_dir()
        }
        expected_directories = set(directories)
        if not present_directories <= expected_directories:
            raise ValueError("dependency removal directory ownership drift detected")
        for directory in sorted(
            (path for path in removing.rglob("*") if path.is_dir()),
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
