#!/usr/bin/env python3
"""Install the portable ChaosEngine tree into a consumer project."""

from __future__ import annotations

import argparse
from contextlib import contextmanager
import hashlib
import json
import os
import re
import shutil
import sys
import tempfile
import zipfile
from pathlib import Path


INSTALL_DIRECTORY = ".chaos-engine"
MANIFEST_NAME = "manifest.json"
SCHEMA_VERSION = 1
COMMIT_PATTERN = re.compile(r"[0-9a-f]{40}")
LOCK_NAME = ".chaos-engine.lock"
BACKUP_NAME = ".chaos-engine.backup"
JOURNAL_NAME = ".chaos-engine.transaction.json"
LOCK_MAGIC = b"chaos-engine-lock-v1\n"
NEXT_BACKUP_NAME = ".chaos-engine.backup.next"
OLD_BACKUP_NAME = ".chaos-engine.backup.old"
UNINSTALL_ARCHIVE_NAME = ".chaos-engine-uninstall-recovery.zip"
UNINSTALL_CURRENT_NAME = ".chaos-engine-uninstall-current"
UNINSTALL_OLD_BACKUP_NAME = ".chaos-engine-uninstall-old-backup"


def file_sha256(path: Path) -> str:
    return hashlib.sha256(path.read_bytes()).hexdigest()


def is_link_or_reparse(path: Path) -> bool:
    if path.is_symlink():
        return True
    try:
        attributes = getattr(os.lstat(path), "st_file_attributes", 0)
    except FileNotFoundError:
        return False
    return bool(attributes & 0x400)


def reject_link_or_reparse(path: Path) -> None:
    if is_link_or_reparse(path):
        raise ValueError(f"path is a link or reparse point: {path}")


def require_absent(path: Path, label: str) -> None:
    if path.exists() or is_link_or_reparse(path):
        raise ValueError(f"{label} collision: {path}")


def source_files(source: Path) -> tuple[Path, ...]:
    reject_link_or_reparse(source)
    if not (source / "skills/chaos-engine/SKILL.md").is_file():
        raise ValueError(f"source is not a portable ChaosEngine tree: {source}")
    if (source / MANIFEST_NAME).exists():
        raise ValueError(f"source contains the reserved manifest path: {MANIFEST_NAME}")
    files: list[Path] = []
    for path in sorted(source.rglob("*")):
        relative = path.relative_to(source)
        if "__pycache__" in relative.parts or path.suffix == ".pyc":
            continue
        if is_link_or_reparse(path):
            raise ValueError(f"source contains a link or reparse point: {relative}")
        if path.is_file():
            files.append(path)
    return tuple(files)


def verify_staged_payload(stage: Path, ownership: dict[str, str]) -> None:
    staged = {
        path.relative_to(stage).as_posix(): file_sha256(path)
        for path in sorted(stage.rglob("*"))
        if path.is_file()
    }
    if staged != ownership:
        raise ValueError("staged payload does not match the immutable ownership plan")


def load_manifest(target: Path) -> dict[str, object]:
    try:
        manifest = json.loads((target / MANIFEST_NAME).read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError) as error:
        raise ValueError(f"ChaosEngine manifest is missing or invalid: {target}") from error
    if not isinstance(manifest, dict) or manifest.get("schemaVersion") != SCHEMA_VERSION:
        raise ValueError("ChaosEngine manifest schema is unsupported")
    source = manifest.get("source")
    files = manifest.get("files")
    if (
        not isinstance(source, dict)
        or COMMIT_PATTERN.fullmatch(str(source.get("commit", ""))) is None
        or not isinstance(files, dict)
        or any(not isinstance(path, str) or not isinstance(digest, str) for path, digest in files.items())
    ):
        raise ValueError("ChaosEngine manifest has an invalid ownership record")
    return manifest


def installed_payload(target: Path) -> dict[str, str]:
    reject_link_or_reparse(target)
    payload: dict[str, str] = {}
    for path in sorted(target.rglob("*")):
        reject_link_or_reparse(path)
        relative = path.relative_to(target).as_posix()
        if path.is_file() and relative != MANIFEST_NAME:
            payload[relative] = file_sha256(path)
    return payload


def verify_install(target: Path) -> dict[str, object]:
    manifest = load_manifest(target)
    if installed_payload(target) != manifest["files"]:
        raise ValueError(f"ChaosEngine ownership drift detected: {target}")
    return manifest


@contextmanager
def project_lock(project: Path):
    project = project.resolve()
    lock_path = project / LOCK_NAME
    flags = os.O_RDWR | getattr(os, "O_BINARY", 0)
    created = False
    try:
        descriptor = os.open(lock_path, flags | os.O_CREAT | os.O_EXCL, 0o600)
        created = True
    except FileExistsError:
        reject_link_or_reparse(lock_path)
        descriptor = os.open(lock_path, flags)
    lock_file = os.fdopen(descriptor, "r+b", closefd=True)
    opened = os.fstat(lock_file.fileno())
    named = os.stat(lock_path, follow_symlinks=False)
    if (opened.st_dev, opened.st_ino) != (named.st_dev, named.st_ino):
        lock_file.close()
        raise ValueError(f"ChaosEngine lock collision: {lock_path}")
    if created:
        lock_file.write(LOCK_MAGIC)
        lock_file.flush()
        os.fsync(lock_file.fileno())
    else:
        lock_file.seek(0)
        try:
            lock_contents = lock_file.read()
        except PermissionError as error:
            lock_file.close()
            raise RuntimeError("another ChaosEngine operation is already running") from error
        if lock_contents != LOCK_MAGIC:
            lock_file.close()
            raise ValueError(f"ChaosEngine lock collision: {lock_path}")
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
        raise RuntimeError("another ChaosEngine operation is already running") from error
    try:
        yield
    finally:
        lock_file.seek(0)
        if os.name == "nt":
            msvcrt.locking(lock_file.fileno(), msvcrt.LK_UNLCK, 1)
        else:
            fcntl.flock(lock_file.fileno(), fcntl.LOCK_UN)
        lock_file.close()


def write_journal(project: Path, operation: str, commit: str) -> Path:
    journal = project / JOURNAL_NAME
    temporary = journal.with_suffix(journal.suffix + ".tmp")
    reject_link_or_reparse(journal)
    payload = (
        json.dumps({"schemaVersion": 1, "operation": operation, "commit": commit}, sort_keys=True)
        + "\n"
    ).encode()
    flags = os.O_WRONLY | os.O_CREAT | os.O_EXCL | getattr(os, "O_BINARY", 0)
    try:
        descriptor = os.open(temporary, flags, 0o600)
    except FileExistsError as error:
        raise ValueError(f"transaction journal scratch path collision: {temporary}") from error
    with os.fdopen(descriptor, "wb") as handle:
        handle.write(payload)
        handle.flush()
        os.fsync(handle.fileno())
        opened = os.fstat(handle.fileno())
        named = os.stat(temporary, follow_symlinks=False)
        if (opened.st_dev, opened.st_ino) != (named.st_dev, named.st_ino):
            raise ValueError(f"transaction journal scratch path collision: {temporary}")
    temporary.replace(journal)
    return journal


def publish_staged_tree(stage: Path, target: Path, displaced: Path) -> None:
    previous = target.exists()
    if previous:
        require_absent(displaced, "update displaced-tree path")
        target.replace(displaced)
    try:
        stage.replace(target)
    except BaseException:
        if previous and displaced.exists() and not target.exists():
            displaced.replace(target)
        raise


def archive_install(source: Path, archive: Path) -> None:
    require_absent(archive, "uninstall recovery archive")
    temporary = archive.with_suffix(archive.suffix + ".tmp")
    require_absent(temporary, "uninstall recovery archive scratch path")
    manifest = verify_install(source)
    try:
        with zipfile.ZipFile(temporary, "x", compression=zipfile.ZIP_DEFLATED) as bundle:
            bundle.writestr(MANIFEST_NAME, (source / MANIFEST_NAME).read_bytes())
            for relative in manifest["files"]:  # type: ignore[union-attr]
                bundle.write(source / str(relative), str(relative))
        with zipfile.ZipFile(temporary) as bundle:
            archived = {
                name: hashlib.sha256(bundle.read(name)).hexdigest()
                for name in bundle.namelist()
                if name != MANIFEST_NAME
            }
        if archived != manifest["files"]:
            raise ValueError("uninstall recovery archive failed verification")
        temporary.replace(archive)
    finally:
        temporary.unlink(missing_ok=True)


def restore_archive(archive: Path, target: Path) -> None:
    require_absent(target, "uninstall restore target")
    stage = Path(tempfile.mkdtemp(prefix=f"{INSTALL_DIRECTORY}-restore-", dir=target.parent))
    try:
        with zipfile.ZipFile(archive) as bundle:
            bundle.extractall(stage)
        verify_install(stage)
        stage.replace(target)
    finally:
        if stage.exists():
            shutil.rmtree(stage)


def _recover_transaction(project: Path) -> None:
    journal = project / JOURNAL_NAME
    if not journal.exists():
        return
    reject_link_or_reparse(journal)
    try:
        record = json.loads(journal.read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError) as error:
        raise ValueError("transaction journal is invalid") from error
    operation = record.get("operation") if isinstance(record, dict) else None
    target = project / INSTALL_DIRECTORY
    backup = project / BACKUP_NAME
    swap = project / f"{INSTALL_DIRECTORY}-rollback"
    displaced = project / NEXT_BACKUP_NAME
    old_backup = project / OLD_BACKUP_NAME
    uninstall_archive = project / UNINSTALL_ARCHIVE_NAME
    uninstall_current = project / UNINSTALL_CURRENT_NAME
    uninstall_old_backup = project / UNINSTALL_OLD_BACKUP_NAME
    for path in (
        target,
        backup,
        swap,
        displaced,
        old_backup,
        uninstall_archive,
        uninstall_current,
        uninstall_old_backup,
    ):
        reject_link_or_reparse(path)
    if operation in ("install", "update"):
        if not target.exists() and displaced.exists():
            verify_install(displaced)
            displaced.replace(target)
        elif not target.exists() and backup.exists() and not displaced.exists():
            verify_install(backup)
            backup.replace(target)
        elif not target.exists() and not backup.exists() and not displaced.exists():
            journal.unlink()
            return
        verify_install(target)
        if backup.exists():
            verify_install(backup)
        if displaced.exists():
            verify_install(displaced)
            if backup.exists():
                require_absent(old_backup, "obsolete backup path")
                backup.replace(old_backup)
            displaced.replace(backup)
            verify_install(backup)
        if old_backup.exists():
            shutil.rmtree(old_backup)
    elif operation == "rollback":
        if swap.exists() and not target.exists() and backup.exists():
            verify_install(swap)
            verify_install(backup)
            swap.replace(target)
        elif swap.exists() and target.exists() and not backup.exists():
            verify_install(swap)
            verify_install(target)
            swap.replace(backup)
        elif swap.exists():
            raise ValueError("rollback recovery has an ambiguous mixed state")
        verify_install(target)
        verify_install(backup)
    elif operation == "uninstall":
        if target.exists():
            verify_install(target)
        elif uninstall_current.exists():
            try:
                verify_install(uninstall_current)
            except ValueError:
                if not uninstall_archive.exists():
                    raise
                shutil.rmtree(uninstall_current)
                restore_archive(uninstall_archive, target)
            else:
                uninstall_current.replace(target)
        elif uninstall_archive.exists():
            restore_archive(uninstall_archive, target)
        else:
            raise ValueError("uninstall recovery has no verified tree")
        verify_install(target)
        if uninstall_old_backup.exists() and not backup.exists():
            shutil.rmtree(uninstall_old_backup)
        if uninstall_current.exists():
            shutil.rmtree(uninstall_current)
        if uninstall_archive.exists():
            uninstall_archive.unlink()
    else:
        raise ValueError("transaction journal operation is unsupported")
    journal.unlink()


def recover_transaction(project: Path) -> None:
    project = project.resolve()
    with project_lock(project):
        _recover_transaction(project)


def install(project: Path, source: Path, commit: str) -> Path:
    project = project.resolve()
    reject_link_or_reparse(source.absolute())
    source = source.resolve()
    if not project.is_dir():
        raise ValueError(f"project is not a directory: {project}")
    if COMMIT_PATTERN.fullmatch(commit) is None:
        raise ValueError("commit must be a lowercase 40-hex revision")
    if source == project or source.is_relative_to(project) or project.is_relative_to(source):
        raise ValueError("ChaosEngine source and project trees must be disjoint")

    files = source_files(source)
    ownership = {path.relative_to(source).as_posix(): file_sha256(path) for path in files}
    target = project / INSTALL_DIRECTORY
    backup = project / BACKUP_NAME
    displaced = project / NEXT_BACKUP_NAME
    old_backup = project / OLD_BACKUP_NAME
    with project_lock(project):
        _recover_transaction(project)
        if target.exists():
            current = verify_install(target)
            current_commit = current["source"]["commit"]  # type: ignore[index]
            if current_commit == commit:
                if current["files"] != ownership:
                    raise ValueError("same commit resolved to a different ChaosEngine payload")
                return target
        if backup.exists():
            require_absent(old_backup, "obsolete backup path")
        stage = Path(tempfile.mkdtemp(prefix=f"{INSTALL_DIRECTORY}-stage-", dir=project))
        try:
            for path in files:
                relative = path.relative_to(source)
                destination = stage / relative
                destination.parent.mkdir(parents=True, exist_ok=True)
                shutil.copy2(path, destination)
            verify_staged_payload(stage, ownership)
            manifest = {
                "schemaVersion": SCHEMA_VERSION,
                "source": {"commit": commit, "kind": "local"},
                "files": ownership,
            }
            (stage / MANIFEST_NAME).write_text(
                json.dumps(manifest, indent=2, sort_keys=True) + "\n", encoding="utf-8"
            )
            journal = write_journal(project, "update" if target.exists() else "install", commit)
            publish_staged_tree(stage, target, displaced)
            verify_install(target)
            if displaced.exists():
                verify_install(displaced)
                if backup.exists():
                    verify_install(backup)
                    require_absent(old_backup, "obsolete backup path")
                    backup.replace(old_backup)
                displaced.replace(backup)
                verify_install(backup)
            if old_backup.exists():
                shutil.rmtree(old_backup)
            journal.unlink()
        finally:
            if stage.exists():
                shutil.rmtree(stage)
    return target


def status(project: Path) -> dict[str, str]:
    project = project.resolve()
    with project_lock(project):
        manifest = verify_install(project / INSTALL_DIRECTORY)
        state = "recovery-required" if (project / JOURNAL_NAME).exists() else "healthy"
        return {"status": state, "commit": str(manifest["source"]["commit"])}  # type: ignore[index]


def rollback(project: Path) -> Path:
    project = project.resolve()
    target = project / INSTALL_DIRECTORY
    backup = project / BACKUP_NAME
    swap = project / f"{INSTALL_DIRECTORY}-rollback"
    with project_lock(project):
        _recover_transaction(project)
        if not target.exists() and backup.exists():
            verify_install(backup)
            backup.replace(target)
            return target
        current = verify_install(target)
        previous = verify_install(backup)
        require_absent(swap, "rollback scratch path")
        journal = write_journal(project, "rollback", str(previous["source"]["commit"]))  # type: ignore[index]
        target.replace(swap)
        try:
            backup.replace(target)
            swap.replace(backup)
        except BaseException:
            raise
        verify_install(target)
        verify_install(backup)
        journal.unlink()
        del current
    return target


def uninstall(project: Path) -> None:
    project = project.resolve()
    target = project / INSTALL_DIRECTORY
    backup = project / BACKUP_NAME
    archive = project / UNINSTALL_ARCHIVE_NAME
    removed = project / UNINSTALL_CURRENT_NAME
    old_backup = project / UNINSTALL_OLD_BACKUP_NAME
    with project_lock(project):
        _recover_transaction(project)
        if not target.exists():
            if backup.exists():
                verify_install(backup)
            return
        manifest = verify_install(target)
        require_absent(archive, "uninstall recovery archive")
        require_absent(
            archive.with_suffix(archive.suffix + ".tmp"),
            "uninstall recovery archive scratch path",
        )
        require_absent(removed, "uninstall current-tree path")
        require_absent(old_backup, "uninstall old-backup path")
        if backup.exists():
            verify_install(backup)
        journal = write_journal(project, "uninstall", str(manifest["source"]["commit"]))  # type: ignore[index]
        target.replace(removed)
        verify_install(removed)
        archive_install(removed, archive)
        if backup.exists():
            backup.replace(old_backup)
        try:
            shutil.rmtree(removed)
        except BaseException:
            if not target.exists():
                restore_archive(archive, target)
            if old_backup.exists() and not backup.exists():
                old_backup.replace(backup)
            raise
        if old_backup.exists():
            shutil.rmtree(old_backup)
        archive.unlink()
        journal.unlink()


def parser() -> argparse.ArgumentParser:
    result = argparse.ArgumentParser(description=__doc__)
    commands = result.add_subparsers(dest="command", required=True)
    install_command = commands.add_parser("install")
    install_command.add_argument("--project", required=True, type=Path)
    install_command.add_argument("--source", required=True, type=Path)
    install_command.add_argument("--commit", required=True)
    for name in ("status", "rollback", "uninstall"):
        command = commands.add_parser(name)
        command.add_argument("--project", required=True, type=Path)
    return result


def main() -> int:
    args = parser().parse_args()
    try:
        if args.command == "install":
            target = install(args.project, args.source, args.commit)
            result: object = {"status": "installed", "root": str(target)}
        elif args.command == "status":
            result = status(args.project)
        elif args.command == "rollback":
            result = {"status": "rolled-back", "root": str(rollback(args.project))}
        else:
            uninstall(args.project)
            result = {"status": "uninstalled"}
    except (OSError, RuntimeError, ValueError) as error:
        print(str(error), file=sys.stderr)
        return 1
    print(json.dumps(result))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
