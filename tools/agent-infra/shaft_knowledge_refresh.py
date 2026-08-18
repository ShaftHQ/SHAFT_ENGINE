#!/usr/bin/env python3
"""Refresh an installer-owned SHAFT clone and its knowledge stores."""

from __future__ import annotations

import argparse
from contextlib import contextmanager
import importlib.util
import json
import os
from pathlib import Path
import re
import shutil
import stat
import subprocess  # nosec B404 - fixed list-form maintenance commands.
import sys
from typing import Iterator

ORIGIN = "https://github.com/ShaftHQ/SHAFT_ENGINE"
WING = "shaft_engine_main"
TRUST_MODEL = "exclusive-maintenance-home-v1"
GIT_ROUTING_VARIABLES = (
    "GIT_DIR",
    "GIT_COMMON_DIR",
    "GIT_WORK_TREE",
    "GIT_INDEX_FILE",
    "GIT_OBJECT_DIRECTORY",
    "GIT_ALTERNATE_OBJECT_DIRECTORIES",
    "GIT_CONFIG_GLOBAL",
    "GIT_CONFIG_SYSTEM",
    "GIT_CONFIG_COUNT",
    "GIT_CONFIG_PARAMETERS",
)


def _load_promote():
    path = Path(__file__).resolve().parents[1] / "repository-map/mempalace_promote.py"
    spec = importlib.util.spec_from_file_location("mempalace_promote", path)
    if spec is None or spec.loader is None:
        raise RuntimeError(f"cannot load MemPalace promote helper: {path}")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


_promote = _load_promote()
list_promote_paths = _promote.list_promote_paths
include_ignored_batches = _promote.include_ignored_batches


def required(name: str) -> str:
    executable = shutil.which(name)
    if executable is None:
        raise ValueError(f"required executable is not on PATH: {name}")
    return executable


def run(
    arguments: list[str],
    cwd: Path,
    environment=None,
    allowed_exit_codes: tuple[int, ...] = (0,),
) -> str:
    completed = subprocess.run(  # nosec B603
        arguments, cwd=cwd, env=environment, check=False,
        stdout=subprocess.PIPE, stderr=subprocess.STDOUT, text=True,
    )
    if completed.returncode not in allowed_exit_codes:
        raise subprocess.CalledProcessError(
            completed.returncode, arguments, output=completed.stdout
        )
    if completed.stdout:
        print(completed.stdout, end="" if completed.stdout.endswith("\n") else "\n")
    return completed.stdout


def git_environment() -> dict[str, str]:
    """Return an environment that cannot redirect Git outside the owned clone."""
    environment = os.environ.copy()
    for name in GIT_ROUTING_VARIABLES:
        environment.pop(name, None)
    for name in tuple(environment):
        if name.startswith(("GIT_CONFIG_KEY_", "GIT_CONFIG_VALUE_")):
            environment.pop(name)
    environment["GIT_CONFIG_NOSYSTEM"] = "1"
    environment["GIT_CONFIG_GLOBAL"] = os.devnull
    environment["GIT_OPTIONAL_LOCKS"] = "0"
    return environment


def git(arguments: list[str], cwd: Path) -> str:
    completed = subprocess.run(  # nosec B603
        [required("git"), *arguments], cwd=cwd, env=git_environment(), check=True,
        stdout=subprocess.PIPE, stderr=subprocess.STDOUT, text=True,
    )
    return completed.stdout


def is_system_drive(path: Path) -> bool:
    return os.name == "nt" and path.drive.casefold() == os.environ.get(
        "SystemDrive", "C:"
    ).casefold()


def is_reparse(path: Path) -> bool:
    if path.is_symlink() or (
        hasattr(path, "is_junction") and path.is_junction()  # type: ignore[attr-defined]
    ):
        return True
    try:
        attributes = getattr(path.stat(follow_symlinks=False), "st_file_attributes", 0)
    except FileNotFoundError:
        return False
    except OSError as error:
        raise ValueError(f"cannot inspect maintenance path metadata: {path}") from error
    return bool(attributes & getattr(stat, "FILE_ATTRIBUTE_REPARSE_POINT", 0x400))


def has_unsafe_descendant(path: Path) -> bool:
    """Inspect an owned tree without following reparses or multi-link files."""
    if not path.is_dir():
        return False
    pending = [path]
    while pending:
        current = pending.pop()
        try:
            entries = list(os.scandir(current))
        except OSError as error:
            raise ValueError(f"cannot inspect owned maintenance tree: {current}") from error
        for entry in entries:
            candidate = Path(entry.path)
            if is_reparse(candidate):
                return True
            try:
                metadata = candidate.stat(follow_symlinks=False)
            except OSError as error:
                raise ValueError(f"cannot inspect owned maintenance entry: {candidate}") from error
            if not entry.is_dir(follow_symlinks=False) and metadata.st_nlink != 1:
                return True
            if entry.is_dir(follow_symlinks=False):
                pending.append(candidate)
    return False


@contextmanager
def trusted_graph_output(path: Path) -> Iterator[None]:
    """Validate Graphify output under the exclusive-writer maintenance contract."""
    path.mkdir(exist_ok=True)
    if is_reparse(path) or has_unsafe_descendant(path):
        raise ValueError("Graphify output contains a reparse point")
    yield
    if is_reparse(path) or has_unsafe_descendant(path):
        raise ValueError("Graphify output gained a reparse point during refresh")


def validate_lexical_home(requested_root: Path) -> Path:
    """Validate an existing ancestor chain before any maintenance-home write."""
    lexical_home = Path(os.path.abspath(requested_root)).parent
    if is_system_drive(lexical_home):
        raise ValueError("maintenance home must not be on the Windows system drive")
    for candidate in (lexical_home, *lexical_home.parents):
        if candidate.exists() and is_reparse(candidate):
            raise ValueError("maintenance home and its ancestors must not be reparse points")
    if lexical_home.exists() and has_unsafe_descendant(lexical_home):
        raise ValueError("maintenance home contains an unsafe descendant")
    return lexical_home


def validate_owned_paths(requested_root: Path, requested_sentinel: Path) -> tuple[Path, Path]:
    """Validate paths without reading Git or requiring a completed sentinel."""
    validate_lexical_home(requested_root)
    lexical_root = Path(os.path.abspath(requested_root))
    lexical_sentinel = Path(os.path.abspath(requested_sentinel))
    candidates = (
        lexical_root,
        *lexical_root.parents,
        lexical_sentinel,
        *lexical_sentinel.parents,
        lexical_root / ".git",
        lexical_root / "graphify-out",
    )
    if any(is_reparse(path) for path in candidates):
        raise ValueError("maintenance paths must not be reparse points")
    root = requested_root.resolve()
    sentinel = requested_sentinel.resolve()
    if is_system_drive(root) or not root.is_dir() or not (root / ".git").is_dir():
        raise ValueError("maintenance root must be an installer-owned non-system-drive clone")
    if has_unsafe_descendant(root.parent):
        raise ValueError("owned maintenance home contains an unsafe descendant")
    if sentinel.parent != root.parent or sentinel.name != ".shaft-nightly-maintenance.json":
        raise ValueError("maintenance sentinel must be adjacent to the owned clone")
    return root, sentinel


@contextmanager
def job_lock(sentinel: Path) -> Iterator[None]:
    lock = sentinel.with_suffix(sentinel.suffix + ".lock").open("a+b")
    if lock.seek(0, os.SEEK_END) == 0:
        lock.write(b"\0")
        lock.flush()
    lock.seek(0)
    try:
        if os.name == "nt":
            import msvcrt  # pylint: disable=import-outside-toplevel
            msvcrt.locking(lock.fileno(), msvcrt.LK_NBLCK, 1)
        else:
            import fcntl  # pylint: disable=import-outside-toplevel,import-error
            fcntl.flock(lock.fileno(), fcntl.LOCK_EX | fcntl.LOCK_NB)
    except OSError as error:
        lock.close()
        raise RuntimeError("nightly SHAFT knowledge refresh is already running") from error
    try:
        yield
    finally:
        lock.seek(0)
        if os.name == "nt":
            msvcrt.locking(lock.fileno(), msvcrt.LK_UNLCK, 1)
        else:
            fcntl.flock(lock.fileno(), fcntl.LOCK_UN)
        lock.close()


def validate_owned_clone(requested_root: Path, requested_sentinel: Path) -> tuple[Path, Path]:
    root, sentinel = validate_owned_paths(requested_root, requested_sentinel)
    try:
        data = json.loads(sentinel.read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError) as error:
        raise ValueError("maintenance sentinel is absent or unreadable") from error
    if set(data) != {
        "schema_version", "repository_root", "origin", "owner_token", "trust_model"
    }:
        raise ValueError("maintenance sentinel has unexpected fields")
    token = data.get("owner_token")
    expected = {"origin": ORIGIN, "trust_model": TRUST_MODEL}
    schema_version = data.get("schema_version")
    if (
        not isinstance(schema_version, int)
        or isinstance(schema_version, bool)
        or schema_version != 1
    ):
        raise ValueError("maintenance sentinel schema is invalid")
    recorded_root = data.get("repository_root")
    same_root = isinstance(recorded_root, str) and os.path.normcase(recorded_root) == os.path.normcase(str(root))
    if not same_root or any(data.get(key) != value for key, value in expected.items()) or not isinstance(token, str) or not token:
        raise ValueError("maintenance sentinel does not own this clone")
    lines = git(["rev-parse", "--show-toplevel", "--git-common-dir"], root).splitlines()
    if len(lines) != 2 or Path(lines[0]).resolve() != root:
        raise ValueError("maintenance clone is not its primary checkout")
    common = Path(lines[1])
    if not common.is_absolute():
        common = root / common
    if common.resolve() != (root / ".git").resolve():
        raise ValueError("maintenance clone is not standalone")
    if git(["remote", "get-url", "origin"], root).strip() != ORIGIN:
        raise ValueError("maintenance clone origin is not approved")
    if git(["config", "--local", "--get", "shaft.maintenanceOwner"], root).strip() != token:
        raise ValueError("maintenance owner token does not match the clone")
    return root, sentinel


def validate_pending_receipt(pending: Path, requested_root: Path) -> dict[str, object]:
    """Validate the independent authorization written before clone creation."""
    root = requested_root.resolve()
    if is_reparse(pending):
        raise ValueError("pending receipt must not be a reparse point")
    try:
        data = json.loads(pending.read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError) as error:
        raise ValueError("pending install receipt is absent or unreadable") from error
    expected_fields = {
        "schema_version", "repository_root", "origin", "owner_token", "trust_model"
    }
    if not isinstance(data, dict) or set(data) != expected_fields:
        raise ValueError("pending install receipt has unexpected fields")
    schema_version = data["schema_version"]
    if (
        not isinstance(schema_version, int)
        or isinstance(schema_version, bool)
        or schema_version != 1
    ):
        raise ValueError("pending install receipt schema is invalid")
    recorded_root = data.get("repository_root")
    same_root = isinstance(recorded_root, str) and os.path.normcase(recorded_root) == os.path.normcase(str(root))
    token = data.get("owner_token")
    if (
        not same_root
        or data.get("origin") != ORIGIN
        or data.get("trust_model") != TRUST_MODEL
        or not isinstance(token, str)
        or re.fullmatch(r"[0-9a-f]{32}", token) is None
    ):
        raise ValueError("pending install receipt cannot authorize this clone")
    return data


def refresh(requested_root: Path, requested_sentinel: Path) -> None:
    root, sentinel = validate_owned_clone(requested_root, requested_sentinel)
    git_exe, mempalace = required("git"), required("mempalace")
    with job_lock(sentinel):
        root, sentinel = validate_owned_clone(requested_root, requested_sentinel)
        git_env = git_environment()
        rewrites = run(
            [git_exe, "config", "--local", "--get-regexp", r"^url\..*\.insteadOf$"],
            root,
            git_env,
            allowed_exit_codes=(0, 1),
        )
        if rewrites.strip():
            raise ValueError("maintenance clone must not configure Git URL rewrites")
        validate_owned_paths(requested_root, requested_sentinel)
        run([git_exe, "fetch", "--prune", "--no-tags", ORIGIN,
             "+refs/heads/main:refs/shaft-maintenance/main"], root, git_env)
        fetched = run([git_exe, "rev-parse", "refs/shaft-maintenance/main"], root, git_env).strip()
        remote = run([git_exe, "ls-remote", "--exit-code", ORIGIN, "refs/heads/main"], root, git_env).split()
        if not re.fullmatch(r"[0-9a-f]{40}", fetched) or not remote or remote[0] != fetched:
            raise RuntimeError("approved origin/main changed during fetch; retry later")
        run([git_exe, "cat-file", "-e", f"{fetched}^{{commit}}"], root, git_env)
        validate_owned_paths(requested_root, requested_sentinel)
        run([git_exe, "reset", "--hard", fetched], root, git_env)
        run([git_exe, "clean", "-ffd"], root, git_env)
        environment = git_env.copy()
        environment["SHAFT_GRAPHIFY_OUT"] = str(root / "graphify-out")
        outcomes: dict[str, str] = {}
        try:
            with trusted_graph_output(root / "graphify-out"):
                run([str(Path(sys.executable).resolve()),
                     "tools/repository-map/graphify_maintenance.py", "refresh", "--root", str(root)],
                    root, environment)
        except (OSError, ValueError, RuntimeError, subprocess.SubprocessError):
            outcomes["Graphify"] = "failed"
        else:
            outcomes["Graphify"] = "healthy"
        try:
            run([mempalace, "sync", str(root), "--wing", WING, "--apply"], root, git_env)
            mine = [
                mempalace,
                "mine",
                str(root),
                "--wing",
                WING,
                "--agent",
                "scheduled-refresh",
            ]
            run(mine, root, git_env)
            for batch in include_ignored_batches(list_promote_paths(root)):
                run([*mine, "--include-ignored", batch], root, git_env)
        except (OSError, ValueError, RuntimeError, subprocess.SubprocessError):
            outcomes["MemPalace"] = "failed"
        else:
            outcomes["MemPalace"] = "healthy"
        summary = "; ".join(f"{name}={status}" for name, status in outcomes.items())
        if "failed" in outcomes.values():
            raise RuntimeError(f"knowledge store maintenance failed: {summary}")
        print(f"SHAFT knowledge refresh complete at {fetched}")


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--root", required=True, type=Path)
    parser.add_argument("--sentinel", required=True, type=Path)
    parser.add_argument("--validate-only", action="store_true")
    parser.add_argument("--validate-paths-only", action="store_true")
    parser.add_argument("--validate-home-only", action="store_true")
    parser.add_argument("--validate-pending", type=Path)
    args = parser.parse_args()
    try:
        if args.validate_pending is not None:
            validate_pending_receipt(args.validate_pending, args.root)
        elif args.validate_home_only:
            validate_lexical_home(args.root)
        elif args.validate_paths_only:
            validate_owned_paths(args.root, args.sentinel)
        elif args.validate_only:
            validate_owned_clone(args.root, args.sentinel)
        else:
            refresh(args.root, args.sentinel)
    except (OSError, ValueError, RuntimeError, subprocess.SubprocessError) as error:
        print(str(error), file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
