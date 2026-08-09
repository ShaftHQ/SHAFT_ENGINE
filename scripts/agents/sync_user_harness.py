"""Check or deploy the source-controlled user harness without touching secrets."""

import hashlib
import json
import os
import sys
import ctypes
import errno
from collections.abc import Callable
from pathlib import Path

MANIFEST = ("CLAUDE.md", "settings.json")
RETIRED_OWNED_SETTINGS_PATHS = (
    ("model",),
    ("effortLevel",),
    ("statusLine",),
    ("permissions", "defaultMode"),
    ("extraKnownMarketplaces", "mempalace"),
    ("env", "MEMPALACE_EMBEDDING_MODEL"),
)
RETIRED_MANIFEST = Path(__file__).with_name("user_harness_retired_manifest.json")


def load_retired_targets() -> dict[str, dict[str, object]]:
    """Load the immutable, source-controlled pre-#4649 deployment inventory."""
    manifest = json.loads(RETIRED_MANIFEST.read_text(encoding="utf-8"))
    if manifest.get("schema_version") != 1 or not isinstance(manifest.get("entries"), list):
        raise ValueError(f"invalid retired user harness manifest: {RETIRED_MANIFEST}")
    targets: dict[str, dict[str, object]] = {}
    for entry in manifest["entries"]:
        policy = dict(entry)
        label = policy.pop("label")
        policy["hashes"] = set(policy.get("hashes", []))
        targets[label] = policy
    if len(targets) != len(manifest["entries"]):
        raise ValueError(f"duplicate retired target in manifest: {RETIRED_MANIFEST}")
    return targets


RETIRED_TARGETS = load_retired_targets()

class RetirementConflict(RuntimeError):
    """The target changed after preflight and was preserved instead."""

    def __init__(self, target: Path, recovery: Path | None = None):
        super().__init__(f"retired target changed during migration: {target}")
        self.target = target
        self.recovery = recovery


def repo_root() -> Path:
    return Path(__file__).resolve().parent.parent.parent


def user_claude_dir() -> Path:
    override = os.environ.get("SHAFT_USER_CLAUDE_DIR")
    return Path(override) if override else Path.home() / ".claude"


def user_agents_dir(claude_dir: Path) -> Path:
    override = os.environ.get("SHAFT_USER_AGENTS_DIR")
    return Path(override) if override else claude_dir.parent / ".agents"


def user_codex_dir(claude_dir: Path) -> Path:
    override = os.environ.get("SHAFT_USER_CODEX_DIR")
    return Path(override) if override else claude_dir.parent / ".codex"


def normalize(data: bytes) -> bytes:
    return data.replace(b"\r\n", b"\n")


def content_hash(data: bytes) -> str:
    return hashlib.sha256(normalize(data)).hexdigest()


def atomic_move_noreplace(source: Path, destination: Path) -> None:
    """Atomically move one path without replacing an existing destination."""
    if os.name == "nt":
        kernel32 = ctypes.WinDLL("kernel32", use_last_error=True)
        move_file = kernel32.MoveFileExW
        move_file.argtypes = (ctypes.c_wchar_p, ctypes.c_wchar_p, ctypes.c_uint32)
        move_file.restype = ctypes.c_int
        if move_file(str(source), str(destination), 0):
            return
        error = ctypes.get_last_error()
        if error in (80, 183):
            raise FileExistsError(error, ctypes.FormatError(error), str(destination))
        raise OSError(error, ctypes.FormatError(error), str(source))

    if sys.platform.startswith("linux"):
        libc = ctypes.CDLL(None, use_errno=True)
        renameat2 = getattr(libc, "renameat2", None)
        if renameat2 is None:
            raise OSError(errno.ENOTSUP, "renameat2(RENAME_NOREPLACE) is unavailable")
        renameat2.argtypes = (
            ctypes.c_int,
            ctypes.c_char_p,
            ctypes.c_int,
            ctypes.c_char_p,
            ctypes.c_uint,
        )
        renameat2.restype = ctypes.c_int
        if renameat2(-100, os.fsencode(source), -100, os.fsencode(destination), 1) == 0:
            return
        error = ctypes.get_errno()
        if error == errno.EEXIST:
            raise FileExistsError(error, os.strerror(error), str(destination))
        raise OSError(error, os.strerror(error), str(source))

    raise OSError(errno.ENOTSUP, "atomic no-overwrite move is unsupported on this host")


def merge_owned(existing: object, owned: object) -> object:
    """Overlay recursively-owned settings while preserving unowned keys."""
    if not isinstance(existing, dict) or not isinstance(owned, dict):
        return owned
    merged = dict(existing)
    for key, value in owned.items():
        merged[key] = merge_owned(existing.get(key), value)
    return merged


def remove_retired_owned_path(settings: dict, path: tuple[str, ...]) -> None:
    """Remove one formerly-owned key and empty retired-only parent mappings."""
    key = path[0]
    if len(path) == 1:
        settings.pop(key, None)
        return
    child = settings.get(key)
    if not isinstance(child, dict):
        return
    remove_retired_owned_path(child, path[1:])
    if not child:
        settings.pop(key, None)


def sources(root: Path, claude_dir: Path) -> dict[str, tuple[Path, Path]]:
    harness = root / ".claude/user-harness"
    return {name: (harness / name, claude_dir / name) for name in MANIFEST}


def retired_targets(
    root: Path, claude_dir: Path, agents_dir: Path, codex_dir: Path
) -> list[tuple[str, Path, Path, set[str]]]:
    """Return former managed targets with immutable ownership evidence."""
    retired = []
    for label, policy in RETIRED_TARGETS.items():
        if label.startswith("../.agents/"):
            base, relative = agents_dir, label.removeprefix("../.agents/")
        elif label.startswith("../.codex/"):
            base, relative = codex_dir, label.removeprefix("../.codex/")
        else:
            base, relative = claude_dir, label
        source_name = policy.get("source")
        source = root / source_name if isinstance(source_name, str) else root / label
        hashes = set(policy.get("hashes", set()))
        retired.append((label, source, base / relative, hashes))
    return retired


def backup_and_retire(
    target: Path, is_owned: Callable[[bytes], bool] | None = None
) -> Path:
    """Atomically move the target to an exclusive backup and verify ownership."""
    candidate = target.with_name(target.name + ".bak")
    suffix = 1
    while True:
        try:
            atomic_move_noreplace(target, candidate)
            break
        except FileExistsError:
            candidate = target.with_name(f"{target.name}.bak.{suffix}")
            suffix += 1

    if is_owned is not None and not is_owned(candidate.read_bytes()):
        try:
            # A hard link restores the moved entry only when the original path
            # is still absent; it never overwrites a second concurrent writer.
            os.link(candidate, target)
        except FileExistsError:
            raise RetirementConflict(target, candidate)
        except OSError:
            raise RetirementConflict(target, candidate)
        # Keep the atomic-move candidate on every conflict. The restored hard
        # link can be replaced immediately by another writer; the candidate is
        # the only path guaranteed to preserve the bytes we actually moved.
        raise RetirementConflict(target, candidate)
    if os.path.lexists(target):
        # Another writer recreated the live path after the atomic move. Keep
        # both paths and report the backup instead of claiming retirement.
        raise RetirementConflict(target, candidate)
    return candidate


def main() -> int:
    apply_mode = "--apply" in sys.argv[1:]
    json_mode = "--json" in sys.argv[1:]
    root = repo_root()
    claude_dir = user_claude_dir()
    agents_dir = user_agents_dir(claude_dir)
    codex_dir = user_codex_dir(claude_dir)
    manifest = sources(root, claude_dir)

    entries: list[dict[str, object]] = []

    def record(
        state: str, label: str, source: Path, target: Path, **details: str
    ) -> None:
        entries.append(
            {"state": state, "label": label, "source": str(source), "target": str(target), **details}
        )

    def finish(code: int) -> int:
        if json_mode:
            print(json.dumps({"entries": entries, "exit_code": code}))
        return code

    missing_sources = [
        (label, source, target)
        for label, (source, target) in manifest.items()
        if not source.is_file()
    ]
    if missing_sources:
        for label, source, target in missing_sources:
            record("ERROR", label, source, target)
        if not json_mode:
            print("ERROR: manifest source file(s) missing: " + ", ".join(str(source) for _, source, _ in missing_sources))
        return finish(2)

    all_in_sync = True
    hard_failure = False
    retirement_candidates = []
    for label, source, target, owned_hashes in retired_targets(
        root, claude_dir, agents_dir, codex_dir
    ):
        if not target.is_file():
            continue
        target_bytes = target.read_bytes()
        owned = content_hash(target_bytes) in owned_hashes
        if not owned:
            if not json_mode:
                print(f"CONFLICT  {label}  (unowned retired target exists: {target})")
            record("CONFLICT", label, source, target)
            hard_failure = True
            continue
        retirement_candidates.append((label, source, target, owned_hashes))

    # A collision anywhere makes the migration read-only. This protects a mixed
    # profile from partial retirement or deployment before the user resolves it.
    if hard_failure:
        return finish(2)

    for label, source, target, owned_hashes in retirement_candidates:
        all_in_sync = False
        details = {}
        if apply_mode:
            def still_owned(data: bytes) -> bool:
                return content_hash(data) in owned_hashes

            try:
                backup = backup_and_retire(target, still_owned)
            except RetirementConflict as conflict:
                if not json_mode:
                    print(f"CONFLICT  {label}  ({conflict})")
                    if conflict.recovery is not None:
                        print(f"  -> recovery preserved at {conflict.recovery}")
                conflict_details = {}
                if conflict.recovery is not None:
                    conflict_details["recovery"] = str(conflict.recovery)
                record("CONFLICT", label, source, target, **conflict_details)
                return finish(2)
            details["backup"] = str(backup)
            if not json_mode:
                print(f"RETIRED  {label}  (managed user-level target: {target})")
                print(f"  -> retired to {backup}")
        elif not json_mode:
            print(f"RETIRED  {label}  (managed user-level target: {target})")
        record("RETIRED", label, source, target, **details)

    for label, (source, target) in manifest.items():
        source_bytes = source.read_bytes()
        if not target.is_file():
            details = {}
            if not json_mode:
                print(f"MISSING  {label}  (target absent: {target})")
            all_in_sync = False
            if apply_mode:
                target.parent.mkdir(parents=True, exist_ok=True)
                target.write_bytes(source_bytes)
                if not json_mode:
                    print(f"  -> deployed {target}")
                details["deployed"] = str(target)
            record("MISSING", label, source, target, **details)
            continue

        target_bytes = target.read_bytes()
        desired_bytes = source_bytes
        if label == "settings.json":
            try:
                owned = json.loads(source_bytes)
                existing = json.loads(target_bytes)
            except (UnicodeDecodeError, json.JSONDecodeError):
                if not json_mode:
                    print(f"INVALID  {label}  (target is not valid JSON: {target})")
                record("INVALID", label, source, target)
                all_in_sync = False
                hard_failure = True
                continue
            desired = merge_owned(existing, owned)
            for retired_path in RETIRED_OWNED_SETTINGS_PATHS:
                remove_retired_owned_path(desired, retired_path)
            desired_bytes = (json.dumps(desired, indent=2) + "\n").encode("utf-8")

        if normalize(desired_bytes) == normalize(target_bytes):
            if not json_mode:
                print(f"IN-SYNC  {label}")
            record("IN-SYNC", label, source, target)
            continue

        all_in_sync = False
        if not json_mode:
            print(f"DRIFTED  {label}  (differs from {target})")
        details = {}
        if apply_mode:
            backup = target.with_name(target.name + ".bak")
            if backup.exists():
                if not json_mode:
                    print(f"  -> preserved existing backup {backup}")
            else:
                backup.write_bytes(target_bytes)
                if not json_mode:
                    print(f"  -> backed up to {backup}")
            details["backup"] = str(backup)
            target.write_bytes(desired_bytes)
            if not json_mode:
                print(f"  -> deployed {target}")
            details["deployed"] = str(target)
        record("DRIFTED", label, source, target, **details)

    return finish(0 if apply_mode or all_in_sync else 1)


if __name__ == "__main__":
    raise SystemExit(main())
