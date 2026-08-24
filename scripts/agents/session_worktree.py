#!/usr/bin/env python3
"""Session-scoped primary checkout setup and worktree teardown (#5325)."""

from __future__ import annotations

import hashlib
import json
import os
import subprocess  # nosec B404 - fixed git commands, never a shell.
from contextlib import contextmanager
from datetime import UTC, datetime
from pathlib import Path

SCHEMA_VERSION = 1
GIT_TIMEOUT_SECONDS = 30
FETCH_TIMEOUT_SECONDS = 60
SESSION_ID_MAX = 32

_LOCK_EXCL = os.O_CREAT | os.O_RDWR


def sanitize_session_id(session_id: str) -> str:
    """Return a filesystem-safe session id, hashed when the host id is long."""
    raw = "".join(
        character if character.isalnum() or character in "-_" else "-"
        for character in str(session_id or "").strip()
    ).strip("-_") or "unknown"
    if len(raw) <= SESSION_ID_MAX:
        return raw
    digest = hashlib.sha256(str(session_id).encode("utf-8")).hexdigest()[:12]
    return f"{raw[:19].rstrip('-_')}-{digest}"


def _git(cwd: Path, *arguments: str, timeout: int = GIT_TIMEOUT_SECONDS) -> str | None:
    try:
        completed = subprocess.run(  # nosec B603 B607 - fixed git argv.
            ["git", "-c", "core.longpaths=true", *arguments],
            cwd=str(cwd),
            capture_output=True,
            text=True,
            timeout=timeout,
            check=False,
        )
    except (OSError, subprocess.SubprocessError):
        return None
    if completed.returncode != 0:
        return None
    return completed.stdout or ""


def git_common_dir(cwd: Path) -> Path | None:
    rendered = _git(cwd, "rev-parse", "--git-common-dir")
    if rendered is None:
        return None
    path = Path(rendered.strip())
    if not path.is_absolute():
        path = cwd / path
    try:
        return path.resolve()
    except OSError:
        return None


def primary_checkout(cwd: Path) -> Path | None:
    listing = _git(cwd, "worktree", "list", "--porcelain")
    if listing is None:
        return None
    for line in listing.splitlines():
        if line.startswith("worktree "):
            try:
                return Path(line.split(" ", 1)[1]).resolve()
            except OSError:
                return None
    try:
        return cwd.resolve()
    except OSError:
        return None


def default_upstream(cwd: Path) -> str | None:
    symbolic = _git(cwd, "symbolic-ref", "--quiet", "refs/remotes/origin/HEAD")
    if symbolic:
        value = symbolic.strip()
        if value.startswith("refs/remotes/"):
            return value.removeprefix("refs/remotes/")
    verify = _git(cwd, "rev-parse", "--verify", "--quiet", "refs/remotes/origin/main")
    return "origin/main" if verify is not None else None


def current_branch(cwd: Path) -> str | None:
    name = (_git(cwd, "rev-parse", "--abbrev-ref", "HEAD") or "").strip()
    return name if name and name != "HEAD" else None


def uncommitted_count(cwd: Path) -> int | None:
    rendered = _git(cwd, "status", "--porcelain")
    if rendered is None:
        return None
    return len([line for line in rendered.splitlines() if line.strip()])


def _is_ancestor(cwd: Path, commit: str, upstream: str) -> bool | None:
    try:
        completed = subprocess.run(  # nosec B603 B607 - fixed git argv.
            ["git", "-c", "core.longpaths=true", "merge-base", "--is-ancestor", commit, upstream],
            cwd=str(cwd),
            capture_output=True,
            text=True,
            timeout=GIT_TIMEOUT_SECONDS,
            check=False,
        )
    except (OSError, subprocess.SubprocessError):
        return None
    if completed.returncode == 0:
        return True
    if completed.returncode == 1:
        return False
    return None


def unique_commit_count(cwd: Path, upstream: str) -> int | None:
    rendered = _git(cwd, "rev-list", "--count", f"{upstream}..HEAD")
    if rendered is None:
        return None
    try:
        return int(rendered.strip() or "0")
    except ValueError:
        return None


def session_dir(cwd: Path) -> Path | None:
    common = git_common_dir(cwd)
    return None if common is None else common / "chaos-engine" / "sessions"


def manifest_path(cwd: Path, session_id: str) -> Path | None:
    folder = session_dir(cwd)
    return None if folder is None else folder / f"{sanitize_session_id(session_id)}.json"


def load_manifest(cwd: Path, session_id: str) -> dict | None:
    path = manifest_path(cwd, session_id)
    if path is None or not path.is_file():
        return None
    try:
        payload = json.loads(path.read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError, UnicodeError):
        return None
    if payload.get("schemaVersion") != SCHEMA_VERSION or not isinstance(payload, dict):
        return None
    return payload


def save_manifest(cwd: Path, payload: dict) -> None:
    path = manifest_path(cwd, str(payload.get("sessionId") or ""))
    if path is None:
        return
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(payload, indent=2, sort_keys=True) + "\n", encoding="utf-8")


def _lock(cwd: Path):
    common = git_common_dir(cwd)
    if common is None:
        return None
    lock_path = common / "chaos-engine" / "session-setup.lock"
    lock_path.parent.mkdir(parents=True, exist_ok=True)
    try:
        handle = os.open(str(lock_path), _LOCK_EXCL, 0o644)
    except OSError:
        return None
    try:
        if os.name == "nt":
            import msvcrt

            msvcrt.locking(handle, msvcrt.LK_NBLCK, 1)
        else:
            import fcntl

            fcntl.flock(handle, fcntl.LOCK_EX | fcntl.LOCK_NB)
    except OSError:
        os.close(handle)
        return None
    return handle


def _unlock(handle) -> None:
    if handle is None:
        return
    try:
        os.close(handle)
    except OSError:
        return


@contextmanager
def _session_lock(cwd: Path):
    handle = _lock(cwd)
    try:
        yield handle
    finally:
        _unlock(handle)


def _relative_to(path: Path, root: Path) -> bool:
    try:
        path.resolve().relative_to(root.resolve())
        return True
    except (OSError, ValueError):
        return False


def worktree_path_for(primary: Path, session_id: str) -> Path:
    return primary.parent / f"{primary.name}.session-{sanitize_session_id(session_id)}"


def _live_worktree(path: Path) -> bool:
    return path.is_dir() and (path / ".git").exists()


def prepare_session(
    cwd: Path,
    session_id: str,
    *,
    source: str = "startup",
) -> dict:
    """Prepare primary default and one sibling session worktree, or explain why not."""
    try:
        cwd = Path(cwd).resolve()
    except OSError:
        return {"status": "skipped", "message": "Session worktree setup skipped: cwd is unreadable."}
    with _session_lock(cwd) as handle:
        if handle is None:
            return {"status": "skipped", "message": "Session worktree setup skipped: checkout is locked or unverifiable."}
        return _prepare_locked(cwd, session_id, source)


def _prepare_locked(cwd: Path, session_id: str, source: str) -> dict:
    existing = load_manifest(cwd, session_id)
    if existing and _live_worktree(Path(str(existing.get("worktreePath") or ""))):
        path = existing["worktreePath"]
        return {
            "status": "reused",
            "message": f"Session worktree reused at `{path}`. Do all task work there.",
            "worktreePath": path,
            "primaryRoot": existing.get("primaryRoot"),
        }
    if source in {"resume", "compact"} and existing:
        return {
            "status": "skipped",
            "message": "Session worktree missing on resume; inspect the checkout before recreating it.",
        }
    primary = primary_checkout(cwd)
    if primary is None:
        return {"status": "skipped", "message": "Session worktree setup skipped: primary checkout could not be identified."}
    _reap_merged(primary)
    remotes = (_git(primary, "remote") or "").split()
    if remotes:
        _git(primary, "fetch", "--prune", remotes[0], timeout=FETCH_TIMEOUT_SECONDS)
    synced = _sync_primary_default(primary)
    if synced.get("status") != "ok":
        return synced
    upstream = synced["upstream"]
    target = worktree_path_for(primary, session_id)
    attached = _attach_session_worktree(primary, target, upstream)
    if attached.get("status") != "ok":
        return attached
    head = attached["head"]
    payload = {
        "schemaVersion": SCHEMA_VERSION,
        "sessionId": sanitize_session_id(session_id),
        "primaryRoot": str(primary),
        "worktreePath": str(target),
        "ownedBranch": None,
        "expectedHead": head,
        "createdAt": datetime.now(UTC).isoformat(),
        "merged": False,
        "mergedBranch": None,
        "mergedHead": None,
    }
    save_manifest(primary, payload)
    return {
        "status": "created",
        "message": (
            f"Session worktree ready at `{target}`. Do all planning and implementation there. "
            "The primary default branch stays at the fetched upstream tip."
        ),
        "worktreePath": str(target),
        "primaryRoot": str(primary),
    }


def record_merge(cwd: Path, session_id: str, *, branch: str | None = None, head: str | None = None) -> None:
    payload = load_manifest(cwd, session_id)
    if payload is None:
        return
    payload["merged"] = True
    if branch:
        payload["mergedBranch"] = branch
        payload["ownedBranch"] = branch
    if head:
        payload["mergedHead"] = head
        payload["expectedHead"] = head
    save_manifest(cwd, payload)


def teardown_session(cwd: Path, session_id: str) -> dict:
    """Remove this session worktree only after merge is recorded and the tree is clean."""
    payload = load_manifest(cwd, session_id)
    if payload is None:
        return {"status": "skipped", "message": "No session worktree manifest."}
    if not payload.get("merged"):
        return {"status": "kept", "message": "Session worktree kept: merge is not recorded."}
    return _remove_owned_worktree(cwd, payload)


def _remove_owned_worktree(cwd: Path, payload: dict) -> dict:
    target = Path(str(payload.get("worktreePath") or ""))
    primary = Path(str(payload.get("primaryRoot") or cwd))
    if not _live_worktree(target):
        return {"status": "absent", "message": "Session worktree already gone."}
    dirty = uncommitted_count(target)
    if dirty:
        return {
            "status": "kept",
            "message": f"Session worktree kept: {dirty} uncommitted file(s) remain.",
        }
    removed = _git(primary, "worktree", "remove", "--", str(target))
    if removed is None:
        return {"status": "kept", "message": "Session worktree kept: git worktree remove refused."}
    _reset_primary_default(primary)
    return {"status": "removed", "message": f"Removed session worktree `{target}`."}


def _sync_primary_default(primary: Path) -> dict:
    upstream = default_upstream(primary)
    if upstream is None:
        return {"status": "skipped", "message": "Session worktree setup skipped: configured upstream is missing."}
    default_branch = upstream.split("/", 1)[-1]
    branch = current_branch(primary)
    dirty = uncommitted_count(primary)
    if dirty is None:
        return {"status": "skipped", "message": "Session worktree setup skipped: git status is unverifiable."}
    if branch is not None and branch != default_branch and dirty > 0:
        return {
            "status": "halted",
            "message": (
                f"Primary checkout is on leftover branch `{branch}` with {dirty} uncommitted "
                "file(s). Preserve that work; SessionStart will not discard it or create a "
                "session worktree."
            ),
        }
    unique = unique_commit_count(primary, upstream)
    if unique is None:
        return {"status": "skipped", "message": "Session worktree setup skipped: unique commits are unverifiable."}
    if unique > 0:
        return {
            "status": "halted",
            "message": (
                f"Primary checkout carries {unique} commit(s) not on `{upstream}`. "
                "Unique commits are preserved; SessionStart will not reset them."
            ),
        }
    if branch != default_branch and _git(primary, "checkout", "-q", default_branch) is None:
        return {"status": "skipped", "message": "Session worktree setup skipped: could not check out the default branch."}
    if _git(primary, "reset", "--hard", upstream) is None:
        return {"status": "skipped", "message": "Session worktree setup skipped: could not reset the default branch."}
    _git(primary, "clean", "-fd")
    return {"status": "ok", "upstream": upstream}


def _attach_session_worktree(primary: Path, target: Path, upstream: str) -> dict:
    if _live_worktree(target):
        return {"status": "ok", "head": (_git(target, "rev-parse", "HEAD") or "").strip()}
    if target.exists():
        return {"status": "skipped", "message": f"Session worktree path `{target}` exists and is not a worktree."}
    added = _git(primary, "worktree", "add", "--detach", "--", str(target), upstream)
    if added is None:
        return {"status": "skipped", "message": "Session worktree setup skipped: git worktree add failed."}
    return {"status": "ok", "head": (_git(target, "rev-parse", "HEAD") or "").strip()}


def _reset_primary_default(primary: Path) -> None:
    upstream = default_upstream(primary)
    branch = current_branch(primary)
    if upstream is None or branch is None:
        return
    default_branch = upstream.split("/", 1)[-1]
    if branch != default_branch:
        return
    if uncommitted_count(primary) is None:
        return
    unique = unique_commit_count(primary, upstream)
    if unique:
        return
    _git(primary, "reset", "--hard", upstream)
    _git(primary, "clean", "-fd")


def _reap_merged(primary: Path) -> None:
    folder = session_dir(primary)
    if folder is None or not folder.is_dir():
        return
    upstream = default_upstream(primary)
    for path in folder.glob("*.json"):
        try:
            payload = json.loads(path.read_text(encoding="utf-8"))
        except (OSError, json.JSONDecodeError, UnicodeError):
            continue
        if payload.get("schemaVersion") != SCHEMA_VERSION:
            continue
        target = Path(str(payload.get("worktreePath") or ""))
        if not _live_worktree(target):
            continue
        merged = bool(payload.get("merged"))
        ancestor = False
        if upstream:
            head = (_git(target, "rev-parse", "HEAD") or "").strip()
            ancestor = bool(head) and _is_ancestor(primary, head, upstream) is True
        if merged or ancestor:
            _remove_owned_worktree(primary, payload)


def isolation_denial(
    *,
    cwd: Path | str | None,
    session_id: str,
    mutation: bool,
    workdir: str | None,
    targets: tuple[str, ...],
) -> str | None:
    """Deny mutations of the primary checkout while a session worktree is active."""
    if not mutation or not session_id:
        return None
    try:
        origin = Path(cwd or ".").resolve()
    except OSError:
        return None
    payload = load_manifest(origin, session_id)
    if payload is None:
        return None
    try:
        worktree = Path(str(payload["worktreePath"])).resolve()
        primary = Path(str(payload["primaryRoot"])).resolve()
    except (OSError, KeyError):
        return None
    common = git_common_dir(origin)
    allowed_roots = [worktree]
    if common is not None:
        allowed_roots.append(common)

    def allowed(path: str) -> bool:
        candidate = Path(path)
        if not candidate.is_absolute():
            base = Path(workdir).resolve() if workdir else origin
            candidate = base / candidate
        try:
            resolved = candidate.resolve()
        except OSError:
            return False
        return any(_relative_to(resolved, root) for root in allowed_roots)

    effective_dir = Path(workdir).resolve() if workdir else origin
    if _relative_to(effective_dir, worktree):
        if targets and not all(allowed(target) for target in targets):
            return _isolation_reason(worktree)
        return None
    if not targets:
        if _relative_to(effective_dir, primary):
            return _isolation_reason(worktree)
        return None
    if not all(allowed(target) for target in targets):
        return _isolation_reason(worktree)
    return None


def _isolation_reason(worktree: Path) -> str:
    return (
        "Session isolation: mutate files inside the session worktree "
        f"`{worktree}`, not the primary checkout."
    )


def format_context(result: dict) -> str:
    message = str(result.get("message") or "").strip()
    path = result.get("worktreePath")
    if path and result.get("status") in {"created", "reused"}:
        return message
    return message
