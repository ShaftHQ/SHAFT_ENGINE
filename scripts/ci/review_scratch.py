#!/usr/bin/env python3
"""L78: create / validate reviewer scratch so delete-guard allows removal.

Prefer ``git archive`` into OS temp (``tempfile.mkdtemp``) — no linked worktree.
Alternative: detached worktree at a **pushed** sha with a clean tree.

``evaluate`` re-verifies the sha is on ``origin/*`` (not an honor-system flag),
requires the path to exist, requires ``.git`` for detached scratch, and rejects
archive paths under the workspace root.
"""
from __future__ import annotations

import argparse
import json
import os
import subprocess
import tarfile
import tempfile
from pathlib import Path
from typing import Any

ALLOWED_KINDS = frozenset({"archive", "detached-pushed"})
TMP_PREFIX = "ce-review-"


def _git(repo: Path, *args: str, check: bool = True) -> subprocess.CompletedProcess[str]:
    return subprocess.run(  # nosec B603 B607
        ["git", "-C", str(repo), *args],
        capture_output=True,
        text=True,
        check=check,
    )


def sha_on_origin(repo: Path, sha: str) -> bool:
    """True when *sha* is reachable from any origin/* ref (pushed)."""
    full = sha.strip()
    if len(full) < 7:
        return False
    tip = _git(repo, "rev-parse", "--verify", full + "^{commit}", check=False)
    if tip.returncode != 0:
        return False
    resolved = tip.stdout.strip()
    contains = _git(
        repo, "branch", "-r", "--contains", resolved, check=False,
    )
    if contains.returncode != 0:
        return False
    if any(
        line.strip().startswith("origin/")
        for line in contains.stdout.splitlines()
        if line.strip()
    ):
        return True
    # Fallback: ls-remote tip equality for bare SHAs not yet in local remote-tracking.
    remote = _git(repo, "ls-remote", "--heads", "origin", check=False)
    if remote.returncode != 0:
        return False
    return any(
        line.split()[0].startswith(resolved) or resolved.startswith(line.split()[0])
        for line in remote.stdout.splitlines()
        if line.strip()
    )


def canonical_temp_root() -> Path:
    """OS temp root (realpath). Must not resolve under the workspace."""
    return Path(tempfile.gettempdir()).resolve()


def workspace_root() -> Path | None:
    """Best-effort workspace root to reject as archive scratch."""
    for key in ("CURSOR_WORKSPACE", "CHAOS_ENGINE_WORKSPACE"):
        raw = os.environ.get(key, "").strip()
        if raw:
            try:
                return Path(raw).resolve()
            except OSError:
                # Invalid env path — try the next candidate.
                pass
    # Box convention (scripts/ci only — not portable core).
    candidate = Path("/workspace")
    if candidate.is_dir():
        try:
            return candidate.resolve()
        except OSError:
            return None
    return None


def _under(path: Path, root: Path) -> bool:
    try:
        path.resolve().relative_to(root.resolve())
        return True
    except (ValueError, OSError):
        return False


def create_archive_scratch(repo: Path, sha: str, dest: Path | None = None) -> Path:
    """Extract ``git archive <sha>`` under OS temp (or *dest* if already allowed)."""
    if not sha_on_origin(repo, sha):
        raise ValueError(f"sha {sha} is not on origin/*; push first (L78)")
    resolved = _git(repo, "rev-parse", "--verify", sha + "^{commit}").stdout.strip()
    short = resolved[:12]
    if dest is None:
        target = Path(tempfile.mkdtemp(prefix=f"{TMP_PREFIX}{short}-"))
    else:
        target = Path(dest)
        target.mkdir(parents=True, exist_ok=True)
    raw = subprocess.run(  # nosec B603 B607
        ["git", "-C", str(repo), "archive", "--format=tar", resolved],
        capture_output=True, check=True,
    ).stdout
    fd, tar_name = tempfile.mkstemp(suffix=".tar")
    os.close(fd)
    tar_path = Path(tar_name)
    try:
        tar_path.write_bytes(raw)
        with tarfile.open(tar_path, "r:") as tar:
            tar.extractall(target)  # nosec B202 - trusted own git archive
    finally:
        tar_path.unlink(missing_ok=True)
    receipt = {
        "kind": "archive",
        "sha": resolved,
        "path": str(target),
        "repo": str(Path(repo).resolve()),
        "pushed": True,
    }
    (target / ".ce-review-scratch.json").write_text(
        json.dumps(receipt, indent=2) + "\n", encoding="utf-8",
    )
    return target


def create_detached_scratch(repo: Path, sha: str, dest: Path) -> Path:
    """``git worktree add --detach`` at a pushed sha (no local commits ahead)."""
    if not sha_on_origin(repo, sha):
        raise ValueError(f"sha {sha} is not on origin/*; push first (L78)")
    resolved = _git(repo, "rev-parse", "--verify", sha + "^{commit}").stdout.strip()
    dest = Path(dest)
    if dest.exists():
        raise ValueError(f"dest exists: {dest}")
    _git(repo, "worktree", "add", "--detach", str(dest), resolved)
    receipt = {
        "kind": "detached-pushed",
        "sha": resolved,
        "path": str(dest),
        "repo": str(Path(repo).resolve()),
        "pushed": True,
    }
    (dest / ".ce-review-scratch.json").write_text(
        json.dumps(receipt, indent=2) + "\n", encoding="utf-8",
    )
    return dest


def _hits_archive_path(path: Path) -> list[str]:
    """Archive scratch must exist under OS temp, never under the workspace."""
    hits: list[str] = []
    if not path.is_dir():
        return ["archive-scratch-not-dir"]
    tmp = canonical_temp_root()
    ws = workspace_root()
    if ws is not None and _under(tmp, ws):
        hits.append("archive-temp-root-under-workspace")
    elif ws is not None and _under(path, ws):
        hits.append("archive-scratch-under-workspace")
    else:
        try:
            path.resolve().relative_to(tmp)
        except ValueError:
            hits.append("archive-scratch-not-under-tmp")
    if any(path.rglob("__pycache__")):
        hits.append("scratch-has-pycache-dirt")
    return hits


def _hits_detached_path(path: Path) -> list[str]:
    """Detached scratch needs a live .git and a clean porcelain tree."""
    if not path.is_dir():
        return ["detached-scratch-not-dir"]
    if not (path / ".git").exists():
        return ["detached-scratch-missing-git"]
    status = subprocess.run(  # nosec B603 B607
        ["git", "-C", str(path), "status", "--porcelain", "--ignored"],
        capture_output=True, text=True, check=False,
    )
    if status.returncode == 0 and status.stdout.strip():
        return ["detached-scratch-dirty-porcelain"]
    return []


def _hits_pushed_sha(obs: dict[str, Any]) -> list[str]:
    repo_raw = obs.get("repo")
    sha = str(obs.get("sha") or "").strip()
    if not repo_raw or not sha:
        return ["scratch-missing-repo-or-sha"]
    if not sha_on_origin(Path(str(repo_raw)), sha):
        return ["scratch-sha-not-on-origin"]
    return []


def evaluate(obs: dict[str, Any] | None) -> dict[str, Any]:
    """Fail-closed validation of a review-scratch receipt.

    Pushed status is re-verified via ``git branch -r --contains`` / ``ls-remote``,
    never trusted from a receipt flag alone.
    """
    if obs is None or not isinstance(obs, dict):
        return {"ok": False, "hits": ["missing-input"], "error": "receipt required"}
    hits: list[str] = []
    kind = str(obs.get("kind") or "").strip()
    if kind not in ALLOWED_KINDS:
        hits.append("scratch-kind-not-archive-or-detached-pushed")
    hits.extend(_hits_pushed_sha(obs))

    path_value = obs.get("path")
    if not path_value:
        hits.append("missing-scratch-path")
    else:
        path = Path(str(path_value))
        if not path.exists():
            hits.append("scratch-path-missing")
        elif kind == "archive":
            hits.extend(_hits_archive_path(path))
        elif kind == "detached-pushed":
            hits.extend(_hits_detached_path(path))

    if obs.get("unpushed_tip") is True or obs.get("local_only_branch") is True:
        hits.append("unpushed-tip-forbidden")
    return {
        "ok": not hits,
        "hits": hits,
        "error": None if not hits else "L78 review-scratch gate failed",
    }


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    sub = parser.add_subparsers(dest="cmd", required=True)
    arch = sub.add_parser("archive", help="git archive into OS temp")
    arch.add_argument("--repo", required=True)
    arch.add_argument("--sha", required=True)
    arch.add_argument("--dest", default="")
    det = sub.add_parser("detach", help="detached worktree at pushed sha")
    det.add_argument("--repo", required=True)
    det.add_argument("--sha", required=True)
    det.add_argument("--dest", required=True)
    check = sub.add_parser("check", help="evaluate a receipt JSON")
    check.add_argument("receipt")
    ns = parser.parse_args(argv)
    if ns.cmd == "archive":
        path = create_archive_scratch(
            Path(ns.repo), ns.sha, Path(ns.dest) if ns.dest else None,
        )
        print(path)
        return 0
    if ns.cmd == "detach":
        path = create_detached_scratch(Path(ns.repo), ns.sha, Path(ns.dest))
        print(path)
        return 0
    obs = json.loads(Path(ns.receipt).read_text(encoding="utf-8"))
    result = evaluate(obs)
    print(json.dumps(result, indent=2))
    return 0 if result["ok"] else 1


if __name__ == "__main__":
    raise SystemExit(main())
