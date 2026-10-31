#!/usr/bin/env python3
"""L78: create / validate reviewer scratch so delete-guard allows removal.

Prefer ``git archive`` into ``/tmp/ce-review-*`` (no linked worktree).
Alternative: detached worktree at a **pushed** sha with a clean tree.
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
    return any(
        line.strip().startswith("origin/")
        for line in contains.stdout.splitlines()
        if line.strip()
    )


def create_archive_scratch(repo: Path, sha: str, dest: Path | None = None) -> Path:
    """Extract ``git archive <sha>`` under /tmp (or *dest*)."""
    if not sha_on_origin(repo, sha):
        raise ValueError(f"sha {sha} is not on origin/*; push first (L78)")
    resolved = _git(repo, "rev-parse", "--verify", sha + "^{commit}").stdout.strip()
    short = resolved[:12]
    target = dest or Path(tempfile.mkdtemp(prefix=f"{TMP_PREFIX}{short}-", dir="/tmp"))
    target.mkdir(parents=True, exist_ok=True)
    raw = subprocess.run(  # nosec B603 B607
        ["git", "-C", str(repo), "archive", "--format=tar", resolved],
        capture_output=True, check=True,
    ).stdout
    tar_path = Path(tempfile.mkstemp(suffix=".tar")[1])
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
        "pushed": True,
    }
    (dest / ".ce-review-scratch.json").write_text(
        json.dumps(receipt, indent=2) + "\n", encoding="utf-8",
    )
    return dest


def evaluate(obs: dict[str, Any] | None) -> dict[str, Any]:
    """Fail-closed validation of a review-scratch receipt."""
    if obs is None or not isinstance(obs, dict):
        return {"ok": False, "hits": ["missing-input"], "error": "receipt required"}
    hits: list[str] = []
    kind = str(obs.get("kind") or "").strip()
    if kind not in ALLOWED_KINDS:
        hits.append("scratch-kind-not-archive-or-detached-pushed")
    if obs.get("pushed") is not True:
        hits.append("scratch-sha-not-pushed")
    path_value = obs.get("path")
    if not path_value:
        hits.append("missing-scratch-path")
    else:
        path = Path(str(path_value))
        if kind == "archive":
            # archive extracts live under /tmp (or TMPDIR)
            tmp = Path(os.environ.get("TMPDIR") or tempfile.gettempdir()).resolve()
            try:
                path.resolve().relative_to(tmp)
            except ValueError:
                hits.append("archive-scratch-not-under-tmp")
            if path.is_dir() and any(path.rglob("__pycache__")):
                hits.append("scratch-has-pycache-dirt")
        if kind == "detached-pushed" and path.is_dir() and (path / ".git").exists():
            # porcelain dirt blocks delete-guard — flag it
            status = subprocess.run(  # nosec B603 B607
                ["git", "-C", str(path), "status", "--porcelain", "--ignored"],
                capture_output=True, text=True, check=False,
            )
            if status.returncode == 0 and status.stdout.strip():
                hits.append("detached-scratch-dirty-porcelain")
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
    arch = sub.add_parser("archive", help="git archive into /tmp")
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
