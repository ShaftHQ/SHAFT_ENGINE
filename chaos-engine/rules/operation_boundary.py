"""Operation-boundary guards for push, remotes, deletes, and secret-bearing output.

Callers pass the real command and the project root. Approval is a file on disk,
not a boolean the caller asserts.
"""

from __future__ import annotations

import importlib.util
import json
import re
import sys
from pathlib import Path
from typing import Any

_FORCE = re.compile(r"(--force\b|--force-with-lease\b|(?:^|\s)-f(?:\s|$)|\s\+\S+)")
_REMOTE_ADD = re.compile(r"\bgit\s+remote\s+add\b")
_DELETE = re.compile(r"\b(?:delete|cleanup)\b", re.I)
_DELETE_CODE = re.compile(r"(?:--delete-code|--code)\s+\S+|DELETE_CODE=\S+", re.I)
_PUSH = re.compile(r"\bgit\s+push\b")


def _load(path: Path, name: str) -> Any:
    spec = importlib.util.spec_from_file_location(name, path)
    if spec is None or spec.loader is None:
        raise ImportError(f"cannot load {path}")
    module = importlib.util.module_from_spec(spec)
    sys.modules[name] = module
    spec.loader.exec_module(module)
    return module


def _rules() -> Any:
    return _load(Path(__file__).resolve().with_name("lessons_2026_11_06.py"), "ce_boundary_lessons")


def _pdca() -> Any:
    return _load(Path(__file__).resolve().with_name("lessons_repeat_failure_pdca.py"), "ce_boundary_pdca")


def _normalized(command: str) -> str:
    return " ".join((command or "").split())


def force_push_block(root: Path, command: str) -> str | None:
    text = _normalized(command)
    if not _PUSH.search(text) or not _FORCE.search(text):
        return None
    path = Path(root) / ".chaos-engine-state" / "force-push-approval.txt"
    recorded: set[str] = set()
    if path.is_file():
        recorded = {line.strip() for line in path.read_text(encoding="utf-8").splitlines() if line.strip()}
    if text not in recorded:
        return "force-push-without-approval"
    return None


def private_remote_block(root: Path, command: str) -> str | None:
    if not _REMOTE_ADD.search(command or ""):
        return None
    marker = Path(root) / ".chaos-engine-state" / "private-working-copy"
    if marker.is_file():
        return "remote-on-private-copy"
    return None


def relayed_delete_block(command: str) -> str | None:
    """A delete code inside the agent command was relayed, not typed by the human."""
    text = command or ""
    if _DELETE.search(text) and _DELETE_CODE.search(text):
        return "relayed-delete-code"
    return None


def secret_failures(text: str, *, kind: str = "output") -> list[str]:
    result = _rules().evaluate_secrets_text(text or "", kind=kind)
    return list(result.get("hits") or [])


def file_secret_failures(paths: list[Path]) -> list[str]:
    hits: list[str] = []
    for path in paths:
        if not path.is_file():
            continue
        posix = path.as_posix()
        if "/tests/" in f"/{posix}" or posix.startswith("tests/"):
            continue
        try:
            if path.stat().st_size > 1_000_000:
                continue
            text = path.read_text(encoding="utf-8")
        except (OSError, UnicodeDecodeError):
            continue
        hits.extend(secret_failures(text, kind="file"))
    return hits


def pdca_push_failures(root: Path) -> list[str]:
    """A second consecutive stage failure blocks push until that stage's note exists."""
    state = Path(root) / ".chaos-engine-state" / "repeat-failure-pdca" / "failures.json"
    if not state.is_file():
        return []
    try:
        payload = json.loads(state.read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError):
        return ["pdca-state-unreadable"]
    stages = payload.get("stages") if isinstance(payload, dict) else None
    if not isinstance(stages, dict):
        return ["pdca-state-unreadable"]
    return _pdca_stage_hits(Path(root), stages)


def _pdca_stage_hits(root: Path, stages: dict[str, Any]) -> list[str]:
    notes = _pdca()
    hits: list[str] = []
    for stage, count in stages.items():
        try:
            consecutive = int(count)
        except (TypeError, ValueError):
            continue
        if consecutive < 2:
            continue
        note_path = root / ".chaos-engine-state" / "repeat-failure-pdca" / f"{stage}.json"
        note = None
        if note_path.is_file():
            try:
                loaded = json.loads(note_path.read_text(encoding="utf-8"))
            except (OSError, json.JSONDecodeError):
                loaded = None
            note = loaded if isinstance(loaded, dict) else None
        if not notes.note_covers_stage(note, str(stage)):
            hits.append(f"push-without-rca:{stage}")
    return hits


def command_blocks(root: Path, command: str) -> list[str]:
    """Guards that read the command about to run and the files already on disk."""
    hits: list[str] = []
    for item in (
        force_push_block(root, command),
        private_remote_block(root, command),
        relayed_delete_block(command),
    ):
        if item:
            hits.append(item)
    hits.extend(secret_failures(command, kind="command"))
    return hits


def _watcher_loop_failures(root: Path, changed: list[str]) -> list[str]:
    interesting = [relative for relative in changed if relative.replace("\\", "/").endswith("watch_pr_checks.py")]
    if not interesting:
        return []
    watcher = _load(
        Path(__file__).resolve().parents[2] / "scripts" / "agents" / "watch_pr_checks.py",
        "ce_boundary_watch",
    )
    hits: list[str] = []
    for relative in interesting:
        normal = relative.replace("\\", "/")
        if not normal.endswith("watch_pr_checks.py"):
            continue
        path = Path(root) / relative
        if not path.is_file():
            continue
        try:
            text = path.read_text(encoding="utf-8")
        except (OSError, UnicodeDecodeError):
            continue
        for item in watcher.preflight_unbounded_loops(text):
            hits.append(f"watcher:{item}")
    return hits


def disk_blocks(root: Path, changed: list[str] | None = None) -> list[str]:
    """Push-time evidence: PDCA notes, secret-bearing files, bounded watcher loops."""
    hits = list(pdca_push_failures(root))
    paths = [Path(root) / relative for relative in (changed or [])]
    hits.extend(file_secret_failures(paths))
    hits.extend(_watcher_loop_failures(root, list(changed or [])))
    return hits
