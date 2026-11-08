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

# `set +e` is a shell option. A force refspec is a plus argument that does not follow `set`.
_FORCE = re.compile(r"(--force\b|--force-with-lease\b|(?:^|\s)-f(?:\s|$)|(?<!set)\s\+\S+)")
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


_STAGE = re.compile(r"\b(?:pytest|unittest)\b|\bmvn\b[^\n]*\btest\b|\bnpm\b[^\n]*\btest\b")
_STAGE_FLAG = re.compile(r"--stage(?:=|\s+)([A-Za-z0-9_.-]+)")
_EMPTY_TREE = "4b825dc642cb6eb9a060e54bf8d69288fbee4904"
# #6216: origin/HEAD is the configured default. These names are only a fallback
# when that symref was never written, which a fetch-only checkout often skips.
_REMOTE_BRANCH_FALLBACKS = ("main", "master")


def stage_of_command(command: str) -> str | None:
    flagged = _STAGE_FLAG.search(command or "")
    if flagged:
        return flagged.group(1)
    if _STAGE.search(command or ""):
        return "test"
    return None


def _pdca_dir(root: Path) -> Path:
    return Path(root) / ".chaos-engine-state" / "repeat-failure-pdca"


def _replay_events(path: Path) -> dict[str, int]:
    counts: dict[str, int] = {}
    for line in path.read_text(encoding="utf-8").splitlines():
        if not line.strip():
            continue
        item = json.loads(line)
        if not isinstance(item, dict):
            continue
        name = str(item.get("stage") or "").strip()
        if not name:
            continue
        counts[name] = counts.get(name, 0) + 1 if item.get("failed") is True else 0
    return counts


def record_stage_event(root: Path, stage: str, *, failed: bool) -> str | None:
    """Append one observed failure or reset. The events file is the source of truth."""
    folder = _pdca_dir(root)
    try:
        folder.mkdir(parents=True, exist_ok=True)
        events = folder / "events.jsonl"
        with events.open("a", encoding="utf-8") as handle:
            handle.write(json.dumps({"stage": stage, "failed": bool(failed)}) + "\n")
        counts = _replay_events(events)
        (folder / "failures.json").write_text(json.dumps({"stages": counts}), encoding="utf-8")
    except (OSError, json.JSONDecodeError, TypeError, ValueError):
        return "pdca-state-unavailable"
    return None


def observe_commands(root: Path, commands: list[str] | tuple[str, ...], *, failed: bool) -> str | None:
    """Persist a stage event for a command the host actually ran."""
    for command in commands:
        stage = stage_of_command(command)
        if not stage:
            continue
        problem = record_stage_event(root, stage, failed=failed)
        if problem:
            return problem
    return None


def output_secret_block(result: object) -> str | None:
    """Scan tool stdout/stderr/output. The command string is not this evidence."""
    if not isinstance(result, dict):
        return None
    text = "\n".join(str(result.get(key) or "") for key in ("stdout", "stderr", "output"))
    hits = secret_failures(text, kind="output") if text.strip() else []
    return hits[0] if hits else None


def _git(root: Path, args: list[str]):
    import subprocess  # nosec B404 - fixed git argv, no shell.

    try:
        return subprocess.run(  # nosec B603 B607 - git from PATH, argument list, no shell.
            ["git", *args],
            cwd=root,
            capture_output=True,
            text=True,
            check=False,
        )
    except OSError:
        return None


def _remote_candidates(root: Path) -> list[str] | None:
    head = _git(root, ["symbolic-ref", "--quiet", "refs/remotes/origin/HEAD"])
    if head is None:
        return None
    names: list[str] = []
    text = (head.stdout or "").strip()
    if head.returncode == 0 and text:
        names.append(text)
    names.append("origin/HEAD")
    names.extend(f"refs/remotes/origin/{name}" for name in _REMOTE_BRANCH_FALLBACKS)
    return names


def _remote_base(root: Path) -> str | None:
    """Merge base with the remote default branch. ``''`` means no such branch."""
    names = _remote_candidates(root)
    if names is None:
        return None
    for name in names:
        verified = _git(root, ["rev-parse", "--verify", "--quiet", name])
        if verified is None:
            return None
        if verified.returncode != 0 or not (verified.stdout or "").strip():
            continue
        merged = _git(root, ["merge-base", "HEAD", name])
        if merged is None or merged.returncode != 0 or not (merged.stdout or "").strip():
            return None
        return merged.stdout.strip()
    return ""


def _outgoing_base(root: Path) -> str | None:
    """Commit range being published. ``None`` means the range cannot be read."""
    upstream = _git(root, ["rev-parse", "--verify", "--quiet", "@{upstream}"])
    if upstream is not None and upstream.returncode == 0 and (upstream.stdout or "").strip():
        base = _git(root, ["merge-base", "HEAD", "@{upstream}"])
        if base is not None and base.returncode == 0 and (base.stdout or "").strip():
            return base.stdout.strip()
        return None
    remote = _remote_base(root)
    if remote is None:
        return None
    if remote:
        return remote
    inside = _git(root, ["rev-parse", "--is-inside-work-tree"])
    if inside is None or inside.returncode != 0:
        return ""
    count = _git(root, ["rev-list", "--count", "HEAD"])
    if count is None or count.returncode != 0:
        return None
    try:
        commits = int((count.stdout or "0").strip() or "0")
    except ValueError:
        return None
    if commits > 40:
        return None
    return _EMPTY_TREE


def _added_diff_text(patch: str) -> str:
    """Text introduced by a patch. ``+++`` file headers are not additions.

    Deletions of lines that already existed on the outgoing base are not new
    commits. An add that a later commit removes stays visible in ``git log -p``.
    """
    added: list[str] = []
    for line in (patch or "").splitlines():
        if not line.startswith("+") or line.startswith("+++"):
            continue
        added.append(line[1:])
    return "\n".join(added)


def committed_secret_failures(root: Path) -> list[str]:
    """Scan outgoing commit and index blobs. A clean worktree does not clear a committed secret."""
    if not (Path(root) / ".git").exists():
        return []
    base = _outgoing_base(Path(root))
    if base is None:
        return ["outgoing-diff-unavailable"]
    if not base:
        return []
    exclude = [".", ":(exclude)tests", ":(exclude)tests/**"]
    # Every outgoing commit, including merge resolutions. Default `git log -p`
    # omits merge patches, so a secret that exists only in a merge commit would
    # disappear after a later merge removes it. Only added lines count: deleting
    # a line that already existed on the base is not a new secret.
    history = _git(
        Path(root),
        ["log", "-p", "--diff-merges=first-parent", "--no-color", f"{base}..HEAD", "--", *exclude],
    )
    head = _git(Path(root), ["diff", "--no-color", f"{base}..HEAD", "--", *exclude])
    staged = _git(Path(root), ["diff", "--cached", "--no-color", "--", *exclude])
    scans = (history, head, staged)
    if any(item is None or item.returncode != 0 for item in scans):
        return ["outgoing-diff-unavailable"]
    text = "\n".join(_added_diff_text(item.stdout or "") for item in scans)
    return secret_failures(text, kind="commit")


def pdca_push_failures(root: Path) -> list[str]:
    """A second consecutive observed failure blocks push until that stage's note exists."""
    folder = _pdca_dir(root)
    events = folder / "events.jsonl"
    summary = folder / "failures.json"
    if not folder.exists() and not events.exists() and not summary.exists():
        return []
    if events.exists():
        try:
            return _pdca_stage_hits(Path(root), _replay_events(events))
        except (OSError, json.JSONDecodeError, TypeError, ValueError):
            return ["pdca-state-unavailable"]
    if summary.is_file():
        return _summary_stage_hits(Path(root), summary)
    return [] if folder.is_dir() else ["pdca-state-unavailable"]


def _summary_stage_hits(root: Path, summary: Path) -> list[str]:
    try:
        payload = json.loads(summary.read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError):
        return ["pdca-state-unavailable"]
    stages = payload.get("stages") if isinstance(payload, dict) else None
    if not isinstance(stages, dict):
        return ["pdca-state-unavailable"]
    return _pdca_stage_hits(root, stages)


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
    """Push-time evidence: PDCA events, outgoing blobs, bounded watcher loops."""
    hits = list(pdca_push_failures(root))
    hits.extend(committed_secret_failures(root))
    hits.extend(_watcher_loop_failures(root, list(changed or [])))
    return hits
