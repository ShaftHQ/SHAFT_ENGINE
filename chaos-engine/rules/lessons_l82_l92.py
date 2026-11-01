"""
Fail-closed gates for CE lessons L82–L85 and L87–L92.

Each ``evaluate_*`` returns ``{"ok": bool, "hits": list, "error": str|None}``.
Callers (tests, tip-preflight, doctor) treat ``ok is False`` as a hard failure.
"""

from __future__ import annotations

import re
from pathlib import Path
from typing import Any


def _result(hits: list[str], error: str) -> dict[str, Any]:
    return {"ok": not hits, "hits": hits, "error": None if not hits else error}


# ----- L82 evidence compose redaction -----
_L82_NEEDLES = (
    re.compile(r"redact", re.I),
    re.compile(r"checkpoint", re.I),
    re.compile(r"(HEAD|commit).{0,40}(preflight|equals|==)|preflight.{0,40}(HEAD|commit)", re.I),
    re.compile(r"(25\s*\s*MB|size\s*cap|slim)", re.I),
)


def evaluate_evidence_compose(plan: str) -> dict[str, Any]:
    """L82: compose/zip plan must name redaction, slim/cap, checkpoint, HEAD preflight."""
    text = plan or ""
    hits = []
    labels = ("redaction", "checkpoint", "HEAD-preflight", "size-cap")
    for label, pattern in zip(labels, _L82_NEEDLES):
        if not pattern.search(text):
            hits.append(label)
    return _result(hits, "L82 evidence-compose gate failed")


# ----- L83 status fresh -----
_L83_FETCH = re.compile(r"\b(fetch|live\s+state|fresh\s+status|gh\s+pr\s+view|gh\s+issue)\b", re.I)
_L83_DROPPED = re.compile(r"\b(dropped|VERIFY-DROPPED|verify.dropped)\b", re.I)


def evaluate_status_fresh(report: str) -> dict[str, Any]:
    """L83: status text must show a fresh fetch and a dropped-item check."""
    text = report or ""
    hits = []
    if not _L83_FETCH.search(text):
        hits.append("fresh-fetch")
    if not _L83_DROPPED.search(text):
        hits.append("dropped-check")
    return _result(hits, "L83 status-fresh gate failed")


# ----- L84 mempalace sqlite doctor (pure helpers; install wires doctor) -----
PREVIOUS_DIR = re.compile(r"^mempalace\.previous-")


def evaluate_mempalace_sqlite(palace: Path) -> dict[str, Any]:
    """L84: palace directory present must contain sqlite_exact.sqlite3."""
    hits: list[str] = []
    if palace.is_dir() and not (palace / "sqlite_exact.sqlite3").is_file():
        hits.append("missing-sqlite_exact")
    return _result(hits, "L84 mempalace sqlite missing")


def empty_previous_dirs(parent: Path) -> list[Path]:
    """Return empty ``mempalace.previous-*`` dirs beside *parent*'s parent or *parent*."""
    found: list[Path] = []
    if not parent.exists():
        return found
    for child in parent.iterdir():
        if child.is_dir() and PREVIOUS_DIR.match(child.name):
            try:
                if not any(child.iterdir()):
                    found.append(child)
            except OSError:
                continue
    return found


def evaluate_no_empty_previous(parent: Path) -> dict[str, Any]:
    """L84: repair must not leave empty mempalace.previous-* directories."""
    empties = empty_previous_dirs(parent)
    hits = [p.name for p in empties]
    return _result(hits, "L84 empty mempalace.previous-* left behind")


def remove_empty_previous_dirs(parent: Path) -> list[str]:
    """Delete empty mempalace.previous-* dirs; return removed names."""
    removed: list[str] = []
    for path in empty_previous_dirs(parent):
        try:
            path.rmdir()
            removed.append(path.name)
        except OSError:
            continue
    return removed


# ----- L85 dump partial denominator -----
def evaluate_dump_partial(receipt: dict[str, Any]) -> dict[str, Any]:
    """L85: count_ok never true without own_shard; partial needs explicit denominator."""
    hits: list[str] = []
    if not isinstance(receipt, dict):
        return _result(["receipt"], "L85 dump receipt missing")
    own = receipt.get("own_shard") or receipt.get("known_shard_ids")
    count_ok = bool(receipt.get("count_ok"))
    if count_ok and not own:
        hits.append("count_ok-without-shard")
    if receipt.get("partial") and not receipt.get("denominator"):
        hits.append("partial-without-denominator")
    return _result(hits, "L85 dump-partial-denominator gate failed")


# ----- L87 locator runtime xpath -----
_XPATH_TEMPLATE = re.compile(r"concat\s*\(|quote\s*\(", re.I)


def evaluate_locator_inventory(entries: list[dict[str, Any]]) -> dict[str, Any]:
    """L87: runtime xpath templates must be resolved before classifying broken."""
    hits: list[str] = []
    for entry in entries or []:
        expr = str(entry.get("xpath") or entry.get("expression") or "")
        status = str(entry.get("status") or "").lower()
        resolved = entry.get("resolved_xpath") or entry.get("resolved")
        if _XPATH_TEMPLATE.search(expr) and status in {"broken", "fail", "invalid"} and not resolved:
            hits.append(str(entry.get("id") or expr[:40]))
    return _result(hits, "L87 locator-runtime-xpath gate failed")


# ----- L88 highlight element id -----
def evaluate_highlight_identity(record: dict[str, Any]) -> dict[str, Any]:
    """L88: highlighted node id must equal intended target id."""
    hits: list[str] = []
    intended = record.get("intended_id") or record.get("target_id")
    highlighted = record.get("highlighted_id") or record.get("actual_id")
    if not intended or not highlighted or str(intended) != str(highlighted):
        hits.append("identity-mismatch")
    return _result(hits, "L88 highlight-element-id gate failed")


# ----- L89 teams zip apk -----
def evaluate_share_paths(paths: list[str]) -> dict[str, Any]:
    """L89: raw .apk paths fail; .zip of apk ok."""
    hits: list[str] = []
    for raw in paths or []:
        name = str(raw).replace("\\", "/").rsplit("/", 1)[-1].lower()
        if name.endswith(".apk"):
            hits.append(str(raw))
    return _result(hits, "L89 teams-zip-apk gate failed")


# ----- L90 StopSubagent targeted -----
def evaluate_stop_subagent(payload: dict[str, Any]) -> dict[str, Any]:
    """L90: StopSubagent all:true is denied."""
    hits: list[str] = []
    if not isinstance(payload, dict):
        return _result(["payload"], "L90 stop-subagent gate failed")
    all_flag = payload.get("all")
    if all_flag is True or str(all_flag).lower() in {"true", "1", "yes"}:
        hits.append("all:true")
    return _result(hits, "L90 StopSubagent all:true denied")


# ----- L91 evidence html size -----
_EVIDENCE_HTML = re.compile(r"evidence.*\.(html?)$", re.I)


def evaluate_evidence_html_paths(paths_with_size: list[tuple[str, int]]) -> dict[str, Any]:
    """L91: evidence HTML in git paths fails; files >20MB must be zipped."""
    hits: list[str] = []
    for path, size in paths_with_size or []:
        norm = str(path).replace("\\", "/")
        if _EVIDENCE_HTML.search(norm) and not norm.endswith(".zip"):
            # tracked evidence html is forbidden
            if "/.git/" not in norm and not norm.endswith(".gitignore"):
                hits.append(f"html:{norm}")
        if size > 20 * 1024 * 1024 and not str(path).lower().endswith(".zip"):
            hits.append(f"oversize:{path}")
    return _result(hits, "L91 evidence-html-size gate failed")


# ----- L92 CE version after upgrade -----
def evaluate_version_match(installed: str, intended: str) -> dict[str, Any]:
    """L92: installed VERSION must equal intended release."""
    hits: list[str] = []
    left = (installed or "").strip()
    right = (intended or "").strip()
    if not left or not right or left != right:
        hits.append(f"{left!r}!={right!r}")
    return _result(hits, "L92 CE VERSION mismatch after upgrade")


def read_version_file(path: Path) -> str:
    try:
        return path.read_text(encoding="utf-8").strip()
    except OSError:
        return ""
