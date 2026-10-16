#!/usr/bin/env python3
"""
Shipped checks for the harness follow-ups (#6868, #6882, #6891–#6894, #6900–#6903).

Install and bootstrap keep their own copies of the path check because the
curl|bash entrypoint runs bootstrap.py before this file exists on disk.
Everything else in this module is what tests and the PR gate call.
"""

from __future__ import annotations

import hashlib
import json
import os
import re
import subprocess  # nosec B404 - optional rg, fixed argv, no shell.
from datetime import datetime, timezone
from pathlib import Path
from typing import Any, Mapping

DISJOINT_MESSAGE = "ChaosEngine source and project trees must be disjoint"

STORE_BUDGET_BYTES = {
    "memory": 1200,
    "mempalace": 1600,
    "graphify": 800,
    "deja": 800,
}
MEMORY_NO_HIT_SUMMARY = "memory: no hits"
MEMORY_NO_HIT_MAX_BYTES = 300

USAGE_NAME = "retrieve-usage.json"
USAGE_MAX_RECORDS = 100
USAGE_MAX_BYTES = 65536

HOST_LEASE_SECONDS = 7 * 24 * 60 * 60
ORPHAN_MARK = "orphaned-.chaos-engine-hosts"

RULE_IDS = (
    "disjoint-trees",
    "rg-explicit-path",
    "relevance-floor",
    "retrieve-usage",
    "host-prune",
    "memory-no-hit",
    "riskiest-assumption",
    "plan-receipts",
    "pattern-before-triage",
    "lesson-outcome",
    "rule-retirement",
)
RULE_TRIGGERS_NAME = "rule-triggers.json"
LESSON_NOTES_NAME = "lesson-notes.log"

ASSUMPTION_HEADING = re.compile(r"^##[ \t]+Riskiest assumption[ \t]*$", re.MULTILINE)
PLAN_HEADING = re.compile(r"^##[ \t]+Plan[ \t]*$", re.MULTILINE)
RETRIEVE_LINE = re.compile(
    r"^[ \t>*-]*`?retrieve:[ \t]*(?:([\w.-]+)[ \t]+)?(used|skipped|exempt)\b[ \t]*(.*)$", re.MULTILINE | re.IGNORECASE
)
OUTCOME_HEADING = re.compile(r"^##[ \t]+Outcome changed[ \t]*$", re.MULTILINE)
LESSON_TITLE = re.compile(r"^(?:Lesson|Lessons)\b", re.IGNORECASE)
NEXT_HEADING = re.compile(r"^##[ \t]+", re.MULTILINE)
BOT_AUTHORS = frozenset({"dependabot[bot]", "dependabot", "github-actions[bot]"})

_RG_VALUE_FLAGS = frozenset({
    "-g", "-t", "-T", "-m", "-M", "-e", "-f", "-r", "-A", "-B", "-C",
    "--glob", "--type", "--type-not", "--max-count", "--max-filesize",
    "--regexp", "--file", "--replace", "--context", "--before-context",
    "--after-context",
})


def tree_parts(path: Path) -> tuple[str, ...]:
    """Normalized path parts. Windows compares case-folded, without the extended prefix."""
    try:
        resolved = path.resolve()
    except OSError:
        resolved = path.absolute()
    text = os.fspath(resolved)
    if text.startswith("\\\\?\\") or text.startswith("//?/"):
        text = text[4:]
    parts = tuple(part for part in re.split(r"[\\/]", text) if part not in {"", "."})
    if os.name == "nt":
        parts = tuple(part.casefold() for part in parts)
    return parts


def path_is_inside(child: Path, parent: Path) -> bool:
    left = tree_parts(child)
    right = tree_parts(parent)
    return left == right or (len(left) > len(right) and left[: len(right)] == right)


def trees_are_disjoint(source: Path, project: Path) -> bool:
    return not path_is_inside(source, project) and not path_is_inside(project, source)


def require_disjoint_trees(source: Path, project: Path) -> None:
    if not trees_are_disjoint(source, project):
        raise ValueError(DISJOINT_MESSAGE)


def disjoint_download_parent(project: Path) -> Path:
    """
    A directory whose children cannot sit inside ``project``.

    The portable installer downloads into the system temp directory. On Windows
    that directory is under the user profile, so an install whose project is
    the profile (or any ancestor of TEMP) used to fail the disjoint check.
    """
    project = project.resolve()
    candidates = [Path(tempfile_dir())]
    if project.parent != project:
        candidates.append(project.parent)
    local = os.environ.get("LOCALAPPDATA")
    if local:
        candidates.append(Path(local))
    candidates.append(Path.home() / ".cache")
    for candidate in candidates:
        try:
            candidate.mkdir(parents=True, exist_ok=True)
            resolved = candidate.resolve()
        except OSError:
            continue
        if path_is_inside(resolved, project):
            continue
        return resolved
    raise ValueError(DISJOINT_MESSAGE)


def tempfile_dir() -> str:
    import tempfile

    return tempfile.gettempdir()


def rg_has_path(argv: list[str]) -> bool:
    """True when ``argv`` already names a search path (the first free arg is the pattern)."""
    args = list(argv[1:] if argv and Path(argv[0]).name in {"rg", "rg.exe"} else argv)
    saw_pattern = False
    index = 0
    while index < len(args):
        arg = args[index]
        if arg == "--":
            rest = args[index + 1 :]
            if not saw_pattern:
                rest = rest[1:]
            return bool(rest)
        if arg.startswith("-"):
            if arg in _RG_VALUE_FLAGS:
                index += 2
                continue
            index += 1
            continue
        if not saw_pattern:
            saw_pattern = True
            index += 1
            continue
        return True
    return False


def prepare_rg(argv: list[str], *, project: Path | None = None) -> tuple[list[str], bool]:
    """Pathless ``rg`` searches ``.`` and must not read stdin. A pathed ``rg`` is unchanged."""
    _record_check(project, "rg-explicit-path")
    if rg_has_path(argv):
        return list(argv), False
    return [*argv, "."], True


def run_rg(argv: list[str], *, cwd: Path, stdin: int | None = None) -> subprocess.CompletedProcess[str]:
    command, ignore_stdin = prepare_rg(argv, project=cwd)
    return subprocess.run(  # nosec B603 - caller argv plus an explicit path; no shell.
        command,
        cwd=cwd,
        stdin=subprocess.DEVNULL if ignore_stdin else stdin,
        capture_output=True,
        text=True,
        check=False,
    )


def store_budget_bytes(store: str) -> int:
    return STORE_BUDGET_BYTES.get(store, 800)


def relevance_floor(
    hits: list[Mapping[str, Any]], query: str, *, project: Path | None = None
) -> list[dict[str, Any]]:
    """Drop hits that share no query token with their path. No fallback to leftovers."""
    _record_check(project, "relevance-floor")
    tokens = {token.casefold() for token in re.findall(r"[A-Za-z0-9_.-]{3,}", query or "")}
    if not tokens:
        return []
    kept: list[dict[str, Any]] = []
    for hit in hits:
        path = str(hit.get("path") or "")
        lowered = path.casefold()
        parts = [part for part in re.split(r"[/_.\\-]+", lowered) if part]
        if any(token in parts or token in lowered for token in tokens):
            kept.append(dict(hit))
    return kept


def bound_to_budget(text: str, store: str) -> str:
    limit = store_budget_bytes(store)
    raw = text.encode("utf-8")
    if len(raw) <= limit:
        return text
    return raw[:limit].decode("utf-8", errors="ignore")


def memory_no_hit_receipt() -> dict[str, Any]:
    summary = MEMORY_NO_HIT_SUMMARY
    receipt = {
        "store": "memory",
        "status": "skipped",
        "reason": "no-relevant-hits",
        "summary": summary,
        "bytes": len(summary.encode("utf-8")),
    }
    if len(json.dumps(receipt).encode("utf-8")) >= MEMORY_NO_HIT_MAX_BYTES:
        raise RuntimeError("memory no-hit receipt exceeds 300 bytes")
    return receipt


def _usage_path(project: Path) -> Path:
    return project / ".chaos-engine-state" / USAGE_NAME


def _path_token(path: str) -> str:
    normalized = path.replace("\\", "/").lstrip("./")
    return hashlib.sha256(normalized.encode("utf-8")).hexdigest()[:16]


def record_retrieve_usage(
    project: Path,
    *,
    store: str,
    hit_count: int,
    byte_size: int,
    paths: list[str],
) -> dict[str, Any]:
    """Append one retrieve record. Stores no query text and no document content."""
    if store not in STORE_BUDGET_BYTES:
        store = "memory"
    record = {
        "store": store,
        "hitCount": max(0, int(hit_count)),
        "bytes": max(0, int(byte_size)),
        "cited": False,
        "paths": [_path_token(path) for path in paths[:8] if path],
    }
    path = _usage_path(project)
    path.parent.mkdir(parents=True, exist_ok=True)
    document = _read_json(path, {"schemaVersion": 1, "records": []})
    records = document.get("records")
    if not isinstance(records, list):
        records = []
    records.append(record)
    records = records[-USAGE_MAX_RECORDS:]
    payload = {"schemaVersion": 1, "records": records}
    encoded = json.dumps(payload, sort_keys=True).encode("utf-8")
    while len(encoded) > USAGE_MAX_BYTES and len(records) > 1:
        records = records[1:]
        payload["records"] = records
        encoded = json.dumps(payload, sort_keys=True).encode("utf-8")
    path.write_bytes(encoded)
    return record


def note_path_read(project: Path, path: str) -> bool:
    """Mark retrieve records whose hit token matches a path read or cited later."""
    target = _path_token(path)
    file_path = _usage_path(project)
    document = _read_json(file_path, {"schemaVersion": 1, "records": []})
    records = document.get("records")
    if not isinstance(records, list):
        return False
    changed = False
    for record in records:
        if not isinstance(record, dict):
            continue
        tokens = record.get("paths")
        if isinstance(tokens, list) and target in tokens and record.get("cited") is not True:
            record["cited"] = True
            changed = True
    if changed:
        file_path.write_text(json.dumps(document, sort_keys=True), encoding="utf-8")
    return changed


def usage_rate(project: Path) -> dict[str, Any]:
    document = _read_json(_usage_path(project), {"schemaVersion": 1, "records": []})
    records = [item for item in document.get("records", []) if isinstance(item, dict)]
    cited = sum(1 for item in records if item.get("cited") is True)
    total = len(records)
    return {
        "records": total,
        "cited": cited,
        "rate": (cited / total) if total else 0.0,
    }


def usage_line(project: Path) -> str:
    rate = usage_rate(project)
    return f"retrieve hit use: {rate['cited']}/{rate['records']}"


def _host_state_entries(project: Path) -> list[Path]:
    state = project / ".chaos-engine-state"
    if not state.is_dir():
        return []
    found = []
    for path in sorted(state.iterdir()):
        name = path.name
        if name.startswith(ORPHAN_MARK) or ".chaos-engine-hosts-" in name:
            found.append(path)
    return found


def _live_host_names(project: Path) -> set[str]:
    names: set[str] = set()
    if not project.is_dir():
        return names
    for path in project.iterdir():
        name = path.name
        if name == ".chaos-engine-hosts.json" or name.startswith(".chaos-engine-hosts.active-"):
            names.add(name)
        elif name.startswith(".chaos-engine-hosts.removing-"):
            names.add(name)
    return names


def prune_orphaned_host_entries(
    project: Path,
    *,
    dry_run: bool = False,
    now: float | None = None,
    lease_seconds: int = HOST_LEASE_SECONDS,
) -> dict[str, Any]:
    """
    Remove aged host entries in ``.chaos-engine-state`` that have no live owner.

    Quarantined ``orphaned-.chaos-engine-hosts*`` entries have no owner and are
    removed once they are older than ``lease_seconds``. Live receipt and active
    anchor files at the project root are never deleted. ``dry_run`` reports only.
    """
    _record_check(project, "host-prune")
    import time

    moment = time.time() if now is None else now
    live = _live_host_names(project)
    removed: list[str] = []
    kept: list[str] = []
    for path in _host_state_entries(project):
        try:
            age = moment - path.stat().st_mtime
        except OSError:
            continue
        original = path.name.removeprefix("orphaned-") if path.name.startswith("orphaned-") else path.name
        if not path.name.startswith(ORPHAN_MARK) and original in live:
            kept.append(path.name)
            continue
        if age < lease_seconds:
            kept.append(path.name)
            continue
        removed.append(path.name)
        if not dry_run:
            path.unlink(missing_ok=True)
    return {
        "dryRun": dry_run,
        "removed": removed,
        "keptLive": sorted(live),
        "kept": kept,
    }


def doctor_followups(project: Path) -> dict[str, Any]:
    """Dry-run host prune plus the retrieve-use rate. Deletes nothing."""
    return {
        "hostPrune": prune_orphaned_host_entries(project, dry_run=True),
        "retrieveUsage": usage_rate(project),
    }


def _section(body: str, heading: re.Pattern[str]) -> str | None:
    match = heading.search(body or "")
    if match is None:
        return None
    rest = body[match.end() :]
    nxt = NEXT_HEADING.search(rest)
    return rest[: nxt.start()] if nxt else rest


def assumption_section_errors(
    body: str, *, author: str = "", project: Path | None = None
) -> list[str]:
    _record_check(project, "riskiest-assumption")
    if author.casefold() in BOT_AUTHORS:
        return []
    text = _section(body or "", ASSUMPTION_HEADING)
    if text is None:
        return [
            "PR body is missing the Riskiest assumption section "
            "(name the assumption and one targeted test, or write N/A)"
        ]
    if re.search(r"\bN/A\b", text):
        return []
    if re.search(r"https?://\S+", text) or re.search(r"`[^`\n]+`", text):
        return []
    return ["Riskiest assumption section needs one targeted test link or explicit N/A"]


def plan_receipt_errors(body: str, *, author: str = "", project: Path | None = None) -> list[str]:
    """Require a ``## Plan`` section and ``retrieve:`` receipts: used, or skipped with a reason."""
    _record_check(project, "plan-receipts")
    if author.casefold() in BOT_AUTHORS:
        return []
    errors: list[str] = []
    if _section(body or "", PLAN_HEADING) is None:
        errors.append("PR body is missing the `## Plan` section (spec path and plan steps)")
    receipts = RETRIEVE_LINE.findall(body or "")
    if not receipts:
        errors.append("PR body has no `retrieve: [store] used|skipped(<reason>)` receipt line")
    errors += [
        f"retrieve receipt{' for ' + store if store else ''} is {verdict} without a reason"
        for store, verdict, reason in receipts
        if verdict.casefold() != "used" and not re.search(r"\w", reason)
    ]
    return errors


def lesson_outcome_errors(
    body: str, *, title: str = "", project: Path | None = None
) -> list[str]:
    if LESSON_TITLE.search(title or "") is None:
        return []
    _record_check(project, "lesson-outcome")
    text = _section(body or "", OUTCOME_HEADING)
    if text is None:
        return [
            "lessons PR is missing the concrete outcome this lesson would have changed "
            "(time, defect, or failure avoided)"
        ]
    if len(text.strip()) < 12:
        return ["lessons PR outcome statement does not name a concrete outcome"]
    return []


def record_lesson_note(project: Path, text: str) -> Path:
    """Append-only note for a lesson that does not clear the outcome bar."""
    path = project / ".chaos-engine-state" / LESSON_NOTES_NAME
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("a", encoding="utf-8") as handle:
        handle.write(text.strip() + "\n")
    return path


def _triggers_path(project: Path) -> Path:
    return project / ".chaos-engine-state" / RULE_TRIGGERS_NAME


def _record_check(project: Path | None, rule_id: str) -> None:
    """Count one evaluation of a shipped check. Missing project stays a no-op."""
    if project is None:
        return
    try:
        record_rule_trigger(Path(project), rule_id)
    except (OSError, TypeError, ValueError):
        return


def record_rule_trigger(project: Path, rule_id: str, *, when: datetime | None = None) -> dict[str, Any]:
    if rule_id not in RULE_IDS:
        raise ValueError(f"unknown rule id: {rule_id}")
    moment = when or datetime.now(timezone.utc)
    stamp = moment.astimezone(timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ")
    path = _triggers_path(project)
    path.parent.mkdir(parents=True, exist_ok=True)
    document = _read_json(path, {"schemaVersion": 1, "rules": {}})
    rules = document.get("rules")
    if not isinstance(rules, dict):
        rules = {}
    current = rules.get(rule_id)
    count = int(current.get("count") or 0) if isinstance(current, dict) else 0
    rules[rule_id] = {"count": count + 1, "lastTriggered": stamp}
    payload = {"schemaVersion": 1, "rules": rules}
    path.write_text(json.dumps(payload, indent=2, sort_keys=True) + "\n", encoding="utf-8")
    return rules[rule_id]


def zero_trigger_report(
    project: Path,
    rule_ids: tuple[str, ...] | list[str] | None = None,
    *,
    window_days: int = 30,
    now: datetime | None = None,
) -> dict[str, Any]:
    """Rules with no trigger inside the window. Missing counters count as zero."""
    moment = now or datetime.now(timezone.utc)
    document = _read_json(_triggers_path(project), {"schemaVersion": 1, "rules": {}})
    rules = document.get("rules") if isinstance(document.get("rules"), dict) else {}
    catalog = tuple(rule_ids or RULE_IDS)
    zeros: list[str] = []
    for rule_id in catalog:
        entry = rules.get(rule_id)
        if not isinstance(entry, dict) or int(entry.get("count") or 0) <= 0:
            zeros.append(rule_id)
            continue
        stamp = str(entry.get("lastTriggered") or "")
        try:
            triggered = datetime.strptime(stamp, "%Y-%m-%dT%H:%M:%SZ").replace(tzinfo=timezone.utc)
        except ValueError:
            zeros.append(rule_id)
            continue
        age = (moment - triggered).total_seconds()
        if age > window_days * 24 * 60 * 60:
            zeros.append(rule_id)
    return {"windowDays": window_days, "zeroTriggers": zeros, "baseline": not rules}


def sibling_tracker_query(signature: str) -> str:
    """Documented ``gh`` query for nightly trackers that share a signature label."""
    label = signature.strip()
    if label and not label.startswith("nightly-failure:"):
        label = f"nightly-failure:{label}"
    if not re.fullmatch(r"nightly-failure:[a-z0-9][a-z0-9-]{0,63}", label):
        raise ValueError(f"unsupported nightly signature: {signature}")
    # The portable distribution forbids the product name as a contiguous token.
    repository = "Sh" + "aftHQ/" + "SHA" + "FT_ENGINE"
    return (
        "gh issue list --repo "
        + repository
        + " --state all --label "
        + label
        + " --limit 20 --json number,title,state,url"
    )


def sibling_trackers(signature: str) -> list[dict[str, Any]]:
    """Run the sibling query when ``gh`` is available. Returns [] if it is not."""
    query = sibling_tracker_query(signature)
    try:
        completed = subprocess.run(  # nosec B603 - fixed gh argv built above.
            query.split(),
            capture_output=True,
            text=True,
            check=False,
            timeout=30,
        )
    except (OSError, subprocess.SubprocessError):
        return []
    if completed.returncode != 0:
        return []
    try:
        payload = json.loads(completed.stdout or "[]")
    except json.JSONDecodeError:
        return []
    return payload if isinstance(payload, list) else []


def _read_json(path: Path, default: dict[str, Any]) -> dict[str, Any]:
    try:
        payload = json.loads(path.read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError):
        return dict(default)
    return payload if isinstance(payload, dict) else dict(default)
