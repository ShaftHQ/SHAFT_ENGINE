"""
Repeat-failure PDCA: stop after two failures on the same stage/gate.

When the same stage or gate fails twice, a task overruns its budget, or
different failures hit the same stage, the agent must STOP. It writes a
root-cause note (stage, failures, hypothesis, evidence), then a plan, makes
one change, checks it, then acts. No chained fix-after-fail.

Notes live under ``.chaos-engine-state/repeat-failure-pdca/`` (JSON). The
always-loaded path is ``references/bot-entry.md`` + ChaosEngine SKILL
``Check and deliver``.
"""

from __future__ import annotations

import json
from pathlib import Path
from typing import Any

STATE_DIR_NAME = "repeat-failure-pdca"
NOTE_REQUIRED_KEYS = ("stage", "failures", "hypothesis", "evidence", "plan")


def _result(hits: list[str], error: str) -> dict[str, Any]:
    return {"ok": not hits, "hits": hits, "error": None if not hits else error}


def state_dir(root: Path | None = None) -> Path:
    base = Path(root) if root is not None else Path.cwd()
    return base / ".chaos-engine-state" / STATE_DIR_NAME


def note_path_for_stage(stage: str, root: Path | None = None) -> Path:
    safe = "".join(ch if ch.isalnum() or ch in "-_" else "-" for ch in stage.strip()) or "stage"
    return state_dir(root) / f"{safe}.json"


def load_root_cause_note(path: Path) -> dict[str, Any] | None:
    try:
        raw = json.loads(path.read_text(encoding="utf-8"))
    except (OSError, ValueError, TypeError):
        return None
    return raw if isinstance(raw, dict) else None


def note_covers_stage(note: dict[str, Any] | None, stage: str) -> bool:
    if not note or any(not note.get(key) for key in NOTE_REQUIRED_KEYS):
        return False
    return str(note.get("stage") or "").strip() == stage.strip()


def _distinct_kinds(failures: list[Any]) -> int:
    kinds = {
        str(item.get("kind") or item.get("summary") or "")
        for item in failures
        if isinstance(item, dict)
    }
    return len({k for k in kinds if k})


def _resolve_note(record: dict[str, Any]) -> dict[str, Any] | None:
    note = record.get("root_cause_note")
    if isinstance(note, dict):
        return note
    path_value = record.get("root_cause_note_path")
    if not path_value:
        return None
    path = Path(path_value)
    root = record.get("project_root")
    if not path.is_absolute() and root:
        path = Path(root) / path
    return load_root_cause_note(path)


def _trigger_label(record: dict[str, Any], failures: list[Any], distinct: int, stage: str) -> str | None:
    if len(failures) >= 2:
        return "repeat-failure-without-note"
    if record.get("budget_overrun"):
        return "budget-overrun-without-note"
    if distinct >= 2 and stage:
        return "distinct-failures-same-stage-without-note"
    return None


def evaluate_repeat_failure_pdca(record: dict[str, Any]) -> dict[str, Any]:
    """Fail closed when a stage repeats failure without a PDCA root-cause note."""
    if not isinstance(record, dict):
        return _result(["record"], "repeat-failure-pdca gate failed")
    stage = str(record.get("stage") or "").strip()
    failures = record.get("consecutive_failures") or []
    if not isinstance(failures, list):
        return _result(["failures-shape"], "repeat-failure-pdca gate failed")

    distinct = record.get("distinct_failure_kinds")
    if not isinstance(distinct, int):
        distinct = _distinct_kinds(failures)

    hits: list[str] = []
    if record.get("chained_fix_after_fail"):
        hits.append("chained-fix-after-fail")

    label = _trigger_label(record, failures, distinct, stage)
    if label is None:
        return _result(hits, "repeat-failure-pdca gate failed")

    if not stage:
        hits.append("missing-stage")
    note = _resolve_note(record)
    if not note_covers_stage(note, stage):
        hits.append(label)
    return _result(hits, "repeat-failure-pdca gate failed")


def evaluate_state_dir_note(stage: str, root: Path | None = None) -> dict[str, Any]:
    """Convenience: load ``.chaos-engine-state/repeat-failure-pdca/<stage>.json``."""
    path = note_path_for_stage(stage, root)
    note = load_root_cause_note(path) if path.is_file() else None
    return evaluate_repeat_failure_pdca(
        {
            "stage": stage,
            "consecutive_failures": [{"kind": "ci"}, {"kind": "ci"}],
            "root_cause_note": note,
            "project_root": str(root) if root else None,
        }
    )
