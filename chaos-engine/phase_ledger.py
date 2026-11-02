#!/usr/bin/env python3
"""Zero-LLM machine phase ledger under `.chaos-engine-state/` (#5623)."""

from __future__ import annotations

import argparse
import json
import re
import time
from pathlib import Path
from typing import Any

SCHEMA_VERSION = 1
LEDGER_RELATIVE = Path(".chaos-engine-state") / "phase-ledger.json"
PHASES = (
    "triage",
    "research",
    "implement",
    "check",
    "learning",
)
TRIAGE_VALUES = frozenset({"one-file", "one-module", "public-contract"})
SESSION_ID_RE = re.compile(r"^[A-Za-z0-9_.:-]{1,128}$")


def project_root(start: Path | None = None) -> Path:
    here = (start or Path.cwd()).resolve()
    for candidate in (here, *here.parents):
        if (candidate / ".chaos-engine" / "install.py").is_file() or (
            candidate / "chaos-engine" / "install.py"
        ).is_file():
            return candidate
    return here


def ledger_path(project: Path | None = None) -> Path:
    return project_root(project) / LEDGER_RELATIVE


def _empty_ledger() -> dict[str, Any]:
    return {
        "schemaVersion": SCHEMA_VERSION,
        "updatedAt": 0,
        "sessions": {},
    }


def load_ledger(project: Path | None = None) -> dict[str, Any]:
    path = ledger_path(project)
    if not path.is_file():
        return _empty_ledger()
    try:
        document = json.loads(path.read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError):
        return _empty_ledger()
    if not isinstance(document, dict):
        return _empty_ledger()
    sessions = document.get("sessions")
    if not isinstance(sessions, dict):
        document["sessions"] = {}
    document["schemaVersion"] = SCHEMA_VERSION
    return document


def save_ledger(document: dict[str, Any], project: Path | None = None) -> Path:
    path = ledger_path(project)
    path.parent.mkdir(parents=True, exist_ok=True)
    document = dict(document)
    document["schemaVersion"] = SCHEMA_VERSION
    document["updatedAt"] = int(time.time())
    path.write_text(json.dumps(document, indent=2, sort_keys=True) + "\n", encoding="utf-8")
    return path


def normalize_triage(value: str | None) -> str | None:
    if value is None:
        return None
    raw = str(value).strip().casefold().replace("_", "-").replace(" ", "-")
    aliases = {
        "onefile": "one-file",
        "file": "one-file",
        "module": "one-module",
        "onemodule": "one-module",
        "public": "public-contract",
        "contract": "public-contract",
        "hard-to-reverse": "public-contract",
    }
    raw = aliases.get(raw, raw)
    return raw if raw in TRIAGE_VALUES else None


def record_phase(
    session_id: str,
    phase: str,
    *,
    triage: str | None = None,
    note: str = "",
    project: Path | None = None,
) -> dict[str, Any]:
    if not SESSION_ID_RE.fullmatch(session_id or ""):
        raise ValueError("invalid session id")
    phase_name = str(phase).strip().casefold()
    if phase_name not in PHASES:
        raise ValueError(f"unsupported phase: {phase}")
    document = load_ledger(project)
    sessions = document.setdefault("sessions", {})
    if not isinstance(sessions, dict):
        sessions = {}
        document["sessions"] = sessions
    entry = sessions.get(session_id)
    if not isinstance(entry, dict):
        entry = {"phases": [], "triage": None}
    phases = entry.setdefault("phases", [])
    if not isinstance(phases, list):
        phases = []
        entry["phases"] = phases
    record = {
        "phase": phase_name,
        "at": int(time.time()),
        "note": str(note)[:160],
    }
    phases.append(record)
    # Keep bounded
    if len(phases) > 64:
        entry["phases"] = phases[-64:]
    normalized = normalize_triage(triage) if triage is not None else None
    if normalized is not None:
        entry["triage"] = normalized
    sessions[session_id] = entry
    # Cap sessions map
    if len(sessions) > 128:
        for key in sorted(sessions, key=lambda k: sessions[k].get("phases", [{}])[-1].get("at", 0))[
            : len(sessions) - 128
        ]:
            sessions.pop(key, None)
    save_ledger(document, project)
    return entry


def session_entry(session_id: str, project: Path | None = None) -> dict[str, Any]:
    document = load_ledger(project)
    sessions = document.get("sessions")
    if not isinstance(sessions, dict):
        return {}
    entry = sessions.get(session_id)
    return entry if isinstance(entry, dict) else {}


def session_triage(session_id: str, project: Path | None = None) -> str | None:
    entry = session_entry(session_id, project)
    return normalize_triage(entry.get("triage") if isinstance(entry, dict) else None)


def research_required_for_triage(triage: str | None) -> bool:
    """Hard-to-reverse / public-contract triage auto-enforces research gate."""
    return normalize_triage(triage) == "public-contract"


def doctor_phase_ledger_summary(project: Path | None = None) -> dict[str, Any]:
    """Bounded summary for doctor / Stop hooks (no secrets)."""
    document = load_ledger(project)
    sessions = document.get("sessions")
    if not isinstance(sessions, dict):
        return {"schemaVersion": SCHEMA_VERSION, "sessions": 0, "status": "absent"}
    return {
        "schemaVersion": SCHEMA_VERSION,
        "sessions": len(sessions),
        "status": "healthy" if sessions else "absent",
        "updatedAt": document.get("updatedAt"),
    }


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    sub = parser.add_subparsers(dest="command", required=True)
    record = sub.add_parser("record")
    record.add_argument("--session-id", required=True)
    record.add_argument("--phase", required=True, choices=PHASES)
    record.add_argument("--triage", default=None)
    record.add_argument("--note", default="")
    record.add_argument("--project", type=Path, default=None)
    show = sub.add_parser("show")
    show.add_argument("--session-id", required=True)
    show.add_argument("--project", type=Path, default=None)
    summary = sub.add_parser("summary")
    summary.add_argument("--project", type=Path, default=None)
    args = parser.parse_args(argv)
    if args.command == "record":
        entry = record_phase(
            args.session_id,
            args.phase,
            triage=args.triage,
            note=args.note,
            project=args.project,
        )
        print(json.dumps({"recorded": True, "entry": entry}, sort_keys=True))
        return 0
    if args.command == "show":
        print(json.dumps(session_entry(args.session_id, args.project), sort_keys=True))
        return 0
    print(json.dumps(doctor_phase_ledger_summary(args.project), sort_keys=True))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
