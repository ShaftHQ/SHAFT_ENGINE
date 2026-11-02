#!/usr/bin/env python3
"""Bounded zero-LLM counters for learning metrics (#5653 / master plan #1)."""

from __future__ import annotations

import json
import time
from pathlib import Path
from typing import Any

SCHEMA_VERSION = 1
COUNTERS_RELATIVE = Path(".chaos-engine-state") / "learning-counters.json"
MAX_DIGESTS = 32


def project_root(start: Path | None = None) -> Path:
    here = (start or Path.cwd()).resolve()
    for candidate in (here, *here.parents):
        if (candidate / ".chaos-engine" / "install.py").is_file() or (
            candidate / "chaos-engine" / "install.py"
        ).is_file():
            return candidate
    return here


def counters_path(project: Path | None = None) -> Path:
    return project_root(project) / COUNTERS_RELATIVE


def _empty() -> dict[str, Any]:
    return {
        "schemaVersion": SCHEMA_VERSION,
        "updatedAt": 0,
        "sessionStartBytesLast": 0,
        "sessionStartBytesMax": 0,
        "sessionStartCount": 0,
        "denials": 0,
        "deliveryDigests": [],
        "learningSessionDigests": [],
    }


def load_counters(project: Path | None = None) -> dict[str, Any]:
    path = counters_path(project)
    if not path.is_file():
        return _empty()
    try:
        document = json.loads(path.read_text(encoding="utf-8"))
    except (OSError, UnicodeDecodeError, json.JSONDecodeError):
        return _empty()
    if not isinstance(document, dict):
        return _empty()
    base = _empty()
    base.update({k: document.get(k, base[k]) for k in base})
    base["schemaVersion"] = SCHEMA_VERSION
    for key in ("deliveryDigests", "learningSessionDigests"):
        values = base.get(key)
        if not isinstance(values, list):
            base[key] = []
        else:
            base[key] = [str(item)[:64] for item in values if isinstance(item, str)][
                -MAX_DIGESTS:
            ]
    for key in (
        "sessionStartBytesLast",
        "sessionStartBytesMax",
        "sessionStartCount",
        "denials",
        "updatedAt",
    ):
        value = base.get(key)
        if not isinstance(value, int) or isinstance(value, bool) or value < 0:
            base[key] = 0
    return base


def save_counters(document: dict[str, Any], project: Path | None = None) -> Path:
    path = counters_path(project)
    path.parent.mkdir(parents=True, exist_ok=True)
    payload = dict(document)
    payload["schemaVersion"] = SCHEMA_VERSION
    payload["updatedAt"] = int(time.time())
    path.write_text(json.dumps(payload, indent=2, sort_keys=True) + "\n", encoding="utf-8")
    return path


def record_session_start_bytes(byte_count: int, project: Path | None = None) -> dict[str, Any]:
    """Record last SessionStart injection size (locator budget evidence)."""
    if not isinstance(byte_count, int) or isinstance(byte_count, bool) or byte_count < 0:
        raise ValueError("byte_count must be a non-negative int")
    document = load_counters(project)
    document["sessionStartBytesLast"] = byte_count
    document["sessionStartBytesMax"] = max(int(document.get("sessionStartBytesMax") or 0), byte_count)
    document["sessionStartCount"] = int(document.get("sessionStartCount") or 0) + 1
    save_counters(document, project)
    return document


def record_denial(project: Path | None = None) -> dict[str, Any]:
    document = load_counters(project)
    document["denials"] = int(document.get("denials") or 0) + 1
    save_counters(document, project)
    return document


def _append_digest(document: dict[str, Any], key: str, digest: str) -> None:
    cleaned = "".join(ch for ch in str(digest).strip() if ch.isalnum() or ch in "-_")[:64]
    if not cleaned:
        return
    values = document.setdefault(key, [])
    if not isinstance(values, list):
        values = []
        document[key] = values
    if cleaned not in values:
        values.append(cleaned)
    document[key] = values[-MAX_DIGESTS:]


def record_delivery_digest(digest: str, project: Path | None = None) -> dict[str, Any]:
    document = load_counters(project)
    _append_digest(document, "deliveryDigests", digest)
    save_counters(document, project)
    return document


def record_learning_session_digest(digest: str, project: Path | None = None) -> dict[str, Any]:
    document = load_counters(project)
    _append_digest(document, "learningSessionDigests", digest)
    save_counters(document, project)
    return document
