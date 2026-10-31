"""
SkillSpector baseline refresh keyed by semantic id (H-SKILLSPECTOR).

Native SkillSpector baselines match content hashes. Skill wording churn
rewrites hashes for the same finding. This helper keys entries by
``semantic_id = rule_id + ":" + file`` (optional message slug) and updates
the hash in place while keeping the accepted reason.
"""
from __future__ import annotations

import re
from pathlib import Path
from typing import Any

try:
    import yaml  # type: ignore
except ImportError:  # pragma: no cover - PyYAML is a CI dependency
    yaml = None  # type: ignore


def semantic_id(rule_id: str, file: str, message: str = "") -> str:
    """Stable id for one finding class."""
    base = f"{str(rule_id).strip()}:{str(file).strip().replace(chr(92), '/')}"
    slug = re.sub(r"[^a-z0-9]+", "-", str(message).strip().lower())[:48].strip("-")
    return f"{base}:{slug}" if slug else base


def _load(path: Path) -> dict[str, Any]:
    if yaml is None:
        raise RuntimeError("PyYAML required for semantic baseline refresh")
    raw = yaml.safe_load(path.read_text(encoding="utf-8")) or {}
    if not isinstance(raw, dict):
        raise ValueError("baseline must be a mapping")
    raw.setdefault("fingerprints", [])
    return raw


def refresh_entry(
    baseline: dict[str, Any],
    *,
    rule_id: str,
    file: str,
    new_hash: str,
    reason: str | None = None,
    message: str = "",
) -> dict[str, Any]:
    """Update or append one fingerprint by semantic id; return the entry."""
    sid = semantic_id(rule_id, file, message)
    fingerprints = baseline.setdefault("fingerprints", [])
    for entry in fingerprints:
        if not isinstance(entry, dict):
            continue
        existing = entry.get("semantic_id") or semantic_id(
            str(entry.get("rule_id") or ""),
            str(entry.get("file") or ""),
            str(entry.get("message") or ""),
        )
        if existing == sid:
            entry["hash"] = new_hash
            entry["rule_id"] = rule_id
            entry["file"] = file
            entry["semantic_id"] = sid
            if reason is not None:
                entry["reason"] = reason
            return entry
    entry = {
        "hash": new_hash,
        "rule_id": rule_id,
        "file": file,
        "semantic_id": sid,
        "reason": reason or "Accepted: semantic-id refresh",
    }
    fingerprints.append(entry)
    return entry


def refresh_file(
    path: Path | str,
    *,
    rule_id: str,
    file: str,
    new_hash: str,
    reason: str | None = None,
    message: str = "",
) -> dict[str, Any]:
    """Load, refresh one entry, write back; return the entry."""
    if yaml is None:
        raise RuntimeError("PyYAML required for semantic baseline refresh")
    target = Path(path)
    data = _load(target) if target.is_file() else {"version": 2, "fingerprints": []}
    entry = refresh_entry(
        data,
        rule_id=rule_id,
        file=file,
        new_hash=new_hash,
        reason=reason,
        message=message,
    )
    target.write_text(
        yaml.safe_dump(data, sort_keys=False, allow_unicode=True),
        encoding="utf-8",
    )
    return entry
