#!/usr/bin/env python3
"""
Memory provenance, trust levels, and quarantine (#6520 / AgentPoison).

Every learned item records origin + trust. Untrusted origins stay quarantined so
retrieval and skill promotion skip them until a verifier passes.
"""

from __future__ import annotations

import argparse
import json
import re
import time
from pathlib import Path
from typing import Any

SCHEMA_VERSION = 1

# Origins the harness itself or the owner produced.
TRUSTED_ORIGINS = frozenset(
    {
        "owner",
        "learning-session",
        "verifier",
        "doctor",
        "ci",
        "legacy",
    }
)
# Origins that must not influence retrieval or promotion until verified.
UNTRUSTED_ORIGINS = frozenset(
    {
        "web",
        "tool-output",
        "other-agent",
        "unknown",
    }
)
ORIGINS = TRUSTED_ORIGINS | UNTRUSTED_ORIGINS
TRUST_LEVELS = frozenset({"trusted", "verified", "quarantined"})
ORIGIN_RE = re.compile(r"^[a-z][a-z0-9_-]{0,31}$")

PROVENANCE_KEYS = frozenset({"origin", "trust", "verifiedBy", "verifiedAt"})


def normalize_origin(value: object) -> str:
    raw = re.sub(r"[^a-z0-9_-]+", "-", str(value or "").strip().casefold())[:32]
    if not raw or ORIGIN_RE.fullmatch(raw) is None:
        return "unknown"
    return raw


def default_trust(origin: str) -> str:
    name = normalize_origin(origin)
    if name in TRUSTED_ORIGINS:
        return "trusted"
    return "quarantined"


def stamp_fields(
    *,
    origin: str = "unknown",
    trust: str | None = None,
    verified_by: str | None = None,
    verified_at: int | None = None,
) -> dict[str, Any]:
    """Return provenance fields to merge into a learned item."""
    origin_name = normalize_origin(origin)
    if trust is None:
        level = default_trust(origin_name)
    else:
        level = str(trust).strip().casefold()
        if level not in TRUST_LEVELS:
            raise ValueError(f"invalid trust level: {trust}")
        # Untrusted origins cannot be written as trusted without verify().
        if level == "trusted" and origin_name not in TRUSTED_ORIGINS:
            raise ValueError("untrusted origin cannot be stamped trusted; use verify()")
    fields: dict[str, Any] = {"origin": origin_name, "trust": level}
    if level == "verified":
        by = normalize_origin(verified_by or "verifier")
        fields["verifiedBy"] = by
        fields["verifiedAt"] = int(verified_at if verified_at is not None else time.time())
    return fields


def stamp_item(item: dict[str, Any], *, origin: str = "unknown", trust: str | None = None) -> dict[str, Any]:
    """Return a shallow copy of item with provenance fields applied."""
    if not isinstance(item, dict):
        raise ValueError("item must be a dict")
    stamped = dict(item)
    stamped.update(stamp_fields(origin=origin, trust=trust))
    return stamped


def read_provenance(item: dict[str, Any] | None) -> dict[str, Any]:
    """Normalize provenance on read. Legacy items without fields are trusted."""
    if not isinstance(item, dict):
        return stamp_fields(origin="unknown", trust="quarantined")
    origin = item.get("origin")
    trust = item.get("trust")
    if origin is None and trust is None:
        # Grandfather pre-#6520 stores so retrieval keeps working.
        source = item.get("source")
        inferred = normalize_origin(source) if source else "legacy"
        if inferred not in ORIGINS:
            inferred = "legacy" if source else "legacy"
        return stamp_fields(origin=inferred if inferred in TRUSTED_ORIGINS else "legacy", trust="trusted")
    origin_name = normalize_origin(origin or "unknown")
    level = str(trust or default_trust(origin_name)).strip().casefold()
    if level not in TRUST_LEVELS:
        level = default_trust(origin_name)
    fields: dict[str, Any] = {"origin": origin_name, "trust": level}
    if level == "verified":
        fields["verifiedBy"] = normalize_origin(item.get("verifiedBy") or "verifier")
        try:
            fields["verifiedAt"] = int(item.get("verifiedAt") or 0)
        except (TypeError, ValueError):
            fields["verifiedAt"] = 0
    return fields


def is_retrievable(item: dict[str, Any] | None) -> bool:
    return read_provenance(item).get("trust") in {"trusted", "verified"}


def is_promotable(item: dict[str, Any] | None) -> bool:
    """Skill / playbook promotion requires verified, or trusted from a trusted origin."""
    prov = read_provenance(item)
    trust = prov.get("trust")
    if trust == "verified":
        return True
    if trust == "trusted" and prov.get("origin") in TRUSTED_ORIGINS:
        return True
    return False


def filter_retrievable(items: list[Any], *, limit: int | None = None) -> list[dict[str, Any]]:
    selected: list[dict[str, Any]] = []
    for item in items:
        if not isinstance(item, dict):
            continue
        if not is_retrievable(item):
            continue
        selected.append(item)
        if limit is not None and len(selected) >= limit:
            break
    return selected


def filter_promotable(items: list[Any]) -> list[dict[str, Any]]:
    return [item for item in items if isinstance(item, dict) and is_promotable(item)]


def verify_item(
    item: dict[str, Any],
    *,
    by: str = "verifier",
    at: int | None = None,
) -> dict[str, Any]:
    """Promote a quarantined (or trusted) item to verified after an external check."""
    if not isinstance(item, dict):
        raise ValueError("item must be a dict")
    prov = read_provenance(item)
    updated = dict(item)
    updated.update(
        stamp_fields(
            origin=str(prov.get("origin") or "unknown"),
            trust="verified",
            verified_by=by,
            verified_at=at,
        )
    )
    return updated


def summarize_items(items: list[Any]) -> dict[str, Any]:
    trusted = quarantined = verified = 0
    for item in items:
        if not isinstance(item, dict):
            continue
        level = read_provenance(item).get("trust")
        if level == "trusted":
            trusted += 1
        elif level == "verified":
            verified += 1
        else:
            quarantined += 1
    total = trusted + verified + quarantined
    return {
        "schemaVersion": SCHEMA_VERSION,
        "kind": "memory-provenance-summary",
        "total": total,
        "trusted": trusted,
        "verified": verified,
        "quarantined": quarantined,
        "status": "healthy" if total else "absent",
    }


def doctor_provenance_summary(project: Path | None = None) -> dict[str, Any]:
    """Aggregate provenance across the heuristics store (primary learned-item home)."""
    root = (project or Path.cwd()).resolve()
    for candidate in (root, *root.parents):
        if (candidate / ".chaos-engine" / "install.py").is_file() or (
            candidate / "chaos-engine" / "install.py"
        ).is_file():
            root = candidate
            break
    path = root / ".chaos-engine-state" / "heuristics" / "index.json"
    items: list[Any] = []
    if path.is_file():
        try:
            document = json.loads(path.read_text(encoding="utf-8"))
        except (OSError, UnicodeDecodeError, json.JSONDecodeError):
            document = {}
        raw = document.get("items") if isinstance(document, dict) else None
        if isinstance(raw, list):
            items = raw
    summary = summarize_items(items)
    summary["store"] = "heuristics"
    summary["path"] = str(path.relative_to(root)) if path.is_relative_to(root) else str(path)
    return summary


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    sub = parser.add_subparsers(dest="command", required=True)
    summary = sub.add_parser("summary")
    summary.add_argument("--project", type=Path, default=None)
    stamp = sub.add_parser("stamp")
    stamp.add_argument("--origin", required=True)
    stamp.add_argument("--trust", default=None)
    verify = sub.add_parser("verify")
    verify.add_argument("--origin", default="unknown")
    verify.add_argument("--by", default="verifier")
    args = parser.parse_args(argv)
    try:
        if args.command == "summary":
            print(json.dumps(doctor_provenance_summary(args.project), sort_keys=True))
            return 0
        if args.command == "stamp":
            print(json.dumps(stamp_fields(origin=args.origin, trust=args.trust), sort_keys=True))
            return 0
        item = {"text": "probe", "origin": args.origin, "trust": "quarantined"}
        print(json.dumps(verify_item(item, by=args.by), sort_keys=True))
        return 0
    except ValueError as error:
        print(str(error), file=__import__("sys").stderr)
        return 2


if __name__ == "__main__":
    raise SystemExit(main())
