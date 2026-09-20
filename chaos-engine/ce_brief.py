#!/usr/bin/env python3
"""Locator-only ChaosEngine system brief for local-agency design turns (#6067)."""

from __future__ import annotations

import argparse
import json
import sys
from pathlib import Path

SCHEMA_VERSION = 1
DEFAULT_MAX_BYTES = 4096
ABSOLUTE_MAX_BYTES = 6144

CORE_LOCATORS = (
    "chaos-engine/skills/chaos-engine/SKILL.md",
    "chaos-engine/references/level-1-catalog.md",
    "chaos-engine/references/zero-llm-catalog.md",
    ".chaos-engine-state/wake-pack.md",
)


def _clamp_max_bytes(max_bytes: int) -> int:
    try:
        value = int(max_bytes)
    except (TypeError, ValueError):
        value = DEFAULT_MAX_BYTES
    return min(max(1, value), ABSOLUTE_MAX_BYTES)


def _truncate_utf8(text: str, max_bytes: int) -> tuple[str, bool]:
    raw = text.encode("utf-8")
    if len(raw) <= max_bytes:
        return text, False
    return raw[:max_bytes].decode("utf-8", errors="ignore"), True


def build_brief(project: Path | None = None, max_bytes: int = DEFAULT_MAX_BYTES) -> dict:
    root = Path(project).resolve() if project is not None else Path.cwd().resolve()
    limit = _clamp_max_bytes(max_bytes)
    used: list[str] = list(CORE_LOCATORS)
    skipped: list[str] = []
    lines = list(CORE_LOCATORS)

    identity_candidates = (
        (root / ".chaos-engine" / "identity.md", ".chaos-engine/identity.md"),
        (root / "chaos-engine" / "identity.md", "chaos-engine/identity.md"),
    )
    identity_added = False
    for path, locator in identity_candidates:
        if path.is_file():
            used.append(locator)
            lines.append(locator)
            identity_added = True
            break
    if not identity_added:
        skipped.append("identity.md")

    text = "\n".join(lines) + "\n"
    text, truncated = _truncate_utf8(text, limit)
    encoded = text.encode("utf-8")
    return {
        "schemaVersion": SCHEMA_VERSION,
        "text": text,
        "bytes": len(encoded),
        "truncated": truncated,
        "used": used,
        "skipped": skipped,
    }


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--project", type=Path, default=None)
    parser.add_argument("--max-bytes", type=int, default=DEFAULT_MAX_BYTES)
    parser.add_argument("--json", action="store_true")
    args = parser.parse_args(argv)
    brief = build_brief(project=args.project, max_bytes=args.max_bytes)
    if args.json:
        print(json.dumps(brief, sort_keys=True))
    else:
        sys.stdout.write(brief["text"])
    return 0


if __name__ == "__main__":
    raise SystemExit(main())