#!/usr/bin/env python3
"""Summarise per-dir ChaosEngine install / upgrade-session elapsed times.

Reads JSON objects (one per line or a single object) that contain ``elapsed_s``
and a session/path field. Used when installs look slow across many worktrees.
"""
from __future__ import annotations

import argparse
import json
import sys
from pathlib import Path


def _rows_from_obj(obj: object) -> list[dict]:
    if not isinstance(obj, dict):
        return []
    elapsed = obj.get("elapsed_s")
    if elapsed is None:
        return []
    path = (
        obj.get("session")
        or obj.get("session_path")
        or obj.get("root")
        or obj.get("path")
        or obj.get("project")
        or "?"
    )
    return [{"path": str(path), "elapsed_s": float(elapsed), "status": obj.get("status")}]


def load_rows(source: Path) -> list[dict]:
    rows: list[dict] = []
    if source.is_dir():
        files = sorted(source.rglob("*.json")) + sorted(source.rglob("*.jsonl"))
    else:
        files = [source]
    for path in files:
        text = path.read_text(encoding="utf-8")
        if path.suffix == ".jsonl" or "\n{" in text:
            for line in text.splitlines():
                line = line.strip()
                if not line:
                    continue
                try:
                    rows.extend(_rows_from_obj(json.loads(line)))
                except json.JSONDecodeError:
                    continue
        else:
            try:
                rows.extend(_rows_from_obj(json.loads(text)))
            except json.JSONDecodeError:
                for line in text.splitlines():
                    line = line.strip()
                    if not line:
                        continue
                    try:
                        rows.extend(_rows_from_obj(json.loads(line)))
                    except json.JSONDecodeError:
                        continue
    return rows


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--log", required=True, help="JSON/JSONL file or directory of receipts")
    parser.add_argument("--slow", type=float, default=25.0, help="flag rows slower than this (s)")
    args = parser.parse_args(argv)
    source = Path(args.log)
    if not source.exists():
        print(f"missing: {source}", file=sys.stderr)
        return 2
    rows = load_rows(source)
    if not rows:
        print(json.dumps({"rows": 0, "slow": []}))
        return 0
    rows.sort(key=lambda r: r["elapsed_s"], reverse=True)
    slow = [r for r in rows if r["elapsed_s"] >= args.slow]
    print(json.dumps({"rows": len(rows), "slow": slow, "top": rows[:10]}, indent=2))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
