#!/usr/bin/env python3
"""Portable Learning Session finalizer inside `.chaos-engine/` (#5625).

Issues-first. Does **not** open draft PRs (phase-2 opt-in is out of scope).
Privacy gates remain those in colocated `learning.py`.
Works without monorepo `scripts/agents/learning_session.py`.
"""

from __future__ import annotations

import argparse
import importlib.util
import json
import sys
from pathlib import Path


def _load_learning():
    path = Path(__file__).resolve().with_name("learning.py")
    spec = importlib.util.spec_from_file_location("chaos_engine_learning_portable", path)
    if spec is None or spec.loader is None:
        raise RuntimeError("learning.py missing from installed core")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def finalize(session_id: str, *, disposition: str = "issues-first") -> dict[str, object]:
    """Record a portable completion receipt. Never auto-opens draft PRs."""
    if not isinstance(session_id, str) or not session_id.strip():
        raise ValueError("session id required")
    if disposition not in {"issues-first", "no-durable", "blocked"}:
        raise ValueError("unsupported disposition")
    learning = _load_learning()
    # Privacy invariants unchanged — any queue payload must pass learning gates.
    receipt = {
        "schemaVersion": 1,
        "kind": "learning-session-portable-finalize",
        "sessionId": session_id.strip()[:128],
        "disposition": disposition,
        "draftPrs": False,
        "policy": "issues-first",
        "privacyModule": "learning.py",
    }
    # Touch privacy module self-check surface without filing.
    if hasattr(learning, "PRIVACY_SCHEMA_VERSION") or hasattr(learning, "queue_learning"):
        receipt["privacyReady"] = True
    state = Path.cwd() / ".chaos-engine-state" / "learning-session"
    state.mkdir(parents=True, exist_ok=True)
    out = state / f"{session_id.strip()[:64]}.completion.json"
    out.write_text(json.dumps(receipt, indent=2, sort_keys=True) + "\n", encoding="utf-8")
    receipt["path"] = str(out)
    return receipt


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    sub = parser.add_subparsers(dest="command", required=True)
    final = sub.add_parser("finalize")
    final.add_argument("--session-id", required=True)
    final.add_argument(
        "--disposition",
        default="issues-first",
        choices=("issues-first", "no-durable", "blocked"),
    )
    # Alias accepted by guard finalize-command matcher
    runtime = sub.add_parser("finalize-runtime")
    runtime.add_argument("--session-id", required=True)
    runtime.add_argument(
        "--disposition",
        default="issues-first",
        choices=("issues-first", "no-durable", "blocked"),
    )
    args = parser.parse_args(argv)
    try:
        result = finalize(args.session_id, disposition=args.disposition)
    except (OSError, RuntimeError, ValueError) as error:
        print(str(error), file=sys.stderr)
        return 1
    print(json.dumps(result, sort_keys=True))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
