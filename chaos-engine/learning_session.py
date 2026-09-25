#!/usr/bin/env python3
"""Portable Learning Session finalizer (#5625); issues-first, no auto draft PRs."""

from __future__ import annotations

import argparse
import contextlib
import hashlib
import importlib.util
import json
import os
import sys
from pathlib import Path


def _load_sibling(name: str):
    path = Path(__file__).resolve().with_name(name)
    spec = importlib.util.spec_from_file_location(f"chaos_engine_{name}", path)
    if spec is None or spec.loader is None:
        raise RuntimeError(f"{name} missing from installed core")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def _load_learning():
    return _load_sibling("learning.py")


def _extract_heuristics(session_id: str, limit: int = 3) -> list[dict]:
    """Pull ≤N privacy-safe heuristics from recent queue lessons (#5656)."""
    try:
        heuristics = _load_sibling("heuristics.py")
        learning = _load_learning()
    except RuntimeError:
        return []
    state = Path.cwd() / ".chaos-engine-state" / "learning"
    lessons: list[str] = []
    try:
        document = learning.queue_document(state)
    except (OSError, ValueError):
        document = {"items": []}
    items = document.get("items") if isinstance(document, dict) else []
    if isinstance(items, list):
        for item in reversed(items):
            if not isinstance(item, dict):
                continue
            lesson = item.get("lesson")
            title = item.get("title")
            if isinstance(lesson, str) and lesson.strip():
                lessons.append(lesson.strip())
            elif isinstance(title, str) and title.strip():
                lessons.append(title.strip())
            if len(lessons) >= limit:
                break
    return heuristics.extract_from_lessons(
        lessons,
        limit=limit,
        project=Path.cwd(),
        source=f"session-{session_id[:24]}",
    )



def _drain_significance(session_id: str) -> list[dict]:
    """Deferred Learning Session path for significance marks (#5658)."""
    try:
        significance = _load_sibling("significance.py")
    except RuntimeError:
        return []
    drained = significance.drain_marks(project=Path.cwd())
    return drained if isinstance(drained, list) else []





def _token_usage_summary(session_id: str) -> dict[str, object] | None:
    """Attach privacy-safe local vs cloud token retrospective (#5981)."""
    try:
        usage = _load_sibling("session_token_usage.py")
    except RuntimeError:
        return None
    try:
        return usage.summarize(session_id)
    except (OSError, ValueError):
        return None


def protect_identity_truth_if_present(project, before: bytes | None, after: bytes) -> bytes:
    """Preserve identity.md Truth markers across Learning proposals (#5807)."""
    import importlib.util as _ilu
    from pathlib import Path as _P

    path = _P(__file__).resolve().with_name("identity_md.py")
    if not path.is_file():
        return after
    spec = _ilu.spec_from_file_location("ce_identity_learn", path)
    if spec is None or spec.loader is None:
        return after
    mod = _ilu.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod.learning_may_write_identity(before, after)


# #6201: instruction-only hosts have no read gate or Stop hook; their research
# receipt `retrieve:` field is checked here and flagged, never blocking.
INSTRUCTION_ONLY_HOSTS = ("opencode", "cursor", "grok-bot")
RESEARCH_RECEIPT_SINK = ".chaos-engine-state/research-receipt.md"
RETRIEVE_LEDGER = ".chaos-engine-state/retrieve-justification.json"


def _ledger_has_retrieve(project: Path) -> bool:
    try:
        payload = json.loads((project / RETRIEVE_LEDGER).read_text(encoding="utf-8"))
    except (OSError, UnicodeDecodeError, ValueError):
        return False
    if not isinstance(payload, dict):
        return False
    return bool(payload.get("citations")) or bool(payload.get("outcomes"))


def retrieve_receipt_check(project: Path) -> dict[str, str]:
    """Find a `retrieve:` receipt in the sink file or the retrieve ledger."""
    gate = None
    path = Path(__file__).resolve().parent / "hooks" / "retrieve_justification.py"
    spec = importlib.util.spec_from_file_location("chaos_engine_receipt_gate", path)
    if path.is_file() and spec is not None and spec.loader is not None:
        gate = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(gate)
    try:
        text = (project / RESEARCH_RECEIPT_SINK).read_text(encoding="utf-8")
    except (OSError, UnicodeDecodeError):
        text = ""
    field = gate.retrieve_receipt_field(text) if gate is not None and text else None
    if field:
        return {"status": "present", "source": RESEARCH_RECEIPT_SINK, "field": field}
    if _ledger_has_retrieve(project):
        return {"status": "present", "source": "ledger"}
    return {
        "status": "missing",
        "fixNext": f"record `retrieve: used|skipped(<reason>)|exempt(harness)` in {RESEARCH_RECEIPT_SINK}",
    }


def finalize(
    session_id: str,
    *,
    disposition: str = "issues-first",
    extract_heuristics: bool = True,
    host: str | None = None,
) -> dict[str, object]:
    """Record a portable completion receipt. Never auto-opens draft PRs."""
    if not isinstance(session_id, str) or not session_id.strip():
        raise ValueError("session id required")
    if disposition not in {"issues-first", "no-durable", "blocked"}:
        raise ValueError("unsupported disposition")
    learning = _load_learning()
    receipt = {
        "schemaVersion": 1,
        "kind": "learning-session-portable-finalize",
        "sessionId": session_id.strip()[:128],
        "disposition": disposition,
        "draftPrs": False,
        "policy": "issues-first",
        "privacyModule": "learning.py",
    }
    if hasattr(learning, "PRIVACY_SCHEMA_VERSION") or hasattr(learning, "queue_learning"):
        receipt["privacyReady"] = True
    heuristics_added: list[dict] = []
    if extract_heuristics and disposition == "issues-first":
        heuristics_added = _extract_heuristics(session_id.strip())
        receipt["heuristicsExtracted"] = len(heuristics_added)
    significance_drained = _drain_significance(session_id.strip())
    receipt["significanceDrained"] = len(significance_drained)
    if significance_drained:
        receipt["significanceKinds"] = sorted(
            {
                str(item.get("kind"))
                for item in significance_drained
                if isinstance(item, dict) and item.get("kind")
            }
        )
    token_summary = _token_usage_summary(session_id.strip())
    if isinstance(token_summary, dict):
        retrospective = None
        try:
            usage = _load_sibling("session_token_usage.py")
            retrospective = usage.format_retrospective(token_summary)
        except (RuntimeError, OSError, ValueError, TypeError):
            retrospective = None
        receipt["tokenUsage"] = {
            "totals": token_summary.get("totals"),
            "cost": token_summary.get("cost"),
            "eventCount": token_summary.get("eventCount"),
            "runtimeClasses": token_summary.get("runtimeClasses"),
            "retrospective": retrospective,
        }

    chosen_host = (host or os.environ.get("CHAOS_ENGINE_HOST") or "").strip().casefold()
    if chosen_host in INSTRUCTION_ONLY_HOSTS:
        check = retrieve_receipt_check(Path.cwd())
        receipt["retrieveReceipt"] = check
        if check["status"] == "missing":
            receipt["flags"] = ["missing-retrieve-receipt"]

    state = Path.cwd() / ".chaos-engine-state" / "learning-session"
    state.mkdir(parents=True, exist_ok=True)
    out = state / f"{session_id.strip()[:64]}.completion.json"
    out.write_text(json.dumps(receipt, indent=2, sort_keys=True) + "\n", encoding="utf-8")
    receipt["path"] = str(out)
    digest = hashlib.sha256(
        json.dumps(receipt, sort_keys=True, separators=(",", ":")).encode("utf-8")
    ).hexdigest()[:16]
    receipt["digest"] = digest
    with contextlib.suppress(Exception):
        counters = _load_sibling("learning_counters.py")
        counters.record_learning_session_digest(digest, project=Path.cwd())
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
    final.add_argument(
        "--silent",
        action="store_true",
        help="silent-on-success / errors-only (#5654)",
    )
    final.add_argument(
        "--no-heuristics",
        action="store_true",
        help="skip ERL heuristic extract (#5656)",
    )
    final.add_argument(
        "--host", default=None, help="host id; instruction-only hosts get a retrieve receipt check"
    )
    runtime = sub.add_parser("finalize-runtime")
    runtime.add_argument("--session-id", required=True)
    runtime.add_argument(
        "--disposition",
        default="issues-first",
        choices=("issues-first", "no-durable", "blocked"),
    )
    runtime.add_argument("--silent", action="store_true")
    runtime.add_argument("--no-heuristics", action="store_true")
    runtime.add_argument("--host", default=None)
    args = parser.parse_args(argv)
    try:
        result = finalize(
            args.session_id,
            disposition=args.disposition,
            extract_heuristics=not args.no_heuristics,
            host=args.host,
        )
    except (OSError, RuntimeError, ValueError) as error:
        if getattr(args, "silent", False):
            print(str(error), file=sys.stderr)
            return 2
        print(str(error), file=sys.stderr)
        return 1
    if getattr(args, "silent", False):
        return 0
    print(json.dumps(result, sort_keys=True))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
