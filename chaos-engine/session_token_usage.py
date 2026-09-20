#!/usr/bin/env python3
"""Session token usage ledger for Learning Session retrospectives (#5981).

Stdlib only. Records coarse local vs cloud token counts and a ballpark USD
estimate. Never stores model ids, provider names, routes, prompts, or paths.
"""

from __future__ import annotations

import argparse
import json
import re
import sys
from pathlib import Path

SCHEMA_VERSION = 1
CHANNELS = frozenset({"local", "cloud"})
# Coarse enums only — never persist vendor/model identifiers.
RUNTIME_CLASSES = frozenset(
    {
        "freetoken",
        "colibri",
        "openai-compat",
        "omniroute",
        "host-session",
        "other",
        "unknown",
    }
)
# Ballpark composite cloud rates (USD per 1M tokens). Not a quote; for
# retrospective comparison only. Local API cost is treated as $0.
DEFAULT_CLOUD_INPUT_USD_PER_M = 3.0
DEFAULT_CLOUD_OUTPUT_USD_PER_M = 15.0
DEFAULT_LOCAL_USD_PER_M = 0.0
SESSION_ID_RE = re.compile(r"^[A-Za-z0-9._:-]{1,128}$")
MAX_TOKENS_PER_EVENT = 50_000_000


def _state_dir(project: Path | None = None) -> Path:
    root = project if project is not None else Path.cwd()
    return root / ".chaos-engine-state" / "session-token-usage"


def _safe_session_id(session_id: str) -> str:
    if not isinstance(session_id, str) or not SESSION_ID_RE.fullmatch(session_id.strip()):
        raise ValueError("session id must be 1..128 safe characters")
    return session_id.strip()


def _ledger_path(session_id: str, project: Path | None = None) -> Path:
    safe = _safe_session_id(session_id)
    # Avoid path separators; session ids are already constrained.
    return _state_dir(project) / f"{safe}.json"


def _empty_ledger(session_id: str) -> dict[str, object]:
    return {
        "schemaVersion": SCHEMA_VERSION,
        "sessionId": session_id,
        "events": [],
        "totals": {
            "localPromptTokens": 0,
            "localCompletionTokens": 0,
            "cloudPromptTokens": 0,
            "cloudCompletionTokens": 0,
        },
    }


def _load(session_id: str, project: Path | None = None) -> dict[str, object]:
    path = _ledger_path(session_id, project)
    if not path.is_file():
        return _empty_ledger(_safe_session_id(session_id))
    try:
        raw = json.loads(path.read_text(encoding="utf-8"))
    except (OSError, UnicodeDecodeError, json.JSONDecodeError) as error:
        raise ValueError(f"session token ledger unreadable: {error}") from error
    if not isinstance(raw, dict) or raw.get("schemaVersion") != SCHEMA_VERSION:
        raise ValueError("session token ledger schema mismatch")
    events = raw.get("events")
    if not isinstance(events, list):
        raise ValueError("session token ledger events must be a list")
    return raw


def _write(ledger: dict[str, object], project: Path | None = None) -> Path:
    session_id = str(ledger["sessionId"])
    path = _ledger_path(session_id, project)
    path.parent.mkdir(parents=True, exist_ok=True)
    tmp = path.with_suffix(".tmp")
    tmp.write_text(json.dumps(ledger, indent=2, sort_keys=True) + "\n", encoding="utf-8")
    tmp.replace(path)
    return path


def _as_nonneg_int(value: object, field: str) -> int:
    if isinstance(value, bool) or not isinstance(value, int):
        raise ValueError(f"{field} must be an int")
    if value < 0 or value > MAX_TOKENS_PER_EVENT:
        raise ValueError(f"{field} out of range")
    return value


def record(
    session_id: str,
    *,
    channel: str,
    prompt_tokens: int,
    completion_tokens: int,
    runtime_class: str = "unknown",
    project: Path | None = None,
) -> dict[str, object]:
    """Append one usage event and refresh totals. Privacy-safe fields only."""
    if channel not in CHANNELS:
        raise ValueError("channel must be local or cloud")
    if runtime_class not in RUNTIME_CLASSES:
        raise ValueError("unsupported runtime class")
    prompt = _as_nonneg_int(prompt_tokens, "prompt_tokens")
    completion = _as_nonneg_int(completion_tokens, "completion_tokens")
    if prompt == 0 and completion == 0:
        raise ValueError("at least one of prompt_tokens or completion_tokens must be > 0")
    ledger = _load(session_id, project)
    events = ledger.get("events")
    if not isinstance(events, list):
        raise TypeError("ledger events must be a list")
    events.append(
        {
            "channel": channel,
            "runtimeClass": runtime_class,
            "promptTokens": prompt,
            "completionTokens": completion,
        }
    )
    totals = {
        "localPromptTokens": 0,
        "localCompletionTokens": 0,
        "cloudPromptTokens": 0,
        "cloudCompletionTokens": 0,
    }
    for event in events:
        if not isinstance(event, dict):
            continue
        ch = event.get("channel")
        pt = event.get("promptTokens")
        ct = event.get("completionTokens")
        if not isinstance(pt, int) or not isinstance(ct, int):
            continue
        if ch == "local":
            totals["localPromptTokens"] += pt
            totals["localCompletionTokens"] += ct
        elif ch == "cloud":
            totals["cloudPromptTokens"] += pt
            totals["cloudCompletionTokens"] += ct
    ledger["totals"] = totals
    path = _write(ledger, project)
    ledger["path"] = str(path)
    return ledger


def estimate_cost_usd(
    totals: dict[str, object],
    *,
    cloud_input_usd_per_m: float = DEFAULT_CLOUD_INPUT_USD_PER_M,
    cloud_output_usd_per_m: float = DEFAULT_CLOUD_OUTPUT_USD_PER_M,
    local_usd_per_m: float = DEFAULT_LOCAL_USD_PER_M,
) -> dict[str, object]:
    """Ballpark USD from totals. Local defaults to $0 API cost."""
    def _get(key: str) -> int:
        value = totals.get(key, 0)
        return value if isinstance(value, int) and not isinstance(value, bool) else 0

    local_prompt = _get("localPromptTokens")
    local_completion = _get("localCompletionTokens")
    cloud_prompt = _get("cloudPromptTokens")
    cloud_completion = _get("cloudCompletionTokens")
    local_tokens = local_prompt + local_completion
    cloud_tokens = cloud_prompt + cloud_completion
    local_cost = (local_tokens / 1_000_000.0) * local_usd_per_m
    cloud_cost = (
        (cloud_prompt / 1_000_000.0) * cloud_input_usd_per_m
        + (cloud_completion / 1_000_000.0) * cloud_output_usd_per_m
    )
    return {
        "localTokens": local_tokens,
        "cloudTokens": cloud_tokens,
        "localEstimatedUsd": round(local_cost, 6),
        "cloudEstimatedUsd": round(cloud_cost, 6),
        "totalEstimatedUsd": round(local_cost + cloud_cost, 6),
        "rateNote": "ballpark composite; not a vendor invoice",
        "cloudInputUsdPerMillion": cloud_input_usd_per_m,
        "cloudOutputUsdPerMillion": cloud_output_usd_per_m,
        "localUsdPerMillion": local_usd_per_m,
    }


def summarize(session_id: str, project: Path | None = None) -> dict[str, object]:
    """Return privacy-safe totals + cost estimate for Learning Session."""
    ledger = _load(session_id, project)
    raw_totals = ledger.get("totals")
    totals = raw_totals if isinstance(raw_totals, dict) else {}
    cost = estimate_cost_usd(totals)
    events = ledger.get("events") if isinstance(ledger.get("events"), list) else []
    runtime_classes = sorted(
        {
            str(event.get("runtimeClass"))
            for event in events
            if isinstance(event, dict) and event.get("runtimeClass") in RUNTIME_CLASSES
        }
    )
    return {
        "schemaVersion": SCHEMA_VERSION,
        "sessionId": _safe_session_id(session_id),
        "eventCount": len(events) if isinstance(events, list) else 0,
        "totals": totals,
        "cost": cost,
        "runtimeClasses": runtime_classes,
        "privacy": {
            "storesModelIds": False,
            "storesProviderNames": False,
            "storesPrompts": False,
            "storesPaths": False,
        },
    }


def format_retrospective(summary: dict[str, object]) -> str:
    """Short retrospective paragraph for the Learning Session user summary."""
    cost = summary.get("cost") if isinstance(summary.get("cost"), dict) else {}
    local_tokens = int(cost.get("localTokens") or 0)
    cloud_tokens = int(cost.get("cloudTokens") or 0)
    local_usd = float(cost.get("localEstimatedUsd") or 0.0)
    cloud_usd = float(cost.get("cloudEstimatedUsd") or 0.0)
    total_usd = float(cost.get("totalEstimatedUsd") or 0.0)
    if local_tokens == 0 and cloud_tokens == 0:
        return (
            "Token retrospective: no local/cloud usage events were recorded for "
            "this session (record via session_token_usage.py during work)."
        )
    classes = summary.get("runtimeClasses") if isinstance(summary.get("runtimeClasses"), list) else []
    class_note = f" Runtime classes: {', '.join(classes)}." if classes else ""
    return (
        f"Token retrospective: local={local_tokens} tokens (~${local_usd:.4f} API), "
        f"cloud={cloud_tokens} tokens (~${cloud_usd:.4f} ballpark), "
        f"combined~${total_usd:.4f}.{class_note}"
    )


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    sub = parser.add_subparsers(dest="command", required=True)

    rec = sub.add_parser("record", help="append a local or cloud usage event")
    rec.add_argument("--session-id", required=True)
    rec.add_argument("--channel", required=True, choices=sorted(CHANNELS))
    rec.add_argument("--prompt-tokens", type=int, required=True)
    rec.add_argument("--completion-tokens", type=int, required=True)
    rec.add_argument(
        "--runtime-class",
        default="unknown",
        choices=sorted(RUNTIME_CLASSES),
        help="coarse runtime class (never a model id)",
    )

    summ = sub.add_parser("summarize", help="JSON totals + cost estimate")
    summ.add_argument("--session-id", required=True)
    summ.add_argument("--text", action="store_true", help="print retrospective text instead of JSON")

    args = parser.parse_args(argv)
    try:
        if args.command == "record":
            ledger = record(
                args.session_id,
                channel=args.channel,
                prompt_tokens=args.prompt_tokens,
                completion_tokens=args.completion_tokens,
                runtime_class=args.runtime_class,
            )
            print(json.dumps({"ok": True, "totals": ledger["totals"]}, sort_keys=True))
            return 0
        if args.command == "summarize":
            summary = summarize(args.session_id)
            if args.text:
                print(format_retrospective(summary))
            else:
                print(json.dumps(summary, sort_keys=True))
            return 0
    except (OSError, ValueError) as error:
        print(str(error), file=sys.stderr)
        return 1
    print(f"unknown command: {args.command}", file=sys.stderr)
    return 2


if __name__ == "__main__":
    raise SystemExit(main())
