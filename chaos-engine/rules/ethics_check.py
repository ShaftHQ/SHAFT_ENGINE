#!/usr/bin/env python3
"""Person-only attestation receipt check.

The receipt is the log: counterpart, decision, and live_feed only. This tool
reads it and does not persist a copy, and it rejects quiz text. Telemetry is
a separate enum-and-hash line, not a second receipt.
"""

from __future__ import annotations

import hashlib
import importlib.util
import json
import sys
from datetime import datetime, timezone
from pathlib import Path

COUNTERPARTS = frozenset({"ai-accepted", "person-only", "unclear"})
DECISIONS = frozenset({"allow", "decline", "alternative"})
RECEIPT_KEYS = frozenset({"counterpart", "decision", "live_feed"})
MAX_RECEIPT_BYTES = 1024
DEFAULT_RECEIPT = Path(".chaos-engine-state") / "ethics-receipt.json"

# Fixture-backed shapes. Not a web denylist. The word "exam" alone is not a shape.
# A safe marker does not hide a shape. A # comment does not run.
ASSESSMENT_SHAPES = (
    "closed-book",
    "hiring-quiz",
    "hiring_quiz",
    "assessment-quiz",
    "certification-exam",
    "proctored-exam",
    "answer-on-your-own",
    "live-answer-feed",
    "exam/submit",
    "quiz/submit",
    "exam-submit",
)
SAFE_MARKERS = (
    "git push",
    "gh issue",
    "job-application",
    "job_application",
    "job application",
)
DENY_REASON = (
    "Person-only attestation: do not sit the assessment, do not answer as the user would, "
    "and do not feed answers live. Offer preparation beforehand. "
    "A receipt with counterpart person-only or live_feed is not a submit. "
    "Card: companions/ethics-card.md."
)
TELEMETRY_RELATIVE = Path(".chaos-engine-state") / "ethics-telemetry.jsonl"
TELEMETRY_KEYS = frozenset({
    "counterpart",
    "decision",
    "live_feed",
    "outcome",
    "tool_name_sha256",
    "session_id",
    "timestamp",
})


def _validate(data: object) -> dict:
    if not isinstance(data, dict):
        raise ValueError("receipt must be a JSON object")
    if set(data) != RECEIPT_KEYS:
        raise ValueError("receipt must contain only counterpart, decision, and live_feed")
    counterpart = data["counterpart"]
    decision = data["decision"]
    live_feed = data["live_feed"]
    if counterpart not in COUNTERPARTS:
        raise ValueError("counterpart must be ai-accepted, person-only, or unclear")
    if decision not in DECISIONS:
        raise ValueError("decision must be allow, decline, or alternative")
    if not isinstance(live_feed, bool):
        raise ValueError("live_feed must be a bool")
    return {"counterpart": counterpart, "decision": decision, "live_feed": live_feed}


def load_receipt(path: Path) -> dict:
    """Read one receipt. Refuse oversized files so quiz text cannot hide here."""
    if path.stat().st_size > MAX_RECEIPT_BYTES:
        raise ValueError("receipt is too large")
    return _validate(json.loads(path.read_text(encoding="utf-8")))


def exit_code(receipt: dict) -> int:
    """0 for allow and for alternative. 2 for a person-only submit, a decline, or a live feed."""
    if receipt["live_feed"]:
        return 2
    if receipt["counterpart"] == "person-only" and receipt["decision"] == "allow":
        return 2
    if receipt["decision"] == "decline":
        return 2
    return 0


def _blob(commands: tuple[str, ...], tool_input: object) -> str:
    parts = [command for command in commands if command]
    if isinstance(tool_input, dict):
        for key in ("url", "uri", "href", "command", "cmd"):
            value = tool_input.get(key)
            if isinstance(value, str) and value.strip():
                parts.append(value)
    elif isinstance(tool_input, str) and tool_input.strip():
        parts.append(tool_input)
    return "\n".join(parts)


def _odd_backslash_run(text: str, index: int) -> bool:
    """True when an odd run of backslashes immediately precedes index."""
    slashes = 0
    look = index - 1
    while look >= 0 and text[look] == "\\":
        slashes += 1
        look -= 1
    return slashes % 2 == 1


def _without_unquoted_comment(text: str) -> str:
    """Drop a # comment through end of line. A hash inside quotes stays.

    A hash starts a comment at index 0, or after space, tab, LF, CR, semicolon,
    pipe, or ampersand when that separator is not escaped. Escaped means an odd
    run of backslashes immediately before the separator; an even run is not.
    A single quote always ends. A double quote ends only when the preceding
    backslash run is even.
    """
    kept: list[str] = []
    quote = ""
    index = 0
    length = len(text)
    while index < length:
        char = text[index]
        if quote == "'":
            kept.append(char)
            if char == "'":
                quote = ""
            index += 1
            continue
        if quote == '"':
            kept.append(char)
            if char == '"' and not _odd_backslash_run(text, index):
                quote = ""
            index += 1
            continue
        if char in "'\"":
            quote = char
            kept.append(char)
            index += 1
            continue
        if char == "#" and (
            index == 0
            or (
                text[index - 1] in " \t\n\r;|&"
                and not _odd_backslash_run(text, index - 1)
            )
        ):
            while index < length and text[index] not in "\n\r":
                index += 1
            continue
        kept.append(char)
        index += 1
    return "".join(kept)


def matches_assessment(text: str) -> bool:
    """True when operative text contains a shape. A safe marker never hides one."""
    if not SAFE_MARKERS:
        raise ValueError("SAFE_MARKERS empty")
    folded = _without_unquoted_comment(text).casefold()
    return any(shape in folded for shape in ASSESSMENT_SHAPES)


def read_receipt_or_none(path: Path) -> dict | None:
    try:
        return load_receipt(path)
    except (OSError, ValueError, json.JSONDecodeError):
        return None


def blocks_submit(receipt: dict | None) -> bool:
    """True when the receipt is missing, person-only, unclear, or a live feed."""
    if receipt is None:
        return True
    if receipt["live_feed"]:
        return True
    if receipt["counterpart"] in {"person-only", "unclear"}:
        return True
    if receipt["decision"] != "allow":
        return True
    return False


def _client_name_scrub():
    cached = getattr(_client_name_scrub, "module", None)
    if cached is not None:
        return cached
    path = Path(__file__).resolve().parents[1] / "client_name_scrub.py"
    spec = importlib.util.spec_from_file_location("chaos_engine_client_name_scrub", path)
    if spec is None or spec.loader is None:
        return None
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    _client_name_scrub.module = module
    return module


def _telemetry_record(
    receipt: dict | None,
    outcome: str,
    tool_name: str,
    session_id: str,
) -> dict:
    record = {
        "outcome": outcome,
        "tool_name_sha256": hashlib.sha256(tool_name.encode("utf-8")).hexdigest(),
        "session_id": session_id,
        "timestamp": datetime.now(timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ"),
    }
    if isinstance(receipt, dict):
        for key in ("counterpart", "decision", "live_feed"):
            if key in receipt:
                record[key] = receipt[key]
    enums = [key for key in ("counterpart", "decision", "live_feed") if key in record]
    if not enums and outcome not in {"allow", "deny"}:
        return {}
    return {key: record[key] for key in TELEMETRY_KEYS if key in record}


def _append_ethics_telemetry(cwd: Path | None, record: dict | None) -> None:
    """Enums and hashes only. A denylist hit writes nothing."""
    if not isinstance(record, dict) or not record:
        return
    enums = [key for key in ("counterpart", "decision", "live_feed") if key in record]
    if not enums and record.get("outcome") not in {"allow", "deny"}:
        return
    slim = {key: record[key] for key in TELEMETRY_KEYS if key in record}
    text = json.dumps(slim, sort_keys=True, separators=(",", ":"))
    root = Path(cwd) if cwd is not None else Path.cwd()
    scrub = _client_name_scrub()
    if scrub is None:
        return
    if scrub.findings_in_text(text, scrub.load_denylist(root), surface="ethics-telemetry"):
        return
    path = root / TELEMETRY_RELATIVE
    try:
        path.parent.mkdir(parents=True, exist_ok=True)
        with path.open("a", encoding="utf-8") as handle:
            handle.write(text + "\n")
    except OSError:
        return


def assessment_block_reason(
    commands: tuple[str, ...],
    tool_input: object,
    cwd: Path | None = None,
    receipt: dict | None = None,
    *,
    tool_name: str = "",
    session_id: str = "",
) -> str | None:
    """Deny an assessment-shaped command or URL unless the receipt allows it."""
    blob = _blob(commands, tool_input)
    if not matches_assessment(blob):
        return None
    if receipt is None and cwd is not None:
        receipt = read_receipt_or_none(Path(cwd) / DEFAULT_RECEIPT)
    blocked = blocks_submit(receipt)
    _append_ethics_telemetry(
        cwd,
        _telemetry_record(
            receipt if isinstance(receipt, dict) else None,
            "deny" if blocked else "allow",
            tool_name,
            session_id,
        ),
    )
    if blocked:
        return DENY_REASON
    return None


def main(arguments: list[str] | None = None) -> int:
    args = list(sys.argv[1:] if arguments is None else arguments)
    path = Path(args[0]) if args else Path.cwd() / DEFAULT_RECEIPT
    try:
        receipt = load_receipt(path)
    except (OSError, ValueError, json.JSONDecodeError) as error:
        print(f"ethics-check: {error}", file=sys.stderr)
        return 2
    code = exit_code(receipt)
    _append_ethics_telemetry(
        Path.cwd(),
        _telemetry_record(receipt, "deny" if code else "allow", "", ""),
    )
    if code != 0:
        print(DENY_REASON, file=sys.stderr)
        return code
    print(f"ethics-check: {receipt['decision']}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
