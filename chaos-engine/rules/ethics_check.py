#!/usr/bin/env python3
"""Person-only attestation receipt check.

The receipt is the log: counterpart, decision, and live_feed only. This tool
reads it and does not persist a copy, and it rejects quiz text.
"""

from __future__ import annotations

import json
import sys
from pathlib import Path

COUNTERPARTS = frozenset({"ai-accepted", "person-only", "unclear"})
DECISIONS = frozenset({"allow", "decline", "alternative"})
RECEIPT_KEYS = frozenset({"counterpart", "decision", "live_feed"})
MAX_RECEIPT_BYTES = 1024
DEFAULT_RECEIPT = Path(".chaos-engine-state") / "ethics-receipt.json"

# Fixture-backed shapes. Not a web denylist. Job applications, git push, and
# gh issue are excluded even when nearby text mentions an exam.
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
    "A receipt with counterpart person-only or live_feed is not a submit."
)


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


def matches_assessment(text: str) -> bool:
    folded = text.casefold()
    if any(marker in folded for marker in SAFE_MARKERS):
        return False
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


def assessment_block_reason(
    commands: tuple[str, ...],
    tool_input: object,
    cwd: Path | None = None,
    receipt: dict | None = None,
) -> str | None:
    """Deny an assessment-shaped command or URL unless the receipt allows it."""
    blob = _blob(commands, tool_input)
    if not matches_assessment(blob):
        return None
    if receipt is None and cwd is not None:
        receipt = read_receipt_or_none(Path(cwd) / DEFAULT_RECEIPT)
    if blocks_submit(receipt):
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
    if code != 0:
        print(DENY_REASON, file=sys.stderr)
        return code
    print(f"ethics-check: {receipt['decision']}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
