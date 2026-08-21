#!/usr/bin/env python3
"""Portable task-scoped reflection state in the existing session ledger."""

from __future__ import annotations

import argparse
import hashlib
import hmac
import json
import os
import re
import secrets
import tempfile
from datetime import UTC, datetime
from pathlib import Path


SCHEMA_VERSION = 1
TRIGGERS = frozenset(
    {
        "second-failure",
        "repeated-fingerprint",
        "third-fix",
        "platform-disagreement",
        "review-repeat",
        "user-correction",
        "guard-repeat",
        "safety-incident",
        "scope-expansion",
        "premise-invalidated",
        "long-session-completion",
    }
)
DISPOSITIONS = frozenset(
    {"guidance-fixed", "issue-filed", "knowledge-recorded", "nothing-durable", "degraded"}
)
NON_ATTEMPT_REASONS = frozenset({"setup-error", "syntax-error", "capability-probe"})
_SAFE_TEXT = re.compile(r"^[^\r\n\x00]{1,240}$")
_ABSOLUTE_PATH = re.compile(
    r"(?i)(?:[a-z]:[\\/]|\\\\[^\\/\s]+[\\/][^\\/\s]+|(?:^|\s)/(?!/)|"
    r"(?:home|users?|workspace|server)[\\/:-][^\s]+)"
)
_SECRET = re.compile(
    r"(?i)(?:(?:password|secret|access[_-]?token|token|credential|api[_-]?key|"
    r"authorization)\s*(?:=|:)\s*\S+|bearer\s+\S+)"
)
_GITHUB_ISSUE_URL = re.compile(r"https://github\.com/[^/]+/[^/]+/issues/[1-9]\d*")


def scope_session_id(session_id: str, agent_id: object = None) -> str:
    """Return root session identity or an isolated, non-reversible agent scope."""
    root = str(session_id or "").strip()
    agent = str(agent_id or "").strip()
    if not agent:
        return root
    digest = hashlib.sha256(agent.encode("utf-8")).hexdigest()[:24]
    return f"{root}:agent:{digest}"


def ledger_path(session_id: str) -> Path:
    """Resolve the same external, session-hashed ledger used by guard.py."""
    key = hashlib.sha256(session_id.strip().encode("utf-8")).hexdigest()[:32]
    base = os.environ.get("TMPDIR") or os.environ.get("TEMP") or os.environ.get("TMP")
    if not base or not os.path.isabs(base):
        base = tempfile.gettempdir()
    return Path(base, "agent-session-ledger", f"{key}.json")


def append_entry(session_id: str, entry: dict) -> bool:
    if not isinstance(session_id, str) or not session_id.strip():
        return False
    path = ledger_path(session_id)
    try:
        path.parent.mkdir(parents=True, exist_ok=True)
        with path.open("a", encoding="utf-8") as handle:
            handle.write(json.dumps(entry, separators=(",", ":"), sort_keys=True) + "\n")
    except (OSError, TypeError, ValueError):
        return False
    return True


def _token_path(session_id: str) -> Path:
    return ledger_path(session_id).with_suffix(".token")


def _read_session_token(session_id: str) -> str | None:
    try:
        token = _token_path(session_id).read_text(encoding="utf-8").strip()
    except (OSError, UnicodeDecodeError):
        return None
    return token if len(token) >= 24 else None


def _ensure_session_token(session_id: str) -> str | None:
    if not isinstance(session_id, str) or not session_id.strip():
        return None
    existing = _read_session_token(session_id)
    if existing:
        return existing
    path = _token_path(session_id)
    token = secrets.token_urlsafe(24)
    try:
        path.parent.mkdir(parents=True, exist_ok=True)
        descriptor = os.open(path, os.O_WRONLY | os.O_CREAT | os.O_EXCL, 0o600)
        with os.fdopen(descriptor, "w", encoding="utf-8") as handle:
            handle.write(token)
    except FileExistsError:
        return _read_session_token(session_id)
    except OSError:
        return None
    return token


def _session_hash(session_id: str) -> str:
    return hashlib.sha256(session_id.strip().encode("utf-8")).hexdigest()[:24]


def _checkpoint_digest(active: list[dict]) -> str:
    bindings = [
        {
            "kind": item.get("kind"),
            "failureId": item.get("failureId"),
            "fingerprint": item.get("fingerprint"),
            "trigger": item.get("trigger"),
        }
        for item in active
    ]
    return hashlib.sha256(
        json.dumps(bindings, sort_keys=True, separators=(",", ":")).encode("utf-8")
    ).hexdigest()[:24]


def _receipt_hash(session_id: str, entry: dict) -> str | None:
    token = _read_session_token(session_id)
    if token is None:
        return None
    payload = {key: value for key, value in entry.items() if key != "receiptHash"}
    return hmac.new(
        token.encode("utf-8"),
        json.dumps(payload, sort_keys=True, separators=(",", ":")).encode("utf-8"),
        hashlib.sha256,
    ).hexdigest()


def _receipt_clears_active(session_id: str, entry: dict, active: list[dict]) -> bool:
    supplied_hash = entry.get("receiptHash")
    return bool(
        isinstance(supplied_hash, str)
        and hmac.compare_digest(supplied_hash, _receipt_hash(session_id, entry) or "")
        and entry.get("sessionHash") == _session_hash(session_id)
        and entry.get("checkpointDigest") == _checkpoint_digest(active)
    )


def entries(session_id: str) -> list[dict]:
    if not isinstance(session_id, str) or not session_id.strip():
        return []
    path = ledger_path(session_id)
    if not path.is_file():
        return []
    try:
        lines = path.read_text(encoding="utf-8").splitlines()
    except (OSError, UnicodeDecodeError):
        return []
    found: list[dict] = []
    decoder = json.JSONDecoder()
    for line in lines:
        position = 0
        text = line.strip()
        while position < len(text):
            try:
                item, position = decoder.raw_decode(text, position)
            except ValueError:
                break
            if isinstance(item, dict):
                found.append(item)
            while position < len(text) and text[position] in " \t,":
                position += 1
    return found


def record_session_start(session_id: str, observed_at: str | None = None) -> str | None:
    token = _ensure_session_token(session_id)
    if token is None:
        return None
    if any(item.get("kind") == "session-start" for item in entries(session_id)):
        return token
    recorded = append_entry(
        session_id,
        {
            "schemaVersion": SCHEMA_VERSION,
            "kind": "session-start",
            "observedAt": observed_at or datetime.now(UTC).isoformat(),
        },
    )
    return token if recorded else None


def _bounded_token(value: object, fallback: str = "unknown") -> str:
    raw = str(value or "").casefold()
    if _ABSOLUTE_PATH.search(raw) or _SECRET.search(raw):
        return "digest-" + hashlib.sha256(raw.encode("utf-8")).hexdigest()[:16]
    rendered = re.sub(r"[^a-z0-9_.:-]+", "-", raw).strip("-")
    return rendered[:80] or fallback


def record_failure(
    session_id: str,
    *,
    phase: object,
    target: object,
    failure_class: object,
    platform: object = "unknown",
    invariant: object = "command-outcome",
    head: object = "unknown",
    attempted: bool = True,
    observation_id: object = None,
) -> dict | None:
    _ensure_session_token(session_id)
    observation = None
    if observation_id:
        observation = hashlib.sha256(str(observation_id).encode("utf-8")).hexdigest()[:24]
        duplicate = next(
            (
                item
                for item in entries(session_id)
                if item.get("kind") == "task-failure"
                and item.get("observationId") == observation
            ),
            None,
        )
        if duplicate is not None:
            return duplicate
    fields = {
        "phase": _bounded_token(phase),
        "target": _bounded_token(target),
        "failureClass": _bounded_token(failure_class),
        "platform": _bounded_token(platform),
        "invariant": _bounded_token(invariant),
        "head": _bounded_token(head),
    }
    fingerprint = hashlib.sha256(
        json.dumps(fields, sort_keys=True, separators=(",", ":")).encode("utf-8")
    ).hexdigest()[:24]
    entry = {
        "schemaVersion": SCHEMA_VERSION,
        "kind": "task-failure",
        "failureId": "failure-" + (observation or secrets.token_hex(12)),
        "fingerprint": fingerprint,
        "attempted": bool(attempted),
        "observedAt": datetime.now(UTC).isoformat(),
        **fields,
    }
    if observation:
        entry["observationId"] = observation
    return entry if append_entry(session_id, entry) else None


def mark_non_attempt(session_id: str, failure_id: str, reason: str) -> bool:
    if reason not in NON_ATTEMPT_REASONS:
        raise ValueError("reason is not a supported non-attempt enum value")
    failures = {
        item.get("failureId")
        for item in entries(session_id)
        if item.get("kind") == "task-failure"
    }
    if failure_id not in failures:
        raise ValueError("failureId does not exist in this session")
    if any(
        item.get("kind") == "failure-disposition" and item.get("failureId") == failure_id
        for item in entries(session_id)
    ):
        return True
    return append_entry(
        session_id,
        {
            "schemaVersion": SCHEMA_VERSION,
            "kind": "failure-disposition",
            "failureId": failure_id,
            "disposition": "non-attempt",
            "reason": reason,
            "observedAt": datetime.now(UTC).isoformat(),
        },
    )


def record_trigger(session_id: str, trigger: str, fingerprint: str = "manual") -> bool:
    if trigger not in TRIGGERS or trigger == "long-session-completion":
        raise ValueError("trigger is not an actionable checkpoint enum value")
    return append_entry(
        session_id,
        {
            "schemaVersion": SCHEMA_VERSION,
            "kind": "reflection-trigger",
            "trigger": trigger,
            "fingerprint": _bounded_token(fingerprint, "manual"),
            "observedAt": datetime.now(UTC).isoformat(),
        },
    )


def record_platform_outcome(
    session_id: str, *, target: str, platform: str, outcome: str
) -> bool:
    if outcome not in {"passed", "failed"}:
        raise ValueError("platform outcome must be passed or failed")
    recorded = append_entry(
        session_id,
        {
            "schemaVersion": SCHEMA_VERSION,
            "kind": "platform-outcome",
            "target": _bounded_token(target),
            "platform": _bounded_token(platform),
            "outcome": outcome,
            "observedAt": datetime.now(UTC).isoformat(),
        },
    )
    if not recorded:
        return False
    outcomes = [
        item
        for item in entries(session_id)
        if item.get("kind") == "platform-outcome" and item.get("target") == _bounded_token(target)
    ]
    disagreements = 0
    for index, left in enumerate(outcomes):
        if any(
            right.get("platform") != left.get("platform")
            and right.get("outcome") != left.get("outcome")
            for right in outcomes[index + 1 :]
        ):
            disagreements += 1
    if disagreements >= 2 and not any(
        item.get("kind") == "reflection-trigger" and item.get("trigger") == "platform-disagreement"
        for item in entries(session_id)
    ):
        record_trigger(session_id, "platform-disagreement", _bounded_token(target))
    return True


def record_activity(session_id: str, activity: str) -> bool:
    return append_entry(
        session_id,
        {
            "schemaVersion": SCHEMA_VERSION,
            "kind": "task-activity",
            "activity": _bounded_token(activity),
            "observedAt": datetime.now(UTC).isoformat(),
        },
    )


def active_entries(session_id: str) -> list[dict]:
    """Return attempted outcomes/triggers after the last valid bound receipt."""
    active: list[dict] = []
    for item in entries(session_id):
        kind = item.get("kind")
        if kind == "reflection-receipt":
            if _receipt_clears_active(session_id, item, active):
                active = []
        elif kind == "task-failure" and item.get("attempted") is not False:
            active.append(item)
        elif kind == "reflection-trigger":
            active.append(item)
        elif kind == "failure-disposition" and item.get("disposition") == "non-attempt":
            active = [
                candidate
                for candidate in active
                if candidate.get("failureId") != item.get("failureId")
            ]
    return active


def pending_checkpoint(session_id: str) -> dict | None:
    """Reduce ledger records to the currently required reflection checkpoint."""
    active = active_entries(session_id)
    explicit = next((item for item in reversed(active) if item.get("kind") == "reflection-trigger"), None)
    if explicit is not None:
        return {
            "depth": "deep",
            "trigger": explicit["trigger"],
            "failureFingerprints": sorted(
                {str(item.get("fingerprint", "manual")) for item in active}
            ),
            "attemptCount": len(active),
        }
    if len(active) < 2:
        return None
    fingerprints = [str(item.get("fingerprint", "manual")) for item in active]
    same = len(set(fingerprints)) == 1
    return {
        "depth": "deep" if same else "task",
        "trigger": "repeated-fingerprint" if same else "second-failure",
        "failureFingerprints": sorted(set(fingerprints)),
        "attemptCount": len(active),
    }


def session_elapsed_seconds(session_id: str, now: datetime | None = None) -> float | None:
    start = next((item for item in entries(session_id) if item.get("kind") == "session-start"), None)
    if not start:
        return None
    try:
        begun = datetime.fromisoformat(str(start["observedAt"]).replace("Z", "+00:00"))
        current = now or datetime.now(UTC)
        if begun.tzinfo is None:
            begun = begun.replace(tzinfo=UTC)
        return max(0.0, (current.astimezone(UTC) - begun.astimezone(UTC)).total_seconds())
    except (KeyError, TypeError, ValueError):
        return None


def has_valid_terminal_receipt(session_id: str) -> bool:
    active: list[dict] = []
    terminal = False
    for item in entries(session_id):
        kind = item.get("kind")
        if kind == "task-failure" and item.get("attempted") is not False:
            active.append(item)
            terminal = False
        elif kind == "reflection-trigger":
            active.append(item)
            terminal = False
        elif kind in {"task-activity", "platform-outcome", "failure-disposition"}:
            terminal = False
        elif kind == "reflection-receipt" and _receipt_clears_active(
            session_id, item, active
        ):
            if item.get("trigger") == "long-session-completion":
                terminal = True
            else:
                active = []
    return terminal


def _safe_text(name: str, value: object, *, allow_github_issue: bool = False) -> str:
    rendered = str(value or "").strip()
    if not _SAFE_TEXT.fullmatch(rendered):
        raise ValueError(f"{name} must be 1-240 characters on one line")
    issue_url = allow_github_issue and _GITHUB_ISSUE_URL.fullmatch(rendered)
    if (not issue_url and _ABSOLUTE_PATH.search(rendered)) or _SECRET.search(rendered):
        raise ValueError(f"{name} contains forbidden path or credential-shaped text")
    return rendered


def _validate_receipt_shape(receipt: dict) -> None:
    allowed_fields = {
        "schemaVersion", "taskId", "trigger", "failureFingerprints",
        "failedAssumption", "approachesCompared", "chosenExperiment",
        "changedApproach", "proofCommandOrCheck", "proofOutcome",
        "durableDisposition", "issue",
    }
    unknown = set(receipt) - allowed_fields
    if unknown:
        raise ValueError("receipt contains unknown fields: " + ", ".join(sorted(unknown)))
    if receipt.get("schemaVersion") != SCHEMA_VERSION:
        raise ValueError(f"schemaVersion must be {SCHEMA_VERSION}")


def _validate_receipt_context(session_id: str, receipt: dict) -> tuple[dict | None, str]:
    checkpoint = pending_checkpoint(session_id)
    trigger = str(receipt.get("trigger", ""))
    if trigger not in TRIGGERS:
        raise ValueError("trigger is not a supported enum value")
    if trigger != "long-session-completion" and checkpoint is None:
        raise ValueError("no reflection checkpoint is pending")
    if checkpoint is not None and trigger != checkpoint["trigger"]:
        raise ValueError("trigger does not match the pending checkpoint")
    if trigger == "long-session-completion":
        elapsed = session_elapsed_seconds(session_id)
        if elapsed is None or elapsed <= 3600:
            raise ValueError("long-session-completion requires elapsed time over one hour")
    return checkpoint, trigger


def record_receipt(session_id: str, receipt: dict, session_token: str) -> dict:
    expected_token = _read_session_token(session_id)
    if expected_token is None or not hmac.compare_digest(expected_token, session_token):
        raise ValueError("session token is missing or invalid")
    _validate_receipt_shape(receipt)
    checkpoint, trigger = _validate_receipt_context(session_id, receipt)
    disposition = str(receipt.get("durableDisposition", ""))
    if disposition not in DISPOSITIONS:
        raise ValueError("durableDisposition is not a supported enum value")
    compared = receipt.get("approachesCompared")
    if not isinstance(compared, list) or len(compared) < 2:
        raise ValueError("approachesCompared must contain at least two approaches")
    expected = [] if checkpoint is None else checkpoint["failureFingerprints"]
    supplied = receipt.get("failureFingerprints", expected)
    if sorted(supplied) != sorted(expected):
        raise ValueError("failureFingerprints do not match the pending checkpoint")
    active = active_entries(session_id)
    entry = {
        "schemaVersion": SCHEMA_VERSION,
        "kind": "reflection-receipt",
        "sessionHash": _session_hash(session_id),
        "checkpointDigest": _checkpoint_digest(active),
        "taskId": _safe_text("taskId", receipt.get("taskId")),
        "trigger": trigger,
        "failureFingerprints": supplied,
        "failedAssumption": _safe_text("failedAssumption", receipt.get("failedAssumption")),
        "approachesCompared": [_safe_text("approachesCompared", item) for item in compared],
        "chosenExperiment": _safe_text("chosenExperiment", receipt.get("chosenExperiment")),
        "changedApproach": _safe_text("changedApproach", receipt.get("changedApproach")),
        "proofCommandOrCheck": _safe_text("proofCommandOrCheck", receipt.get("proofCommandOrCheck")),
        "proofOutcome": _safe_text("proofOutcome", receipt.get("proofOutcome")),
        "durableDisposition": disposition,
        "observedAt": datetime.now(UTC).isoformat(),
    }
    if entry["proofOutcome"].casefold() in {"pending", "unknown", "not run"}:
        raise ValueError("proofOutcome must describe a completed proof")
    if receipt.get("issue"):
        issue = _safe_text("issue", receipt["issue"], allow_github_issue=True)
        if not _GITHUB_ISSUE_URL.fullmatch(issue):
            raise ValueError("issue must be a GitHub issue URL")
        entry["issue"] = issue
    entry["receiptHash"] = _receipt_hash(session_id, entry)
    if not append_entry(session_id, entry):
        raise OSError("could not append reflection receipt")
    return entry


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    commands = parser.add_subparsers(dest="operation", required=True)
    receipt_command = commands.add_parser("receipt", help="append a bounded JSON receipt")
    receipt_command.add_argument("--session-id", required=True)
    receipt_command.add_argument("--agent-id")
    receipt_command.add_argument("--session-token", required=True)
    receipt_input = receipt_command.add_mutually_exclusive_group(required=True)
    receipt_input.add_argument("--json", dest="receipt_json")
    receipt_input.add_argument("--file", type=Path)
    trigger_command = commands.add_parser("trigger", help="record an explicit semantic trigger")
    trigger_command.add_argument("--session-id", required=True)
    trigger_command.add_argument("--agent-id")
    trigger_command.add_argument("--trigger", required=True, choices=sorted(TRIGGERS - {"long-session-completion"}))
    trigger_command.add_argument("--fingerprint", default="manual")
    non_attempt = commands.add_parser("non-attempt", help="exclude one exact setup/probe failure")
    non_attempt.add_argument("--session-id", required=True)
    non_attempt.add_argument("--agent-id")
    non_attempt.add_argument("--failure-id", required=True)
    non_attempt.add_argument("--reason", required=True, choices=sorted(NON_ATTEMPT_REASONS))
    arguments = parser.parse_args(argv)
    session_id = scope_session_id(arguments.session_id, arguments.agent_id)
    if arguments.operation == "trigger":
        try:
            recorded = record_trigger(session_id, arguments.trigger, arguments.fingerprint)
        except ValueError as error:
            parser.error(str(error))
        print(json.dumps({"recorded": recorded, "trigger": arguments.trigger}, separators=(",", ":")))
        return 0
    if arguments.operation == "non-attempt":
        try:
            recorded = mark_non_attempt(session_id, arguments.failure_id, arguments.reason)
        except ValueError as error:
            parser.error(str(error))
        print(json.dumps({"recorded": recorded, "failureId": arguments.failure_id}, separators=(",", ":")))
        return 0
    try:
        rendered = (
            arguments.receipt_json
            if arguments.receipt_json is not None
            else arguments.file.read_text(encoding="utf-8")
        )
        payload = json.loads(rendered)
        recorded = record_receipt(session_id, payload, arguments.session_token)
    except (OSError, ValueError, TypeError, json.JSONDecodeError) as error:
        parser.error(str(error))
    print(json.dumps(recorded, separators=(",", ":"), sort_keys=True))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
