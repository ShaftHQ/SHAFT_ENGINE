#!/usr/bin/env python3
"""OmniRoute proof-of-dispatch checks when the adopter required OmniRoute."""

from __future__ import annotations

from typing import Any


_PROBE_CALL_LOG_MODELS = frozenset({
    "connection-test",
    "credentialhealth",
    "credential-health",
})
_PROBE_CLAIMED_STEPS = frozenset({
    "probe",
    "health",
    "candidates",
    "catalog",
    "models",
    "quota",
    "credentialhealth",
    "credential-health",
    "token-health",
    "api/health",
})
_BLOCKER = (
    "OmniRoute was required but no omniroute run receipt or coding "
    "completion call_log was found. Catalog/candidates/health probes "
    "alone are not OmniRoute progress."
)


def call_log_is_probe(entry: object) -> bool:
    """True for CredentialHealth / connection-test / catalog-style probe rows."""
    if not isinstance(entry, dict):
        return True
    model = str(entry.get("model") or "").strip().casefold()
    if not model or model in _PROBE_CALL_LOG_MODELS:
        return True
    compact = model.replace(" ", "").replace("_", "").replace("-", "")
    if "credentialhealth" in compact:
        return True
    for key in ("source", "kind", "type", "caller", "client", "origin"):
        value = str(entry.get(key) or "").casefold().replace(" ", "")
        if not value:
            continue
        if "credentialhealth" in value or value in {"health", "probe", "catalog"}:
            return True
    return False


def call_log_is_coding_completion(entry: object) -> bool:
    """True for a non-probe completion row suitable as OmniRoute coding proof."""
    if not isinstance(entry, dict) or call_log_is_probe(entry):
        return False
    model = str(entry.get("model") or "").strip()
    if not model:
        return False
    method = str(entry.get("method") or "POST").strip().upper()
    if method and method != "POST":
        return False
    status = entry.get("status")
    if status is not None:
        try:
            if int(status) >= 400:
                return False
        except (TypeError, ValueError):
            return False
    tokens = entry.get("tokens")
    if isinstance(tokens, (int, float)) and tokens > 0:
        return True
    # After probe filtering, a successful POST to a real model id counts.
    return True


def run_receipt_proves_dispatch(receipt: object) -> bool:
    """True when a ChaosEngine OmniRoute runner receipt proves omniroute run."""
    if not isinstance(receipt, dict):
        return False
    if receipt.get("omnirouteRun") is True or receipt.get("dispatchProved") is True:
        return True
    outcome = str(receipt.get("outcome") or "").casefold()
    status = str(receipt.get("status") or "").casefold()
    exit_code = receipt.get("exitCode")
    if outcome == "success" and status in {"completed", "review", "success"}:
        return True
    if exit_code == 0 and status == "completed":
        return True
    return False


def evaluate_required_dispatch_proof(
    *,
    omniroute_required: bool,
    run_receipt: dict[str, Any] | None = None,
    call_logs: list[Any] | None = None,
    claimed_steps: list[str] | None = None,
) -> dict[str, Any]:
    """Fail closed when OmniRoute was required but only probes ran."""
    if not omniroute_required:
        return {
            "state": "NOT_REQUIRED",
            "proved": True,
            "blocker": None,
            "proofKind": None,
            "probeOnlyEvidence": False,
        }
    if run_receipt_proves_dispatch(run_receipt):
        return {
            "state": "PROVED",
            "proved": True,
            "blocker": None,
            "proofKind": "run_receipt",
            "probeOnlyEvidence": False,
        }
    coding_logs = [
        entry for entry in (call_logs or [])
        if call_log_is_coding_completion(entry)
    ]
    if coding_logs:
        return {
            "state": "PROVED",
            "proved": True,
            "blocker": None,
            "proofKind": "coding_call_log",
            "probeOnlyEvidence": False,
        }
    normalized_steps = [
        str(step).strip().casefold().replace("_", "-")
        for step in (claimed_steps or [])
        if str(step).strip()
    ]
    probe_only = (
        not normalized_steps
        or all(step in _PROBE_CLAIMED_STEPS for step in normalized_steps)
    )
    return {
        "state": "BLOCKED",
        "proved": False,
        "blocker": _BLOCKER,
        "proofKind": None,
        "probeOnlyEvidence": probe_only,
    }
