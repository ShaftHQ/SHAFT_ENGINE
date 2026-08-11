#!/usr/bin/env python3
"""Fetch, verify, and score the pinned external guardrail corpus (#4704)."""

from __future__ import annotations

import argparse
import hashlib
import json
import re
import sys
import urllib.error
import urllib.request
from pathlib import Path
from typing import Callable

ROOT = Path(__file__).resolve().parents[2]
DEFAULT_CONTRACT = ROOT / "scripts/ci/external_guardrail_corpus.json"
if str(ROOT) not in sys.path:
    sys.path.insert(0, str(ROOT))

# Direct script execution needs the repository root before this local import.
from scripts.agents.guard import evaluate_command  # noqa: E402


class ContractError(ValueError):
    """The pinned source or its local evaluation contract is inconsistent."""


def _validate_contract(contract: dict) -> None:
    if (
        not isinstance(contract, dict)
        or type(contract.get("schema_version")) is not int  # pylint: disable=unidiomatic-typecheck
        or contract["schema_version"] != 1
    ):
        raise ContractError("contract schema_version must be integer 1")
    source = contract.get("source")
    if not isinstance(source, dict):
        raise ContractError("contract source must be an object")
    for field in ("repository", "path", "url"):
        if not isinstance(source.get(field), str) or not source[field].strip():
            raise ContractError(f"source {field} is required")
    if not source["repository"].startswith("https://") or not source["url"].startswith("https://"):
        raise ContractError("source repository and URL must use HTTPS")
    revision = source.get("revision")
    digest = source.get("sha256")
    expected_cases = source.get("expected_cases")
    if not isinstance(revision, str) or not re.fullmatch(r"[0-9a-f]{40}", revision):
        raise ContractError("source revision must be a full lowercase commit SHA")
    if revision not in source["url"]:
        raise ContractError("source URL must contain the immutable revision")
    if not isinstance(digest, str) or not re.fullmatch(r"[0-9a-f]{64}", digest):
        raise ContractError("source sha256 must contain 64 lowercase hex characters")
    if type(expected_cases) is not int or expected_cases <= 0:  # pylint: disable=unidiomatic-typecheck
        raise ContractError("source expected_cases must be a positive integer")
    floors = contract.get("floors")
    if not isinstance(floors, dict) or set(floors) != {"precision", "recall"}:
        raise ContractError("floors must contain precision and recall")
    if any(type(floors[key]) not in {float, int} or not 0 <= floors[key] <= 1 for key in floors):
        raise ContractError("score floors must be numbers from zero through one")
    exclusions = contract.get("exclusion_rules")
    if not isinstance(exclusions, list):
        raise ContractError("exclusion_rules must be a list")
    identifiers: list[str] = []
    for rule in exclusions:
        if not isinstance(rule, dict) or set(rule) != {"id", "command_prefix", "reason"}:
            raise ContractError("each exclusion must contain id, command_prefix, and reason")
        if not all(isinstance(rule[key], str) and rule[key].strip() for key in rule):
            raise ContractError("exclusion values must be non-empty strings")
        identifiers.append(rule["id"])
    if len(identifiers) != len(set(identifiers)):
        raise ContractError("exclusion ids must be unique")


def _parse_cases(payload: bytes) -> list[dict]:
    try:
        text = payload.decode("utf-8")
    except UnicodeDecodeError as error:
        raise ContractError("corpus must be UTF-8 text") from error
    cases: list[dict] = []
    for line_number, raw_line in enumerate(text.splitlines(), start=1):
        if not raw_line or raw_line.startswith("#"):
            continue
        fields = raw_line.split("\t")
        if len(fields) not in {2, 3} or fields[0] not in {"block", "allow"} or not fields[1]:
            raise ContractError(f"invalid corpus row at line {line_number}")
        cases.append(
            {
                "line": line_number,
                "expected": fields[0],
                "command": fields[1],
                "current_branch": fields[2] if len(fields) == 3 and fields[2] else None,
            }
        )
    identities = [(case["command"], case["current_branch"]) for case in cases]
    if len(identities) != len(set(identities)):
        raise ContractError("corpus commands and branch contexts must be unique")
    return cases


def evaluate_payload(payload: bytes, contract: dict, classifier: Callable[[str], str | None]) -> dict:
    """Verify and score bytes; unverified or ambiguously excluded rows never run."""
    _validate_contract(contract)
    source = contract["source"]
    observed_digest = hashlib.sha256(payload).hexdigest()
    if observed_digest != source["sha256"]:
        raise ContractError(f"corpus SHA-256 mismatch: expected {source['sha256']}, got {observed_digest}")
    cases = _parse_cases(payload)
    if len(cases) != source["expected_cases"]:
        raise ContractError(f"corpus case count mismatch: expected {source['expected_cases']}, got {len(cases)}")

    applicable: list[dict] = []
    excluded: list[dict] = []
    for case in cases:
        matches = [
            rule
            for rule in contract["exclusion_rules"]
            if case["command"].startswith(rule["command_prefix"])
        ]
        if len(matches) > 1:
            raise ContractError(f"corpus line {case['line']} matches multiple exclusion rules")
        if matches:
            rule = matches[0]
            excluded.append(
                {
                    **case,
                    "exclusion_id": rule["id"],
                    "exclusion_reason": rule["reason"],
                }
            )
            continue
        reason = classifier(case["command"])
        observed = "block" if reason else "allow"
        applicable.append({**case, "observed": observed, "reason": reason})

    true_positive = sum(row["expected"] == "block" and row["observed"] == "block" for row in applicable)
    false_positive = sum(row["expected"] == "allow" and row["observed"] == "block" for row in applicable)
    false_negative = sum(row["expected"] == "block" and row["observed"] == "allow" for row in applicable)
    true_negative = sum(row["expected"] == "allow" and row["observed"] == "allow" for row in applicable)
    precision = true_positive / (true_positive + false_positive) if true_positive + false_positive else 0.0
    recall = true_positive / (true_positive + false_negative) if true_positive + false_negative else 0.0
    floors = contract["floors"]
    verdict = "pass" if precision >= floors["precision"] and recall >= floors["recall"] else "fail"
    return {
        "schema_version": 1,
        "verdict": verdict,
        "source": {**source, "observed_sha256": observed_digest},
        "counts": {"corpus": len(cases), "applicable": len(applicable), "excluded": len(excluded)},
        "confusion_matrix": {
            "true_positive": true_positive,
            "false_positive": false_positive,
            "false_negative": false_negative,
            "true_negative": true_negative,
        },
        "metrics": {"precision": precision, "recall": recall},
        "floors": floors,
        "applicable_cases": applicable,
        "excluded_cases": excluded,
    }


def _fetch(url: str) -> bytes:
    request = urllib.request.Request(url, headers={"User-Agent": "SHAFT-external-guardrail-eval"})
    # The contract requires HTTPS and the payload is digest-pinned.
    with urllib.request.urlopen(request, timeout=30) as response:  # nosec B310
        return response.read()


def collect_report(
    contract: dict,
    classifier: Callable[[str], str | None] = evaluate_command,
    fetcher: Callable[[str], bytes] = _fetch,
) -> dict:
    """Return structured evidence for pass, contract failure, or source outage."""
    try:
        _validate_contract(contract)
        payload = fetcher(contract["source"]["url"])
        return evaluate_payload(payload, contract, classifier)
    except (OSError, urllib.error.URLError) as error:
        return {"schema_version": 1, "verdict": "external_blocker", "detail": str(error)}
    except ContractError as error:
        return {"schema_version": 1, "verdict": "fail", "detail": str(error)}


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--contract", type=Path, default=DEFAULT_CONTRACT)
    parser.add_argument("--corpus", type=Path, help="Use already-fetched bytes for an offline replay")
    parser.add_argument("--output", type=Path)
    options = parser.parse_args(argv)
    try:
        contract = json.loads(options.contract.read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError) as error:
        report = {"schema_version": 1, "verdict": "fail", "detail": str(error)}
    else:
        fetcher = (lambda _url: options.corpus.read_bytes()) if options.corpus else _fetch
        report = collect_report(contract, evaluate_command, fetcher)
    rendered = json.dumps(report, indent=2, sort_keys=True) + "\n"
    if options.output:
        options.output.write_text(rendered, encoding="utf-8")
    print(rendered, end="")
    return {"pass": 0, "fail": 1, "external_blocker": 2}.get(report.get("verdict"), 1)


if __name__ == "__main__":
    raise SystemExit(main())
