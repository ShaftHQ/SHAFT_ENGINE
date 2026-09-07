#!/usr/bin/env python3
"""Run ChaosEngine eval / parity fixtures across simulated host hook runners (#5584)."""

from __future__ import annotations

import argparse
import importlib.util
import io
import json
import os
import sys
import tempfile
import unittest.mock
from contextlib import redirect_stderr, redirect_stdout
from pathlib import Path
from typing import Any

ROOT = Path(__file__).resolve().parents[2]
FIXTURES_RELATIVE = Path("chaos-engine/evals/parity-fixtures.json")
DEFAULT_HOSTS = ("claude", "codex", "gemini", "grok", "copilot")


def _load_module(name: str, relative: str):
    path = ROOT / relative
    spec = importlib.util.spec_from_file_location(name, path)
    if spec is None or spec.loader is None:
        raise RuntimeError(f"cannot load {relative}")
    module = importlib.util.module_from_spec(spec)
    sys.modules[spec.name] = module
    spec.loader.exec_module(module)
    return module


def load_fixtures(path: Path | None = None) -> dict[str, Any]:
    target = path or (ROOT / FIXTURES_RELATIVE)
    document = json.loads(target.read_text(encoding="utf-8"))
    if not isinstance(document, dict):
        raise ValueError("fixtures document must be an object")
    return document


def validate_fixtures(document: dict[str, Any]) -> list[str]:
    defects: list[str] = []
    if document.get("schema_version") != 1:
        defects.append("schema_version must be 1")
    if document.get("package") != "chaos-engine":
        defects.append("package must be chaos-engine")
    hosts = document.get("hosts")
    if not isinstance(hosts, list) or sorted(hosts) != sorted(DEFAULT_HOSTS):
        defects.append("hosts must list the five CE hosts exactly")
    thresholds = document.get("thresholds")
    if not isinstance(thresholds, dict) or thresholds.get("case_pass_rate") != 1.0:
        defects.append("thresholds.case_pass_rate must be exactly 1.0")
    fixtures = document.get("fixtures")
    if not isinstance(fixtures, list) or not fixtures:
        defects.append("fixtures must be a nonempty list")
        return defects
    seen: set[str] = set()
    for index, fixture in enumerate(fixtures):
        if not isinstance(fixture, dict):
            defects.append(f"fixture {index} must be an object")
            continue
        identifier = fixture.get("id")
        if not isinstance(identifier, str) or not identifier.strip():
            defects.append(f"fixture {index} needs a nonempty id")
        elif identifier in seen:
            defects.append(f"duplicate fixture id {identifier}")
        else:
            seen.add(identifier)
        if not isinstance(fixture.get("event"), dict):
            defects.append(f"{identifier or index} needs an event object")
        if not isinstance(fixture.get("expect"), dict):
            defects.append(f"{identifier or index} needs an expect object")
        if fixture.get("ratchet") not in {"hooks", "skills", "matrix"}:
            defects.append(f"{identifier or index} ratchet must be hooks|skills|matrix")
    return defects


def _unwrap_context(payload: dict[str, Any]) -> str | None:
    if "additionalContext" in payload:
        return str(payload["additionalContext"])
    specific = payload.get("hookSpecificOutput")
    if isinstance(specific, dict) and "additionalContext" in specific:
        return str(specific["additionalContext"])
    return None


def _is_deny_payload(payload: dict[str, Any], host: str) -> bool:
    if host == "codex":
        specific = payload.get("hookSpecificOutput")
        return isinstance(specific, dict) and specific.get("permissionDecision") == "deny"
    if host == "copilot":
        return payload.get("permissionDecision") == "deny"
    return payload.get("decision") in {"block", "deny"}


def _deny_reason(payload: dict[str, Any], host: str) -> str:
    if host == "codex":
        specific = payload.get("hookSpecificOutput")
        if isinstance(specific, dict):
            return str(specific.get("permissionDecisionReason") or "")
    if host == "copilot":
        return str(payload.get("permissionDecisionReason") or "")
    return str(payload.get("reason") or "")


def _run_one(
    *,
    guard,
    lifecycle,
    kernel,
    fixture: dict[str, Any],
    host: str,
    temporary: str,
) -> dict[str, Any]:
    env = {
        **os.environ,
        "TMPDIR": temporary,
        "TEMP": temporary,
        "TMP": temporary,
        "CHAOS_ENGINE_HOST": host,
    }
    extra = fixture.get("env") or {}
    if isinstance(extra, dict):
        env.update({str(key): str(value) for key, value in extra.items()})

    stdout = io.StringIO()
    stderr = io.StringIO()
    with unittest.mock.patch.dict(os.environ, env, clear=False):
        with redirect_stdout(stdout), redirect_stderr(stderr):
            code = lifecycle.run_hook_protocol(
                json.dumps(fixture["event"]),
                {event: guard._run_event for event in lifecycle.LIFECYCLE_EVENTS},
                normalize=kernel.normalize_hook_input,
                host_for_input=lambda _raw, selected=host: selected,
                adapt_output=kernel.adapt_hook_output,
            )
    rendered = stderr.getvalue() if host == "claude" and code == 2 else stdout.getvalue()
    payload: dict[str, Any] = {}
    for line in reversed(rendered.strip().splitlines() if rendered.strip() else []):
        try:
            candidate = json.loads(line)
        except json.JSONDecodeError:
            continue
        if isinstance(candidate, dict):
            payload = candidate
            break
    return {
        "host": host,
        "exit_code": code,
        "payload": payload,
        "stdout": stdout.getvalue(),
        "stderr": stderr.getvalue(),
        "context": _unwrap_context(payload),
    }



def _check_host_result(
    *,
    result: dict[str, Any],
    expect: dict[str, Any],
    kernel,
    contexts: dict[str, str],
) -> list[str]:
    """Return failure strings for one host result against fixture expectations."""
    failures: list[str] = []
    host = result["host"]
    if result["exit_code"] != expect.get("exit_code"):
        failures.append(
            f"{host}: exit_code={result['exit_code']} expected={expect.get('exit_code')}"
        )
        return failures
    decision = expect.get("decision")
    if decision == "deny":
        if not _is_deny_payload(result["payload"], host):
            failures.append(f"{host}: expected native deny payload, got {result['payload']!r}")
        reason = _deny_reason(result["payload"], host)
        for fragment in expect.get("reason_substrings") or []:
            if fragment not in reason:
                failures.append(f"{host}: reason missing {fragment!r} in {reason!r}")
        capability = kernel.HOST_CAPABILITIES[host]
        if result["exit_code"] != capability.deny_exit_code:
            failures.append(
                f"{host}: deny_exit_code mismatch capability={capability.deny_exit_code}"
            )
    elif decision == "allow":
        if _is_deny_payload(result["payload"], host):
            failures.append(f"{host}: unexpected deny payload {result['payload']!r}")
    context = result["context"]
    if expect.get("context_max_bytes") is not None:
        if context is None:
            failures.append(f"{host}: missing additionalContext")
        else:
            encoded = context.encode("utf-8")
            if len(encoded) > int(expect["context_max_bytes"]):
                failures.append(
                    f"{host}: context {len(encoded)} bytes over budget "
                    f"{expect['context_max_bytes']}"
                )
            for needle in expect.get("context_must_include") or []:
                if needle not in context:
                    failures.append(f"{host}: context missing {needle!r}")
            contexts[host] = context
    return failures


def evaluate_fixture(  # noqa: MC0001  # Host-loop assertions stay auditable together.
    *,
    guard,
    lifecycle,
    kernel,
    fixture: dict[str, Any],
    hosts: list[str],
    temporary: str,
) -> dict[str, Any]:
    expect = fixture["expect"]
    results = [
        _run_one(
            guard=guard,
            lifecycle=lifecycle,
            kernel=kernel,
            fixture=fixture,
            host=host,
            temporary=temporary,
        )
        for host in hosts
    ]
    failures: list[str] = []
    contexts: dict[str, str] = {}
    for result in results:
        failures.extend(
            _check_host_result(
                result=result, expect=expect, kernel=kernel, contexts=contexts
            )
        )
    if expect.get("identical_context_across_hosts") and contexts:
        unique = set(contexts.values())
        if len(unique) != 1:
            failures.append(f"context diverged across hosts: {sorted(contexts)}")
    return {
        "id": fixture["id"],
        "policy": fixture.get("policy"),
        "ratchet": fixture.get("ratchet"),
        "passed": not failures,
        "failures": failures,
        "hosts": results,
    }


def evaluate_suite(
    document: dict[str, Any] | None = None,
    *,
    root: Path | None = None,
) -> dict[str, Any]:
    del root  # reserved for future path overrides; fixtures load from package ROOT
    document = document or load_fixtures()
    defects = validate_fixtures(document)
    if defects:
        return {
            "passed": False,
            "defects": defects,
            "results": [],
            "case_pass_rate": 0.0,  # nosec B105 - pass-rate metric, not a credential.
        }
    guard = _load_module("ce_eval_parity_guard", "chaos-engine/hooks/guard.py")
    lifecycle = _load_module("ce_eval_parity_lifecycle", "chaos-engine/hooks/lifecycle.py")
    kernel = _load_module("ce_eval_parity_kernel", "chaos-engine/hooks/kernel.py")
    hosts = [str(host) for host in document["hosts"]]
    results: list[dict[str, Any]] = []
    with tempfile.TemporaryDirectory(prefix="ce-eval-parity-") as temporary:
        for fixture in document["fixtures"]:
            results.append(
                evaluate_fixture(
                    guard=guard,
                    lifecycle=lifecycle,
                    kernel=kernel,
                    fixture=fixture,
                    hosts=hosts,
                    temporary=temporary,
                )
            )
    passed = sum(1 for row in results if row["passed"])
    total = len(results)
    return {
        "passed": passed == total and total > 0,
        "defects": [],
        "results": results,
        "case_pass_rate": (passed / total) if total else 0.0,  # nosec B105 - pass-rate metric, not a credential.
        "passed_count": passed,
        "total_count": total,
    }


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--fixtures",
        type=Path,
        default=ROOT / FIXTURES_RELATIVE,
        help="path to parity-fixtures.json",
    )
    parser.add_argument(
        "--json",
        action="store_true",
        help="print machine-readable evaluation report",
    )
    args = parser.parse_args(argv)
    document = load_fixtures(args.fixtures)
    report = evaluate_suite(document)
    if args.json:
        print(json.dumps(report, indent=2, sort_keys=True))
    else:
        if report["defects"]:
            print("fixture corpus defects:")
            for defect in report["defects"]:
                print(f"  - {defect}")
        for row in report["results"]:
            status = "PASS" if row["passed"] else "FAIL"
            print(f"[{status}] {row['id']} (ratchet={row['ratchet']})")
            for failure in row["failures"]:
                print(f"    - {failure}")
        print(
            f"case_pass_rate={report['case_pass_rate']:.3f} "
            f"({report.get('passed_count', 0)}/{report.get('total_count', 0)})"
        )
    return 0 if report["passed"] else 1


if __name__ == "__main__":
    raise SystemExit(main())
