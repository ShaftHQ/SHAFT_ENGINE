#!/usr/bin/env python3
"""Collect four-level native client compatibility evidence for shaft-skills (#4641)."""

from __future__ import annotations

import argparse
import json
import os
import re
import shutil
import subprocess  # nosec B404 - runs pinned native client commands without a shell.
import tempfile
import time
from pathlib import Path
from typing import Callable, Mapping

try:
    from scripts.ci.assemble_shaft_skills_plugin import assemble
    from scripts.ci.shaft_skill_routing_eval import (
        evaluate_results,
        output_schema,
        package_decision,
    )
except ModuleNotFoundError:  # Direct script execution places scripts/ci on sys.path.
    from assemble_shaft_skills_plugin import assemble
    from shaft_skill_routing_eval import evaluate_results, output_schema, package_decision

ROOT = Path(__file__).resolve().parents[2]
PACKAGE_NAME = "shaft-skills"
MARKETPLACE_NAME = "shaft-skills"
LOAD_PROOF_TERMS = (
    "shaft-requirements-analysis",
    "load no adjacent skill preemptively",
)
LOAD_PROMPT = (
    "Use the installed SHAFT router for a request whose immediate output is testable "
    "requirements and acceptance criteria. Respond in one short line with the chosen "
    "specialist's canonical name and the routing-map sentence governing proactive loading "
    "of sibling skills."
)
ROUTING_RESPONSE_INSTRUCTION = (
    " Select exactly one installed SHAFT specialist for the immediate requested output. "
    "Return only the structured chosen_skill value using its canonical name."
)
EXECUTION_BUDGET_SECONDS = 900
ROUTING_BUDGET_SECONDS = 600
CLEANUP_RESERVE_SECONDS = 120
ARTIFACT_RESERVE_SECONDS = 60
EVIDENCE_LEVELS = (
    "package_validation",
    "marketplace_discovery",
    "install_enable",
    "real_load",
)
CLIENTS = {
    "claude": {
        "display_name": "Claude Code",
        "version": "2.1.223",
        "npm_package": "@anthropic-ai/claude-code@2.1.223",
        "credential": "ANTHROPIC_API_KEY",
    },
    "codex": {
        "display_name": "Codex CLI",
        "version": "0.146.0",
        "npm_package": "@openai/codex@0.146.0",
        "credential": "OPENAI_API_KEY",
    },
}
Runner = Callable[..., subprocess.CompletedProcess]
WARNING = re.compile(
    r"(?i)^.*(?:warn(?:ing)?.*(?:context|token|description|skill)|"
    r"(?:context.*budget|budget.*context)|"
    r"(?:shorten|truncat).*(?:description|skill)|"
    r"(?:description|skill).*(?:shorten|truncat)).*$"
)
AUTHENTICATION_BLOCKER = re.compile(
    r"(?i)(?:\b(?:401|403)\b|\bunauthori[sz]ed\b|\bforbidden\b|"
    r"(?:invalid|missing|required|denied|disabled|expired|unavailable)\s+"
    r"(?:auth(?:entication|orization)?|credential|api[ _-]?key|subscription|entitlement|token)|"
    r"(?:auth(?:entication|orization)?|credential|api[ _-]?key|subscription|entitlement|"
    r"not logged in|login required|organization access).*(?:fail|invalid|missing|required|"
    r"denied|disabled|expired|unavailable)|(?:disabled|denied).*(?:organization|access))"
)
PROVIDER_OR_TRANSPORT_BLOCKER = re.compile(
    r"(?i)(?:\b429\b|too many requests|rate[ _-]?limit|quota|overload|capacity|"
    r"temporar(?:y|ily) unavailable|service unavailable|\b5(?:00|02|03|04)\b|"
    r"timed?\s*out|timeout|connection (?:reset|refused|aborted)|"
    r"network (?:error|unavailable)|\bdns\b|getaddrinfo|\benotfound\b|\beai_again\b|"
    r"name or service not known|temporary failure in name resolution)"
)


def _run(
    runner: Runner,
    command: list[str],
    cwd: Path,
    environment: Mapping[str, str],
    prompt: str | None = None,
    timeout: float = 180,
    deadline: float | None = None,
    clock: Callable[[], float] = time.monotonic,
) -> subprocess.CompletedProcess:
    if deadline is not None:
        remaining = deadline - clock()
        if remaining <= 0:
            return subprocess.CompletedProcess(
                command,
                124,
                stdout="",
                stderr="aggregate native-client deadline exhausted",
            )
        timeout = min(timeout, remaining)
    execution_command = command
    if runner is subprocess.run:
        resolved_executable = shutil.which(command[0], path=environment.get("PATH"))
        if resolved_executable:
            execution_command = [resolved_executable, *command[1:]]
    try:
        return runner(
            execution_command,
            cwd=cwd,
            env=dict(environment),
            input=prompt,
            text=True,
            capture_output=True,
            check=False,
            timeout=timeout,
        )
    except (OSError, subprocess.TimeoutExpired) as error:
        return subprocess.CompletedProcess(command, 127, stdout="", stderr=str(error))


def _warnings(*completed: subprocess.CompletedProcess) -> list[str]:
    warnings: list[str] = []
    for result in completed:
        for line in f"{result.stdout or ''}\n{result.stderr or ''}".splitlines():
            line = line.strip()
            if line and WARNING.match(line) and line not in warnings:
                warnings.append(line)
    return warnings


def _contains_package(output: str, require_enabled: bool = False) -> bool:
    try:
        value = json.loads(output)
    except (TypeError, json.JSONDecodeError):
        return False

    def contains(candidate: object) -> bool:
        if isinstance(candidate, dict):
            identities = [
                value
                for key, value in candidate.items()
                if key in {"name", "plugin", "pluginId", "id"} and isinstance(value, str)
            ]
            if identities:
                matched = any(
                    identity == PACKAGE_NAME
                    or identity.startswith(f"{PACKAGE_NAME}@")
                    for identity in identities
                )
                return matched and (
                    not require_enabled or candidate.get("enabled") is True
                )
            return any(contains(value) for value in candidate.values())
        if isinstance(candidate, list):
            return any(contains(item) for item in candidate)
        return False

    return contains(value)


def _valid_json(output: str) -> bool:
    try:
        json.loads(output)
    except (TypeError, json.JSONDecodeError):
        return False
    return True


def _version_matches(output: str, expected: str) -> bool:
    versions = re.findall(r"(?<![0-9.])\d+\.\d+\.\d+(?![0-9A-Za-z.+-])", output)
    return bool(versions) and all(version == expected for version in versions)


def _redact(value: object, secrets: tuple[str, ...]) -> object:
    if isinstance(value, str):
        for secret in secrets:
            value = value.replace(secret, "[REDACTED]")
        return value
    if isinstance(value, list):
        return [_redact(item, secrets) for item in value]
    if isinstance(value, dict):
        return {key: _redact(item, secrets) for key, item in value.items()}
    return value


def _extract_chosen_skill(output: str) -> str | None:  # noqa: MC0001  # Nested client JSON has several valid shapes.
    def visit(value: object) -> str | None:
        if isinstance(value, dict):
            chosen = value.get("chosen_skill")
            if isinstance(chosen, str):
                return chosen
            for nested in value.values():
                found = visit(nested)
                if found:
                    return found
        elif isinstance(value, list):
            for nested in value:
                found = visit(nested)
                if found:
                    return found
        elif isinstance(value, str) and value.strip().startswith(("{", "[")):
            try:
                return visit(json.loads(value))
            except json.JSONDecodeError:
                return None
        return None

    for line in output.splitlines():
        try:
            found = visit(json.loads(line))
        except json.JSONDecodeError:
            continue
        if found:
            return found
    try:
        return visit(json.loads(output))
    except json.JSONDecodeError:
        return None


def _routing_command(
    client: str,
    prompt: str,
    schema_path: Path,
) -> tuple[list[str], str | None]:
    routed_prompt = prompt + ROUTING_RESPONSE_INSTRUCTION
    if client == "claude":
        schema = json.dumps(output_schema(), separators=(",", ":"))
        return [
            "claude",
            "-p",
            routed_prompt,
            "--output-format",
            "json",
            "--json-schema",
            schema,
        ], None
    return [
        "codex",
        "exec",
        "--json",
        "-s",
        "read-only",
        "--skip-git-repo-check",
        "--output-schema",
        str(schema_path),
        "-",
    ], routed_prompt


def _row(
    client: str,
    actual_version: str,
    level: str,
    verdict: str,
    commands: list[list[str]],
    detail: str,
    completed: list[subprocess.CompletedProcess] | None = None,
) -> dict:
    return {
        "client": client,
        "client_name": CLIENTS[client]["display_name"],
        "client_version": actual_version,
        "expected_client_version": CLIENTS[client]["version"],
        "evidence_level": level,
        "verdict": verdict,
        "commands": commands,
        "detail": detail,
        "context_warnings": _warnings(*(completed or [])),
    }


def _claude_commands(package_root: Path) -> dict[str, list[list[str]]]:
    selector = f"{PACKAGE_NAME}@{MARKETPLACE_NAME}"
    return {
        "package_validation": [["claude", "plugin", "validate", "--strict", str(package_root)]],
        "marketplace_discovery": [
            ["claude", "plugin", "marketplace", "add", str(package_root), "--scope", "project"],
            ["claude", "plugin", "list", "--available", "--json"],
        ],
        "install_enable": [
            ["claude", "plugin", "install", selector, "--scope", "project"],
            ["claude", "plugin", "list", "--json"],
        ],
        "real_load": [["claude", "-p", LOAD_PROMPT, "--output-format", "json"]],
        "cleanup": [
            ["claude", "plugin", "uninstall", selector, "--scope", "project", "--yes"],
            ["claude", "plugin", "marketplace", "remove", MARKETPLACE_NAME, "--scope", "project"],
        ],
    }


def _codex_commands(package_root: Path) -> dict[str, list[list[str]]]:
    selector = f"{PACKAGE_NAME}@{MARKETPLACE_NAME}"
    return {
        "package_validation": [["codex", "plugin", "marketplace", "add", str(package_root), "--json"]],
        "marketplace_discovery": [["codex", "plugin", "list", "--available", "--json"]],
        "install_enable": [
            ["codex", "plugin", "add", selector, "--json"],
            ["codex", "plugin", "list", "--json"],
        ],
        "real_load": [["codex", "exec", "--json", "-s", "read-only", "--skip-git-repo-check", "-"]],
        "cleanup": [
            ["codex", "plugin", "remove", selector, "--json"],
            ["codex", "plugin", "marketplace", "remove", MARKETPLACE_NAME, "--json"],
        ],
    }


def _client_evidence(  # noqa: MC0001  # One lifecycle owns setup, evidence, and guaranteed cleanup.
    client: str,
    package_root: Path,
    working_directory: Path,
    mode: str,
    runner: Runner,
    environment: Mapping[str, str],
    credentials: Mapping[str, str],
    routing_corpus: dict | None,
    routing_deadline: float | None,
    active_deadline: float,
    cleanup_deadline: float,
    clock: Callable[[], float],
) -> list[dict]:
    def run_active(
        command: list[str],
        environment_override: Mapping[str, str] = environment,
        prompt: str | None = None,
        timeout: float = 180,
    ) -> subprocess.CompletedProcess:
        return _run(
            runner,
            command,
            working_directory,
            environment_override,
            prompt,
            timeout=timeout,
            deadline=active_deadline,
            clock=clock,
        )

    def run_cleanup(command: list[str]) -> subprocess.CompletedProcess:
        return _run(
            runner,
            command,
            working_directory,
            environment,
            deadline=cleanup_deadline,
            clock=clock,
        )

    commands = _claude_commands(package_root) if client == "claude" else _codex_commands(package_root)
    version_command = [client, "--version"]
    version_result = run_active(version_command)
    actual_version = f"{version_result.stdout or ''} {version_result.stderr or ''}".strip()
    version_matches = version_result.returncode == 0 and _version_matches(
        actual_version, CLIENTS[client]["version"]
    )
    rows: list[dict] = []
    preflight_commands = [
        [client, "plugin", "marketplace", "list", "--json"],
        [client, "plugin", "list", "--json"],
    ]
    preflight = []
    for command in preflight_commands:
        result = run_active(command)
        preflight.append(result)
        if result.returncode:
            break
    preflight_passed = len(preflight) == len(preflight_commands) and all(
        result.returncode == 0 and _valid_json(result.stdout or "") for result in preflight
    )
    collision = preflight_passed and any(
        result.returncode == 0 and _contains_package(result.stdout or "") for result in preflight
    )
    if not preflight_passed or collision:
        preflight_failure = next((result for result in preflight if result.returncode), None)
        detail = (
            "pre-existing shaft-skills client state was found; refusing to mutate user-owned state"
            if collision
            else "client-state preflight failed; refusing to mutate unverified client state: "
            + ((preflight_failure.stderr or preflight_failure.stdout).strip() if preflight_failure else "invalid JSON")
        )
        rows.append(
            _row(
                client,
                actual_version,
                "package_validation",
                "fail",
                [version_command] + preflight_commands,
                detail,
                [version_result] + preflight,
            )
        )
        for level in EVIDENCE_LEVELS[1:]:
            rows.append(
                _row(
                    client,
                    actual_version,
                    level,
                    "not_run",
                    commands[level],
                    "not run because client state could not be safely claimed",
                )
            )
        return rows
    install_attempted = False
    marketplace_attempted = False
    prerequisites_pass = version_matches
    try:
        for level in EVIDENCE_LEVELS[:-1]:
            level_commands = commands[level]
            completed: list[subprocess.CompletedProcess] = []
            if prerequisites_pass:
                for command in level_commands:
                    if command[1:4] == ["plugin", "marketplace", "add"]:
                        marketplace_attempted = True
                    if command[1:3] in (["plugin", "install"], ["plugin", "add"]):
                        install_attempted = True
                    result = run_active(command)
                    completed.append(result)
                    if result.returncode:
                        break
            successful = prerequisites_pass and len(completed) == len(level_commands) and all(
                result.returncode == 0 for result in completed
            )
            if successful and level == "marketplace_discovery":
                successful = _contains_package(completed[-1].stdout or "")
            if successful and level == "install_enable":
                successful = _contains_package(completed[-1].stdout or "", require_enabled=True)
            if not version_matches and level == "package_validation":
                detail = f"expected pinned client version {CLIENTS[client]['version']}; got {actual_version or 'no output'}"
            elif successful:
                detail = f"{level.replace('_', ' ')} passed without model credentials"
            elif not prerequisites_pass:
                detail = "not run because an earlier unauthenticated evidence level failed"
            else:
                failure = next((result for result in completed if result.returncode), completed[-1] if completed else None)
                detail = (failure.stderr or failure.stdout or "client output did not contain shaft-skills").strip()
            rows.append(
                _row(
                    client,
                    actual_version,
                    level,
                    "pass" if successful else ("fail" if level == "package_validation" or completed else "not_run"),
                    [version_command] + level_commands if level == "package_validation" else level_commands,
                    detail,
                    [version_result] + completed if level == "package_validation" else completed,
                )
            )
            prerequisites_pass = prerequisites_pass and successful

        load_commands = commands["real_load"]
        credential = CLIENTS[client]["credential"]
        if mode == "smoke":
            rows.append(
                _row(client, actual_version, "real_load", "not_run", load_commands, "live load is outside unauthenticated PR smoke")
            )
        elif not prerequisites_pass:
            rows.append(
                _row(client, actual_version, "real_load", "not_run", load_commands, "live load requires passing install evidence")
            )
        elif not credentials.get(credential):
            load_row = _row(
                    client,
                    actual_version,
                    "real_load",
                    "external_blocker",
                    load_commands,
                    f"required model credential {credential} is missing or disabled",
                )
            if routing_corpus:
                load_row["case_results"] = evaluate_results(
                    routing_corpus,
                    CLIENTS[client]["display_name"],
                    actual_version,
                    [],
                    external_blocker=f"required model credential {credential} is missing or disabled",
                )["results"]
            rows.append(load_row)
        elif routing_corpus:
            live_environment = dict(environment)
            live_environment[credential] = credentials[credential]
            schema_path = working_directory / "routing-output-schema.json"
            schema_path.write_text(json.dumps(output_schema()), encoding="utf-8")
            completed: list[subprocess.CompletedProcess] = []
            executed_commands: list[list[str]] = []
            records: list[dict] = []
            warnings_by_case: dict[str, list[str]] = {}
            external_blocker_detail: str | None = None
            client_failure_detail: str | None = None
            for case in routing_corpus["cases"]:
                remaining = (
                    routing_deadline - clock()
                    if routing_deadline is not None
                    else 180
                )
                if remaining <= 0:
                    external_blocker_detail = (
                        "aggregate routing deadline was exhausted before all cases ran"
                    )
                    break
                command, prompt = _routing_command(client, case["prompt"], schema_path)
                executed_commands.append(command)
                result = run_active(
                    command,
                    live_environment,
                    prompt,
                    timeout=min(180, remaining),
                )
                completed.append(result)
                warnings_by_case[case["id"]] = _warnings(result)
                output = f"{result.stdout or ''}\n{result.stderr or ''}"
                if result.returncode and AUTHENTICATION_BLOCKER.search(output):
                    external_blocker_detail = (
                        "client authentication or entitlement is disabled or unavailable"
                    )
                elif result.returncode and PROVIDER_OR_TRANSPORT_BLOCKER.search(output):
                    external_blocker_detail = (
                        "model provider or transport is temporarily unavailable"
                    )
                elif result.returncode:
                    client_failure_detail = (
                        "native client exited without a routing response"
                    )
                if external_blocker_detail or client_failure_detail:
                    break
                records.append(
                    {
                        "case_id": case["id"],
                        "chosen_skill": _extract_chosen_skill(output),
                    }
                )
            report = evaluate_results(
                routing_corpus,
                CLIENTS[client]["display_name"],
                actual_version,
                records,
                external_blocker=external_blocker_detail,
                client_failure=client_failure_detail,
            )
            for case_result in report["results"]:
                case_result["context_budget_warnings"] = warnings_by_case.get(
                    case_result["case_id"], []
                )
            successful = (
                report["summary"]["failures"] == 0
                and report["summary"]["passes"] == len(routing_corpus["cases"])
            )
            load_row = _row(
                client,
                actual_version,
                "real_load",
                (
                    "external_blocker"
                    if external_blocker_detail
                    else (
                        "client_failure"
                        if client_failure_detail
                        else ("pass" if successful else "fail")
                    )
                ),
                executed_commands,
                (
                    external_blocker_detail
                    or client_failure_detail
                    or f"{report['summary']['passes']}/{len(routing_corpus['cases'])} routing cases passed"
                ),
                completed,
            )
            load_row["case_results"] = report["results"]
            rows.append(load_row)
        else:
            live_environment = dict(environment)
            live_environment[credential] = credentials[credential]
            completed = []
            for command in load_commands:
                result = run_active(
                    command,
                    live_environment,
                    LOAD_PROMPT if client == "codex" else None,
                )
                completed.append(result)
                if result.returncode:
                    break
            output = "\n".join(f"{result.stdout or ''}\n{result.stderr or ''}" for result in completed)
            normalized_output = output.lower()
            successful = all(result.returncode == 0 for result in completed) and all(
                proof in normalized_output for proof in LOAD_PROOF_TERMS
            )
            authentication_blocked = any(
                result.returncode != 0
                and AUTHENTICATION_BLOCKER.search(
                    f"{result.stdout or ''}\n{result.stderr or ''}"
                )
                for result in completed
            )
            rows.append(
                _row(
                    client,
                    actual_version,
                    "real_load",
                    "external_blocker"
                    if authentication_blocked
                    else ("pass" if successful else "fail"),
                    load_commands,
                    "client authentication or entitlement is disabled or unavailable"
                    if authentication_blocked
                    else (
                        "installed skill routed and returned the expected load proof"
                        if successful
                        else "expected load proof was absent"
                    ),
                    completed,
                )
            )
    finally:
        cleanup = commands["cleanup"]
        cleanup_completed: list[subprocess.CompletedProcess] = []
        cleanup_commands: list[list[str]] = []
        if install_attempted:
            cleanup_commands.append(cleanup[0])
            cleanup_completed.append(run_cleanup(cleanup[0]))
        if marketplace_attempted:
            cleanup_commands.append(cleanup[1])
            cleanup_completed.append(run_cleanup(cleanup[1]))
        verification_commands: list[list[str]] = []
        verification_completed: list[subprocess.CompletedProcess] = []
        if install_attempted or marketplace_attempted:
            verification_commands = preflight_commands
            for command in verification_commands:
                result = run_cleanup(command)
                verification_completed.append(result)
                if result.returncode:
                    break
        cleanup_verified = all(
            result.returncode == 0
            and _valid_json(result.stdout or "")
            and not _contains_package(result.stdout or "")
            for result in verification_completed
        )
        if cleanup_completed or verification_completed:
            install_row = next(row for row in rows if row["evidence_level"] == "install_enable")
            install_row["commands"].extend(cleanup_commands + verification_commands)
            install_row["context_warnings"].extend(
                _warnings(*cleanup_completed, *verification_completed)
            )
        if any(result.returncode for result in cleanup_completed) or not cleanup_verified:
            install_row["verdict"] = "fail"
            install_row["detail"] = (
                "client state cleanup failed or could not be verified after install/enable evidence"
            )
    return rows


def collect_evidence(
    package_root: Path,
    mode: str = "smoke",
    runner: Runner = subprocess.run,
    environ: Mapping[str, str] | None = None,
    routing_corpus: dict | None = None,
    clock: Callable[[], float] = time.monotonic,
    execution_budget_seconds: float = EXECUTION_BUDGET_SECONDS,
    routing_budget_seconds: float = ROUTING_BUDGET_SECONDS,
    cleanup_reserve_seconds: float = CLEANUP_RESERVE_SECONDS,
    artifact_reserve_seconds: float = ARTIFACT_RESERVE_SECONDS,
) -> dict:
    """Collect independent package, discovery, install, and load evidence."""
    if mode not in {"smoke", "live"}:
        raise ValueError("mode must be smoke or live")
    if routing_budget_seconds <= 0:
        raise ValueError("routing_budget_seconds must be positive")
    if execution_budget_seconds <= 0:
        raise ValueError("execution_budget_seconds must be positive")
    if cleanup_reserve_seconds <= 0 or artifact_reserve_seconds <= 0:
        raise ValueError("cleanup and artifact reserves must be positive")
    if cleanup_reserve_seconds + artifact_reserve_seconds >= execution_budget_seconds:
        raise ValueError("cleanup and artifact reserves must leave time for active checks")
    package_root = Path(package_root).resolve()
    working_directory = package_root.parent / "client-state"
    working_directory.mkdir(exist_ok=True)
    credentials = os.environ if environ is None else environ
    environment = dict(os.environ)
    if environ is not None:
        environment.update(environ)
    for config in CLIENTS.values():
        environment.pop(config["credential"], None)
    results: list[dict] = []
    started = clock()
    hard_deadline = started + execution_budget_seconds
    cleanup_deadline = hard_deadline - artifact_reserve_seconds
    active_deadline = cleanup_deadline - cleanup_reserve_seconds
    routing_deadline = (
        min(started + routing_budget_seconds, active_deadline)
        if mode == "live" and routing_corpus
        else None
    )
    for client in CLIENTS:
        results.extend(
            _client_evidence(
                client,
                package_root,
                working_directory,
                mode,
                runner,
                environment,
                credentials,
                routing_corpus,
                routing_deadline,
                active_deadline,
                cleanup_deadline,
                clock,
            )
        )
    evidence = {
        "schema_version": 1,
        "package": PACKAGE_NAME,
        "mode": mode,
        "results": results,
    }
    if routing_corpus:
        routing_reports = []
        expected_skills = {
            case["expected_skill"] for case in routing_corpus["cases"]
        }
        for row in results:
            if row["evidence_level"] != "real_load":
                continue
            case_results = row.get("case_results", [])
            passed = sum(case["verdict"] == "pass" for case in case_results)
            failed = sum(case["verdict"] == "fail" for case in case_results)
            blocked = sum(
                case["verdict"] == "external_blocker" for case in case_results
            )
            client_failures = sum(
                case["verdict"] == "client_failure" for case in case_results
            )
            passed_skills = {
                case["expected_skill"]
                for case in case_results
                if case["verdict"] == "pass"
            }
            client_warnings = sorted(
                {
                    warning
                    for client_row in results
                    if client_row["client"] == row["client"]
                    for warning in client_row.get("context_warnings", [])
                }
            )
            routing_reports.append(
                {
                    "client": row["client_name"],
                    "thresholds": routing_corpus["thresholds"],
                    "summary": {
                        "passes": passed,
                        "failures": failed,
                        "external_blockers": blocked,
                        "client_failures": client_failures,
                        "case_pass_rate": (
                            passed / (passed + failed) if passed + failed else None
                        ),
                        "positive_skill_coverage": (
                            len(passed_skills) / len(expected_skills)
                            if expected_skills
                            else None
                        ),
                    },
                    "results": case_results,
                    "context_budget_warnings": client_warnings,
                }
            )
        evidence["package_decision"] = package_decision(routing_reports)
    secrets = tuple(
        secret
        for config in CLIENTS.values()
        if isinstance((secret := credentials.get(config["credential"])), str)
        and secret
    )
    redacted = _redact(evidence, secrets)
    if not isinstance(redacted, dict):
        raise TypeError("redacted evidence must be an object")
    return redacted


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--mode", choices=("smoke", "live"), default="smoke")
    parser.add_argument("--output", type=Path, default=Path("agent-plugin-client-evidence.json"))
    parser.add_argument("--routing-corpus", type=Path)
    parser.add_argument(
        "--execution-budget-seconds",
        type=float,
        default=EXECUTION_BUDGET_SECONDS,
        help="total native-client budget including setup and reserved cleanup time",
    )
    parser.add_argument(
        "--routing-budget-seconds",
        type=float,
        default=ROUTING_BUDGET_SECONDS,
        help="aggregate wall-clock budget shared by all live routing cases",
    )
    arguments = parser.parse_args()
    routing_corpus = (
        json.loads(arguments.routing_corpus.read_text(encoding="utf-8"))
        if arguments.routing_corpus
        else None
    )
    with tempfile.TemporaryDirectory(prefix="shaft-plugin-client-smoke-") as temporary_directory:
        package_root = Path(temporary_directory) / PACKAGE_NAME
        assemble(ROOT, package_root)
        evidence = collect_evidence(
            package_root,
            arguments.mode,
            routing_corpus=routing_corpus,
            execution_budget_seconds=arguments.execution_budget_seconds,
            routing_budget_seconds=arguments.routing_budget_seconds,
        )
    arguments.output.write_text(json.dumps(evidence, indent=2) + "\n", encoding="utf-8")
    for row in evidence["results"]:
        print(f"{row['client']}: {row['evidence_level']}: {row['verdict']}: {row['detail']}")
    return 1 if any(
        row["verdict"] in {"fail", "client_failure"}
        for row in evidence["results"]
    ) else 0


if __name__ == "__main__":
    raise SystemExit(main())
