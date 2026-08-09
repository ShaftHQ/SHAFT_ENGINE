#!/usr/bin/env python3
"""Collect four-level native client compatibility evidence for shaft-skills (#4641)."""

from __future__ import annotations

import argparse
import json
import os
import re
import subprocess
import tempfile
from pathlib import Path
from typing import Callable, Mapping

try:
    from scripts.ci.assemble_shaft_skills_plugin import assemble
except ModuleNotFoundError:  # Direct script execution places scripts/ci on sys.path.
    from assemble_shaft_skills_plugin import assemble

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


def _run(
    runner: Runner,
    command: list[str],
    cwd: Path,
    environment: Mapping[str, str],
    prompt: str | None = None,
) -> subprocess.CompletedProcess:
    try:
        return runner(
            command,
            cwd=cwd,
            env=dict(environment),
            input=prompt,
            text=True,
            capture_output=True,
            check=False,
            timeout=180,
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


def _client_evidence(
    client: str,
    package_root: Path,
    working_directory: Path,
    mode: str,
    runner: Runner,
    environment: Mapping[str, str],
    credentials: Mapping[str, str],
) -> list[dict]:
    commands = _claude_commands(package_root) if client == "claude" else _codex_commands(package_root)
    version_command = [client, "--version"]
    version_result = _run(runner, version_command, working_directory, environment)
    actual_version = f"{version_result.stdout or ''} {version_result.stderr or ''}".strip()
    version_matches = version_result.returncode == 0 and _version_matches(
        actual_version, CLIENTS[client]["version"]
    )
    rows: list[dict] = []
    preflight_commands = [
        [client, "plugin", "marketplace", "list", "--json"],
        [client, "plugin", "list", "--json"],
    ]
    preflight = [
        _run(runner, command, working_directory, environment) for command in preflight_commands
    ]
    preflight_passed = all(
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
                    result = _run(runner, command, working_directory, environment)
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
            rows.append(
                _row(
                    client,
                    actual_version,
                    "real_load",
                    "external_blocker",
                    load_commands,
                    f"required model credential {credential} is missing or disabled",
                )
            )
        else:
            live_environment = dict(environment)
            live_environment[credential] = credentials[credential]
            completed = [
                _run(
                    runner,
                    command,
                    working_directory,
                    live_environment,
                    LOAD_PROMPT if client == "codex" else None,
                )
                for command in load_commands
            ]
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
            cleanup_completed.append(_run(runner, cleanup[0], working_directory, environment))
        if marketplace_attempted:
            cleanup_commands.append(cleanup[1])
            cleanup_completed.append(_run(runner, cleanup[1], working_directory, environment))
        verification_commands: list[list[str]] = []
        verification_completed: list[subprocess.CompletedProcess] = []
        if install_attempted or marketplace_attempted:
            verification_commands = preflight_commands
            verification_completed = [
                _run(runner, command, working_directory, environment)
                for command in verification_commands
            ]
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
) -> dict:
    """Collect independent package, discovery, install, and load evidence."""
    if mode not in {"smoke", "live"}:
        raise ValueError("mode must be smoke or live")
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
            )
        )
    evidence = {
        "schema_version": 1,
        "package": PACKAGE_NAME,
        "mode": mode,
        "results": results,
    }
    secrets = tuple(
        secret
        for config in CLIENTS.values()
        if isinstance((secret := credentials.get(config["credential"])), str)
        and secret
    )
    redacted = _redact(evidence, secrets)
    assert isinstance(redacted, dict)
    return redacted


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--mode", choices=("smoke", "live"), default="smoke")
    parser.add_argument("--output", type=Path, default=Path("agent-plugin-client-evidence.json"))
    arguments = parser.parse_args()
    with tempfile.TemporaryDirectory(prefix="shaft-plugin-client-smoke-") as temporary_directory:
        package_root = Path(temporary_directory) / PACKAGE_NAME
        assemble(ROOT, package_root)
        evidence = collect_evidence(package_root, arguments.mode)
    arguments.output.write_text(json.dumps(evidence, indent=2) + "\n", encoding="utf-8")
    for row in evidence["results"]:
        print(f"{row['client']}: {row['evidence_level']}: {row['verdict']}: {row['detail']}")
    return 1 if any(row["verdict"] == "fail" for row in evidence["results"]) else 0


if __name__ == "__main__":
    raise SystemExit(main())
