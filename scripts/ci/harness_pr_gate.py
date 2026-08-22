#!/usr/bin/env python3
"""Run fast, change-scoped ChaosEngine pull-request checks."""

from __future__ import annotations

import argparse
import fnmatch
import json
import os
import re
import signal
import subprocess  # nosec B404 - executes fixed unittest commands without a shell.
import sys
import time
from dataclasses import dataclass
from datetime import datetime, timedelta, timezone
from pathlib import Path, PurePosixPath
from typing import Any


class GateError(ValueError):
    """Invalid gate input."""


class ChangedPath(str):
    """A changed path annotated with whether its head revision can execute."""

    executable: bool

    def __new__(cls, value: str, *, executable: bool) -> ChangedPath:
        instance = str.__new__(cls, value)
        instance.executable = executable
        return instance


@dataclass(frozen=True)
class Check:
    id: str
    surface: str
    modules: tuple[str, ...]
    protected: bool = False

    @property
    def reproduction_command(self) -> str:
        return "python -m unittest " + " ".join(self.modules) + " -v"


@dataclass(frozen=True)
class GatePlan:
    surfaces: tuple[str, ...] = ()
    checks: tuple[Check, ...] = ()
    unknown_paths: tuple[str, ...] = ()

    @property
    def test_modules(self) -> tuple[str, ...]:
        return tuple(dict.fromkeys(module for check in self.checks for module in check.modules))


@dataclass(frozen=True)
class WaiverReceipt:
    head_sha: str
    check_ids: tuple[str, ...]
    expires_at: datetime


PROTECTED_IDS = frozenset(
    {
        "protected-security",
        "protected-ownership",
        "protected-corruption",
        "protected-rollback",
        "protected-secret-safety",
        "protected-installer-acceptance",
        "protected-confirmed-correctness",
    }
)
OWNER_AUTHORIZATION = "@MohabMohie approved"
WAIVER_FENCE = re.compile(
    r"```chaos-engine-waiver[ \t]*\r?\n(.*?)\r?\n```", re.DOTALL
)


CHECKS = {
    "kernel-contract": Check(
        "kernel-contract", "kernel", ("tests.scripts.test_chaos_engine_kernel",)
    ),
    "lifecycle-contract": Check(
        "lifecycle-contract",
        "lifecycle",
        ("tests.scripts.test_chaos_engine_hook",),
    ),
    "host-contract": Check(
        "host-contract", "hosts", ("tests.scripts.test_chaos_engine_hosts",)
    ),
    "guidance-contract": Check(
        "guidance-contract",
        "guidance",
        ("tests.scripts.test_validate_agent_guidance",),
    ),
    "skill-contract": Check(
        "skill-contract", "guidance", ("tests.scripts.test_validate_skills",)
    ),
    "plugin-contract": Check(
        "plugin-contract",
        "plugins",
        ("tests.scripts.test_validate_agent_plugins",),
    ),
    "plugin-quality-contract": Check(
        "plugin-quality-contract", "plugins", ("tests.scripts.test_shaft_skill_quality",)
    ),
    "retrieval-contract": Check(
        "retrieval-contract",
        "retrieval",
        ("tests.scripts.test_knowledge_stores",),
    ),
    "graph-resolver-contract": Check(
        "graph-resolver-contract", "retrieval", ("tests.scripts.test_resolve_graph_out",)
    ),
    "ci-contract": Check(
        "ci-contract",
        "ci",
        ("tests.scripts.test_harness_pr_gate",),
    ),
    "setup-aggregator-contract": Check(
        "setup-aggregator-contract",
        "ci",
        ("tests.scripts.test_validate_agent_setup",),
    ),
    "fallback-contract": Check(
        "fallback-contract",
        "fallback",
        ("tests.scripts.test_validate_agent_setup",),
    ),
    "fallback-reachability": Check(
        "fallback-reachability",
        "fallback",
        ("tests.scripts.test_agent_harness_reachability",),
    ),
    "protected-ownership": Check(
        "protected-ownership",
        "protected",
        ("tests.scripts.test_validate_agent_ownership",),
        True,
    ),
    "protected-secret-safety": Check(
        "protected-secret-safety",
        "protected",
        (
            "tests.scripts.test_guard_nul_corruption",
            "tests.scripts.test_harness_pr_gate",
            "tests.scripts.test_guard_lifecycle.ReflectionCheckpointContractTest.test_receipt_rejects_stale_fingerprint_secret_and_user_path",
            "tests.scripts.test_guard_lifecycle.ReflectionReceiptPrivacyTest.test_session_token_and_closed_schema_reject_forged_receipts",
            "tests.scripts.test_guard_lifecycle.ReflectionReceiptPrivacyTest.test_failure_classifications_never_persist_secret_or_user_path",
        ),
        True,
    ),
    "protected-security": Check(
        "protected-security",
        "lifecycle",
        ("tests.scripts.test_guard_memory_worktree",),
        True,
    ),
    "protected-installer-acceptance": Check(
        "protected-installer-acceptance",
        "installer",
        (
            "tests.scripts.test_chaos_engine_bootstrap",
            "tests.scripts.test_chaos_engine_dependencies",
        ),
        True,
    ),
    "protected-rollback": Check(
        "protected-rollback",
        "installer",
        ("tests.scripts.test_chaos_engine_installer",),
        True,
    ),
}

SURFACE_CHECKS = {
    "kernel": ("kernel-contract",),
    "lifecycle": ("lifecycle-contract", "protected-security"),
    "hosts": ("host-contract",),
    "guidance": ("guidance-contract", "skill-contract"),
    "plugins": ("plugin-contract", "plugin-quality-contract"),
    "retrieval": ("retrieval-contract", "graph-resolver-contract"),
    "ci": ("ci-contract", "setup-aggregator-contract"),
    "installer": ("protected-installer-acceptance", "protected-rollback"),
    "fallback": ("fallback-contract", "fallback-reachability"),
}

SURFACE_PATTERNS = {
    "kernel": (
        "chaos-engine/hooks/kernel.py",
        "tests/scripts/test_chaos_engine_kernel.py",
    ),
    "installer": (
        "chaos-engine/bootstrap.py",
        "chaos-engine/dependencies.py",
        "chaos-engine/install.py",
        "chaos-engine/install.sh",
        "chaos-engine/install.ps1",
        "tests/scripts/test_chaos_engine_bootstrap.py",
        "tests/scripts/test_chaos_engine_dependencies.py",
        "tests/scripts/test_chaos_engine_installer.py",
        "tests/scripts/test_chaos_engine_install_wrappers.py",
    ),
    "hosts": (
        "chaos-engine/hosts.py",
        ".claude/settings.json",
        ".codex/hooks.json",
        "tests/scripts/test_chaos_engine_hosts.py",
    ),
    "lifecycle": (
        "chaos-engine/hooks/*",
        "scripts/agents/guard.py",
        "tests/scripts/test_chaos_engine_hook.py",
        "tests/scripts/test_guard*.py",
    ),
    "guidance": (
        "AGENTS.md",
        "CLAUDE.md",
        ".agents/*",
        ".claude/agents/*",
        ".claude/skills/*",
        ".codex/agents/*",
        "chaos-engine/skills/*",
        "chaos-engine/references/*",
        "chaos-engine/profiles/*",
        "chaos-engine/vendor/*",
        "tests/scripts/test_agent_router_contract.py",
        "tests/scripts/test_agent_harness_portability.py",
        "tests/scripts/test_validate_agent_guidance.py",
        "tests/scripts/test_validate_skills.py",
    ),
    "plugins": (
        "agent-plugins/*",
        "shaft-skills/*",
        ".claude-plugin/*",
        ".github/skills/*",
        "scripts/ci/*agent_plugin*",
        "scripts/ci/shaft_skill_*",
        "tests/scripts/test_*plugin*.py",
        "tests/scripts/test_shaft_skill*.py",
    ),
    "retrieval": (
        ".memory/*",
        ".mcp.json",
        "mempalace.yaml",
        "tools/repository-map/*",
        "scripts/agents/knowledge_stores.py",
        "scripts/ci/shaft_knowledge_refresh.py",
        "tests/scripts/test_knowledge_stores.py",
        "tests/scripts/test_resolve_*.py",
    ),
    "ci": (
        ".github/workflows/pr-gate.yml",
        ".github/workflows/agent-plugin-acceptance.yml",
        ".github/workflows/README.md",
        "scripts/ci/harness_pr_gate.py",
        "scripts/ci/agent_ownership.json",
        "scripts/ci/validate_agent_ownership.py",
        "scripts/ci/validate_agent_setup.py",
        "tests/scripts/test_harness_pr_gate.py",
        "tests/scripts/test_validate_agent_ownership.py",
        "tests/scripts/test_validate_agent_setup.py",
        "tests/scripts/test_validate_workflow_timeouts.py",
    ),
}

HARNESS_PREFIXES = (
    ".agents/",
    ".claude/",
    ".codex/",
    ".github/instructions/",
    ".github/skills/",
    ".github/workflows/",
    ".memory/",
    "agent-plugins/",
    "chaos-engine/",
    "scripts/agents/",
    "shaft-skills/",
    "tools/agent-infra/",
    "tools/repository-map/",
)
HARNESS_FILES = frozenset(
    {
        "AGENTS.md",
        "CLAUDE.md",
        ".mcp.json",
        "mempalace.yaml",
        ".github/copilot-instructions.md",
        "scripts/ci/validate_red_before_green.py",
        "scripts/ci/external_guardrail_corpus.py",
        "scripts/ci/external_guardrail_corpus.json",
        "scripts/ci/local_gate.py",
        "scripts/ci/build_retry.sh",
        "scripts/ci/extract_allure_failures.py",
        "scripts/ci/watch_pr_checks.py",
        "scripts/ci/worktree_hygiene.py",
        "tests/scripts/test_build_retry.py",
        "tests/scripts/test_extract_allure_failures.py",
        "tests/scripts/test_intellij_recording_powershell.py",
        "tests/scripts/test_repository_context.py",
        "tests/scripts/test_watch_pr_checks.py",
        "tests/scripts/test_worktree_hygiene.py",
        "tests/scripts/test_sync_user_harness.py",
        "tests/scripts/test_graphify_maintenance.py",
        "tests/scripts/test_shaft_knowledge_refresh.py",
    }
)
HARNESS_PATTERNS = (
    "scripts/ci/*agent*",
    "scripts/ci/*skill*",
    "scripts/ci/agnix*",
    "tests/scripts/test_agent*.py",
    "tests/scripts/test_guard*.py",
    "tests/scripts/test_chaos_engine*.py",
    "tools/intellij-plugin-recording/*",
)


def classify_paths(paths: list[str]) -> GatePlan:
    selected: list[str] = []
    unknown: list[str] = []
    changed_tests: list[Check] = []
    for raw_path in paths:
        executable = getattr(raw_path, "executable", True)
        path = raw_path.replace("\\", "/")
        while path.startswith("./"):
            path = path[2:]
        parts = PurePosixPath(path).parts
        if (
            not path
            or path.startswith("/")
            or "\0" in path
            or ".." in parts
            or (parts and parts[0].endswith(":"))
        ):
            raise GateError(f"unsafe changed path: {raw_path!r}")
        matched = False
        for surface, patterns in SURFACE_PATTERNS.items():
            if surface == "lifecycle" and path == "chaos-engine/hooks/kernel.py":
                continue
            if any(fnmatch.fnmatchcase(path, pattern) for pattern in patterns):
                if surface not in selected:
                    selected.append(surface)
                matched = True
        harness_path = (
            path in HARNESS_FILES
            or path.startswith(HARNESS_PREFIXES)
            or any(fnmatch.fnmatchcase(path, pattern) for pattern in HARNESS_PATTERNS)
        )
        if (
            harness_path
            and executable
            and path.startswith("tests/scripts/test_")
            and path.endswith(".py")
        ):
            module = ".".join(PurePosixPath(path).with_suffix("").parts)
            check_id = "protected-changed-test-" + PurePosixPath(path).stem.replace("_", "-")
            changed_tests.append(Check(check_id, "changed-test", (module,), True))
        if harness_path and not matched:
            unknown.append(path)
    if unknown and "fallback" not in selected:
        selected.append("fallback")
    if not selected and not changed_tests:
        return GatePlan()

    check_ids = [check_id for surface in selected for check_id in SURFACE_CHECKS[surface]]
    for protected_id in ("protected-ownership", "protected-secret-safety"):
        if protected_id not in check_ids:
            check_ids.append(protected_id)
    return GatePlan(
        tuple(selected),
        (
            *tuple(CHECKS[check_id] for check_id in dict.fromkeys(check_ids)),
            *tuple({check.id: check for check in changed_tests}.values()),
        ),
        tuple(sorted(set(unknown))),
    )


def parse_waiver(
    body: str, *, expected_head: str, now: datetime | None = None
) -> WaiverReceipt | None:
    matches = WAIVER_FENCE.findall(body)
    if not matches:
        if "chaos-engine-waiver" in body:
            raise GateError("waiver fence is malformed")
        return None
    if len(matches) != 1:
        raise GateError("exactly one waiver receipt is allowed")
    try:
        payload = json.loads(matches[0])
    except json.JSONDecodeError as error:
        raise GateError(f"waiver JSON is malformed: {error.msg}") from error
    if not isinstance(payload, dict):
        raise GateError("waiver must be a JSON object")
    required = {
        "schema",
        "head_sha",
        "check_ids",
        "expires_at",
        "owner_authorization",
        "rationale",
        "replacement_proof",
    }
    if set(payload) != required:
        raise GateError("waiver fields must exactly match schema")
    if payload["schema"] != 1:
        raise GateError("unsupported waiver schema")
    head_sha = payload["head_sha"]
    if not isinstance(head_sha, str) or not re.fullmatch(r"[0-9a-f]{40}", head_sha):
        raise GateError("waiver head_sha must be a full lowercase SHA")
    if head_sha != expected_head:
        raise GateError("waiver is stale for this PR head")
    check_ids = payload["check_ids"]
    if (
        not isinstance(check_ids, list)
        or not check_ids
        or len(check_ids) > 8
        or any(not isinstance(item, str) or not item for item in check_ids)
        or len(check_ids) != len(set(check_ids))
    ):
        raise GateError("waiver check_ids must be 1-8 unique exact IDs")
    if any("*" in item for item in check_ids):
        raise GateError("blanket waiver IDs are forbidden")
    if any(item in PROTECTED_IDS or item.startswith("protected-") for item in check_ids):
        raise GateError("protected checks cannot be waived")
    waivable = {check_id for check_id, check in CHECKS.items() if not check.protected}
    unknown = sorted(set(check_ids) - waivable)
    if unknown:
        raise GateError("unknown waiver check IDs: " + ", ".join(unknown))
    if payload["owner_authorization"] != OWNER_AUTHORIZATION:
        raise GateError("owner authorization marker is invalid")
    for field in ("rationale", "replacement_proof"):
        value = payload[field]
        if not isinstance(value, str) or not value.strip():
            raise GateError(f"waiver {field} must not be blank")
    try:
        expiry = datetime.fromisoformat(str(payload["expires_at"]).replace("Z", "+00:00"))
    except ValueError as error:
        raise GateError("waiver expires_at must be ISO-8601") from error
    if expiry.tzinfo is None:
        raise GateError("waiver expires_at must include a timezone")
    current = now or datetime.now(timezone.utc)
    expiry = expiry.astimezone(timezone.utc)
    current = current.astimezone(timezone.utc)
    if expiry <= current:
        raise GateError("waiver has expired")
    if expiry > current + timedelta(days=14):
        raise GateError("waiver expiry exceeds 14 days")
    return WaiverReceipt(head_sha, tuple(check_ids), expiry)


def render_json(plan: GatePlan, *, head_sha: str, budget_seconds: int) -> str:
    return json.dumps(
        {
            "schema": 1,
            "valid": True,
            "head_sha": head_sha,
            "surfaces": list(plan.surfaces),
            "unknown_paths": list(plan.unknown_paths),
            "checks": [
                {
                    "id": check.id,
                    "surface": check.surface,
                    "protected": check.protected,
                    "tests": list(check.modules),
                    "reproduction_command": check.reproduction_command,
                }
                for check in plan.checks
            ],
            "timing": {"budget_seconds": budget_seconds, "elapsed_seconds": 0.0},
            "safe_history_update_command": "git push --force-with-lease origin HEAD",
        },
        indent=2,
        sort_keys=True,
    )


def changed_paths(root: Path, base: str, head: str) -> list[ChangedPath]:
    completed = subprocess.run(  # nosec B603 - fixed read-only git invocation.
        ["git", "diff", "--diff-filter=ACDMRT", "--name-status", "-z", base, head],
        cwd=root,
        capture_output=True,
        text=True,
        timeout=30,
        check=False,
    )
    if completed.returncode:
        raise GateError("git could not resolve changed paths")
    tokens = [token for token in completed.stdout.split("\0") if token]
    paths: list[ChangedPath] = []
    index = 0
    while index < len(tokens):
        status = tokens[index]
        index += 1
        kind = status[:1]
        if kind not in {"A", "C", "D", "M", "R", "T"}:
            raise GateError(f"git returned an unsupported change status: {status!r}")
        required_paths = 2 if kind in {"C", "R"} else 1
        if index + required_paths > len(tokens):
            raise GateError("git returned malformed changed-path data")
        status_paths = tokens[index : index + required_paths]
        index += required_paths
        if kind in {"C", "R"}:
            paths.append(ChangedPath(status_paths[0], executable=False))
            paths.append(ChangedPath(status_paths[1], executable=True))
        else:
            path = status_paths[0]
            executable = kind in {"A", "M"} or (
                kind == "T" and (root / path).is_file()
            )
            paths.append(ChangedPath(path, executable=executable))
    return paths


def event_waiver(event_path: Path | None, expected_head: str) -> WaiverReceipt | None:
    if event_path is None:
        return None
    try:
        event = json.loads(event_path.read_text(encoding="utf-8"))
        pull_request = event.get("pull_request")
        if not pull_request:
            return None
        event_head = pull_request["head"]["sha"]
        body = pull_request.get("body") or ""
        author = pull_request["user"]["login"]
        association = pull_request["author_association"]
    except (OSError, json.JSONDecodeError, KeyError, TypeError) as error:
        raise GateError("GitHub event JSON is malformed") from error
    if event_head != expected_head:
        raise GateError("event head does not match requested head")
    if author != "MohabMohie" or association not in {"OWNER", "MEMBER"}:
        if "chaos-engine-waiver" in body:
            raise GateError("waiver requires an owner-authored pull request")
        return None
    return parse_waiver(body, expected_head=expected_head)


def _run_check(command: list[str], root: Path, timeout: float) -> tuple[str, int | None]:
    """Run one check with no captured output and terminate its process tree on timeout."""
    windows = os.name == "nt"
    process = subprocess.Popen(  # nosec B603 - fixed unittest command without a shell.
        command,
        cwd=root,
        stdout=subprocess.DEVNULL,
        stderr=subprocess.STDOUT,
        start_new_session=not windows,
        creationflags=subprocess.CREATE_NEW_PROCESS_GROUP if windows else 0,
    )
    try:
        exit_code = process.wait(timeout=timeout)
        return ("passed" if exit_code == 0 else "failed"), exit_code
    except subprocess.TimeoutExpired:
        if windows:
            subprocess.run(  # nosec B603 - fixed Windows process-tree termination.
                ["taskkill", "/PID", str(process.pid), "/T", "/F"],
                stdout=subprocess.DEVNULL,
                stderr=subprocess.DEVNULL,
                timeout=10,
                check=False,
            )
        else:
            try:
                os.killpg(process.pid, signal.SIGKILL)
            except ProcessLookupError:
                pass
        try:
            process.wait(timeout=10)
        except subprocess.TimeoutExpired:
            process.kill()
            process.wait()
        return "timeout", None


def run_plan(
    root: Path,
    plan: GatePlan,
    *,
    head_sha: str,
    budget_seconds: int,
    waiver: WaiverReceipt | None,
) -> tuple[dict[str, Any], int]:
    selected_ids = {check.id for check in plan.checks}
    if waiver and not set(waiver.check_ids) <= selected_ids:
        raise GateError("waiver names checks not selected by this change")
    started = time.monotonic()
    results: list[dict[str, Any]] = []
    failed_ids: set[str] = set()
    execution_cache: dict[tuple[str, ...], tuple[str, int | None, float]] = {}
    for check in plan.checks:
        remaining = budget_seconds - (time.monotonic() - started)
        check_started = time.monotonic()
        cached = execution_cache.get(check.modules)
        if cached:
            status, exit_code, duration = cached
        elif remaining <= 0:
            status, exit_code, duration = "timeout", None, 0.0
        else:
            status, exit_code = _run_check(
                [sys.executable, "-m", "unittest", *check.modules, "-v"],
                root,
                remaining,
            )
            duration = round(time.monotonic() - check_started, 3)
            execution_cache[check.modules] = status, exit_code, duration
        if status != "passed":
            failed_ids.add(check.id)
            if (
                status == "failed"
                and waiver
                and check.id in waiver.check_ids
                and not check.protected
            ):
                status = "waived"
        results.append(
            {
                "id": check.id,
                "surface": check.surface,
                "protected": check.protected,
                "tests": list(check.modules),
                "status": status,
                "exit_code": exit_code,
                "duration_seconds": duration,
                "reproduction_command": check.reproduction_command,
            }
        )
    if waiver and set(waiver.check_ids) != failed_ids.intersection(waiver.check_ids):
        raise GateError("waiver is stale because a named check did not fail")
    valid = all(result["status"] in {"passed", "waived"} for result in results)
    payload = {
        "schema": 1,
        "valid": valid,
        "head_sha": head_sha,
        "surfaces": list(plan.surfaces),
        "unknown_paths": list(plan.unknown_paths),
        "checks": results,
        "waiver": {
            "applied_check_ids": sorted(
                result["id"] for result in results if result["status"] == "waived"
            )
        },
        "timing": {
            "budget_seconds": budget_seconds,
            "elapsed_seconds": round(time.monotonic() - started, 3),
        },
        "safe_history_update_command": "git push --force-with-lease origin HEAD",
    }
    return payload, 0 if valid else 1


def render_text(payload: dict[str, Any]) -> str:
    surfaces = ",".join(payload["surfaces"]) or "none"
    lines = [
        f"harness-pr-gate valid={str(payload['valid']).lower()} surfaces={surfaces} "
        f"elapsed={payload['timing']['elapsed_seconds']}s/{payload['timing']['budget_seconds']}s"
    ]
    lines.extend(
        f"{item['id']} status={item['status']} protected={str(item['protected']).lower()} "
        f"tests={','.join(item['tests'])} reproduce={item['reproduction_command']}"
        for item in payload["checks"]
    )
    return "\n".join(lines)


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--root", type=Path, default=Path(__file__).resolve().parents[2])
    parser.add_argument("--base", required=True)
    parser.add_argument("--head", required=True)
    parser.add_argument("--event", type=Path)
    parser.add_argument("--budget-seconds", type=int, default=240)
    parser.add_argument("--output", type=Path)
    parser.add_argument("--format", choices=("text", "json"), default="text")
    parser.add_argument("--plan-only", action="store_true")
    return parser


def main() -> int:
    args = build_parser().parse_args()
    try:
        if args.budget_seconds < 1 or args.budget_seconds > 600:
            raise GateError("budget must be between 1 and 600 seconds")
        if not re.fullmatch(r"[0-9a-f]{40}", args.head):
            raise GateError("head must be a full lowercase SHA")
        root = args.root.resolve()
        plan = classify_paths(changed_paths(root, args.base, args.head))
        if args.plan_only:
            payload = json.loads(
                render_json(plan, head_sha=args.head, budget_seconds=args.budget_seconds)
            )
            exit_code = 0
        else:
            waiver = event_waiver(args.event, args.head)
            payload, exit_code = run_plan(
                root,
                plan,
                head_sha=args.head,
                budget_seconds=args.budget_seconds,
                waiver=waiver,
            )
    except GateError as error:
        payload = {"schema": 1, "valid": False, "error": str(error)}
        exit_code = 2
    serialized = json.dumps(payload, indent=2, sort_keys=True)
    if args.output:
        args.output.write_text(serialized + "\n", encoding="utf-8")
    print(serialized if args.format == "json" else render_text(payload) if "checks" in payload else serialized)
    return exit_code


if __name__ == "__main__":
    raise SystemExit(main())
