#!/usr/bin/env python3
"""Standalone SHAFT Agentic Tools installer used by the thin ps1/sh launchers."""

from __future__ import annotations

import argparse
import json
import os
import platform
import queue
import re
import shlex
import shutil
import ssl
import stat
import subprocess  # nosec B404 - installer runs trusted setup and validation commands.
import sys
import tarfile
import tempfile
import threading
import time
import tomllib
import urllib.error
import urllib.parse
import urllib.request
import xml.etree.ElementTree as ET  # nosec B314 - Maven metadata from HTTPS URLs this installer builds
import zipfile
from concurrent.futures import ThreadPoolExecutor, as_completed
from hashlib import sha1, sha256
from pathlib import Path
from typing import Any

SERVER_NAME = "shaft-mcp"
ARTIFACT_PATH = "io/github/shafthq/shaft-mcp"
DEFAULT_REPOSITORY = "https://repo.maven.apache.org/maven2"
MAIN_CLASS = "com.shaft.mcp.ShaftMcpApplication"
RUNTIME_DEPENDENCIES_ENTRY = "META-INF/shaft-mcp/runtime-dependencies.txt"
SHAFT_CLI_ARTIFACT_PATH = "io/github/shafthq/shaft-cli"
SHAFT_CLI_MAIN_CLASS = "com.shaft.commandline.ShaftCli"
# Explicit per-project override honored by shaft-mcp; the installer must never pin it
# globally ("shaft.mcp.workspaceRoot") or every non-IntelliJ client would be locked out
# of its own project. Only the fallback below is written to the launcher argfile.
FALLBACK_WORKSPACE_SYSTEM_PROPERTY = "shaft.mcp.fallbackWorkspaceRoot"
USER_GUIDE_URL = "https://shafthq.github.io/docs/agentic/mcp"
BOOTSTRAP_BANNER_SHOWN = "SHAFT_MCP_BOOTSTRAP_BANNER_SHOWN"
_OVERALL_TTY = [False]
SHAFT_SKILLS_DIRECTORY = "shaft-skills"
SHAFT_SKILLS_ROUTER = "shaft-developer"
SHAFT_SKILLS_NATIVE_DIRECTORIES = {
    "codex": (".agents/skills",),
    "claude": (".claude/skills",),
    "claude-desktop": (".claude/skills",),
    "copilot": (".github/skills",),
    "copilot-intellij": (".github/skills",),
    "grok": (".agents/skills",),
    "antigravity": (".agents/skills",),
    "intellij-plugin": (".agents/skills", ".claude/skills", ".github/skills"),
}
SHAFT_SKILLS_ALL_NATIVE_DIRECTORIES = (".agents/skills", ".claude/skills", ".github/skills")
RETIRED_SHAFT_SKILL_DIRECTORIES = frozenset((
    "act-as-shaft-dev",
    "analyzing-shaft-failures",
    "choosing-shaft-locators",
    "planning-shaft-tests",
    "recording-shaft-tests-with-mcp",
    "verifying-and-applying-shaft-changes",
    "writing-shaft-tests",
))
# Every module validate_agent_setup.py imports at module scope must be listed
# here, or the installed validator dies on ImportError in the user's project.
# tests/scripts/test_install_shaft_mcp.py reads the real import statements and
# fails when this list falls behind them.
AGENT_VALIDATION_SCRIPT_FILES = (
    "chaos-engine/dependencies.json",
    "chaos-engine/README.md",
    "chaos-engine/hooks/kernel.py",
    "chaos-engine/hooks/lifecycle.py",
    "scripts/agents/guard.py",
    "scripts/agents/session_worktree.py",
    "scripts/agents/reflection.py",
    "chaos-engine/hooks/reflection.py",
    "scripts/agents/learning_session.py",
    "scripts/agents/repository_context.py",
    "scripts/ci/validate_agent_setup.py",
    "scripts/ci/validate_chaos_engine_readme.py",
    "scripts/ci/validate_agent_ownership.py",
    "scripts/ci/validate_agent_guidance.py",
    "scripts/ci/harness_reachability.py",
    "scripts/ci/validate_documentation_boundaries.py",
    "scripts/ci/readme_contract.py",
    "scripts/ci/overlay_in_temp.py",
    "scripts/ci/overlay_pre_push.py",
    "scripts/ci/skill_inventory.py",
    "scripts/ci/validate_skills.py",
    "scripts/ci/worktree_hygiene.py",
    "scripts/ci/agent_guidance_budget.json",
    "scripts/ci/agent_ownership.json",
)
RETIRED_AGENT_VALIDATION_SCRIPT_FILES = (
    "scripts/agents/learning_loop.py",
)
AGENT_GUIDANCE_SCAFFOLD_MARKER = "AGENTS.md"
TARGETS = ("codex", "claude", "claude-desktop", "copilot", "copilot-intellij", "grok", "antigravity", "intellij-plugin")
TARGET_CHOICES = (
    ("codex", "Codex CLI / IDE"),
    ("claude", "Claude Code"),
    ("claude-desktop", "Claude Desktop"),
    ("copilot", "GitHub Copilot CLI"),
    ("copilot-intellij", "GitHub Copilot for IntelliJ IDEA"),
    ("grok", "Grok CLI"),
    ("antigravity", "Antigravity CLI"),
    ("intellij-plugin", "SHAFT IntelliJ IDEA plugin"),
)

# Lifecycle surface (#6644): match ChaosEngine installer commands with SHAFT Engine branding.
LIFECYCLE_COMMANDS = ("install", "status", "doctor", "repair", "rollback", "uninstall")
REPAIRABLE_COMPONENTS = ("java", "shaft-mcp", "shaft-cli", "shaft-skills", "host-config")
RECEIPT_NAME = "install-receipt.json"
HEAL_HANDOFF_NAME = "heal-handoff.md"
RECEIPT_SCHEMA_VERSION = 1
BRAND_NAME = "SHAFT Engine"
GUIDE_URL = USER_GUIDE_URL


class InstallError(RuntimeError):
    def __init__(self, message: str, code: int = 1) -> None:
        super().__init__(message)
        self.code = code


def log(message: str) -> None:
    print(message, file=sys.stderr)


def debug(message: str) -> None:
    if os.environ.get("SHAFT_MCP_DEBUG") == "1":
        log(f"install-shaft-agentic-tools debug: {message}")


def fail(message: str, code: int = 1) -> None:
    raise InstallError(message, code)


def banner() -> None:
    if os.environ.get(BOOTSTRAP_BANNER_SHOWN) == "1":
        return
    log(
        r"""
  ____  _   _    _    _____ _____
 / ___|| | | |  / \  |  ___|_   _|
 \___ \| |_| | / _ \ | |_    | |
  ___) |  _  |/ ___ \|  _|   | |
 |____/|_| |_/_/   \_\_|     |_|
              Agentic Tools
""".strip("\n"))


def human_bytes(value: int) -> str:
    amount = float(value)
    for unit in ("B", "KB", "MB", "GB"):
        if amount < 1024 or unit == "GB":
            return f"{amount:.1f} {unit}" if unit != "B" else f"{int(amount)} B"
        amount /= 1024
    return f"{value} B"


def progress(label: str, downloaded: int, total: int | None, final: bool = False) -> None:
    if not sys.stderr.isatty():
        return
    if total and total > 0:
        percent = min(1.0, downloaded / total)
        width = 28
        filled = int(percent * width)
        bar = "#" * filled + "-" * (width - filled)
        text = f"\r{label}: [{bar}] {percent:>6.1%} {human_bytes(downloaded)}/{human_bytes(total)}"
    else:
        text = f"\r{label}: {human_bytes(downloaded)} downloaded"
    print(text, end="", file=sys.stderr, flush=True)
    if final:
        print(file=sys.stderr)


def progress_count(label: str, completed: int, total: int, final: bool = False) -> None:
    if not sys.stderr.isatty() or total <= 0:
        return
    percent = min(1.0, completed / total)
    width = 28
    filled = int(percent * width)
    bar = "#" * filled + "-" * (width - filled)
    print(f"\r{label}: [{bar}] {percent:>6.1%} {completed}/{total}", end="", file=sys.stderr, flush=True)
    if final:
        print(file=sys.stderr)


def overall_progress(phase: str, completed: int, total: int, final: bool = False) -> None:
    percent = min(1.0, completed / total) if total else 0.0
    if not sys.stderr.isatty():
        _OVERALL_TTY[0] = False
        log(f"{phase}: {percent:.0%}")
        return
    width = 28
    filled = int(percent * width)
    bar = "#" * filled + "-" * (width - filled)
    line = f"Overall [{bar}] {percent:>6.1%} {phase}"
    if _OVERALL_TTY[0]:
        print(f"\033[1A\r{line}", file=sys.stderr, flush=True)
    else:
        print(line, file=sys.stderr, flush=True)
        _OVERALL_TTY[0] = True
    if final:
        _OVERALL_TTY[0] = False


def normalize_client(value: str | None) -> str | None:
    if value is None:
        return None
    normalized = value.strip()
    if normalized.startswith("--"):
        normalized = normalized[2:]
    return normalized or None


def render_client_menu() -> list[str]:
    """Render the interactive client-menu lines, grouped into labeled sections.

    Entries are numbered contiguously in TARGET_CHOICES order; only section
    headers and a one-line clarifier are inserted around them, so
    choose_client()'s numeric-or-name input loop stays unaffected by the
    grouping.
    """
    lines = ["Choose the MCP client to configure:", "AI agents:"]
    for index, (target, label) in enumerate(TARGET_CHOICES, start=1):
        if target == "intellij-plugin":
            lines.append("Advanced / IDE integration:")
            lines.append(f"  {index}. {label}")
            lines.append(
                "     (Configures the plugin's own MCP command; unnecessary "
                "inside the plugin's guided setup.)"
            )
        else:
            lines.append(f"  {index}. {label}")
    return lines


def choose_client() -> str:
    if not sys.stdin.isatty():
        fail("Pass a client target when running non-interactively.", 2)
    for line in render_client_menu():
        print(line)
    while True:
        answer = input("Enter a number: ").strip()
        if answer.isdigit():
            index = int(answer)
            if 1 <= index <= len(TARGET_CHOICES):
                return TARGET_CHOICES[index - 1][0]
        normalized = normalize_client(answer)
        if normalized in TARGETS:
            return normalized
        print(f"Enter a number from 1 to {len(TARGET_CHOICES)}, or a target name.")


def choose_component(prompt: str, default: bool) -> bool:
    suffix = "[Y/n]" if default else "[y/N]"
    print(f"{prompt} {suffix}: ", end="", file=sys.stderr, flush=True)
    try:
        answer = input().strip().lower()
    except EOFError:
        return default
    return default if not answer else answer not in {"n", "no"}


# Product-facing three-stage UX (platform parity #5943; Automation contract #5966 / S2-10).
# Help text must keep these stage names so SC-002 stays greppable from --help.
THREE_STAGE_UX_EPILOG = (
    "Product stages (Design, Automation, Reporting):\n"
    "  Design     — analysis and handoff into Automation\n"
    "  Automation — live record via MCP capture_* plus shaft capture / shaft codegen\n"
    "               (codegen stays offline; no plugin-only recording protocol)\n"
    "  Reporting  — Allure, Doctor, and healer (Stage 3; healing is never folded into record)"
)



def receipt_path() -> Path:
    return application_data_root() / RECEIPT_NAME


def previous_receipt_path() -> Path:
    return application_data_root() / "install-receipt.previous.json"


def read_receipt() -> dict[str, Any] | None:
    path = receipt_path()
    if not path.is_file():
        return None
    try:
        data = json.loads(path.read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError):
        return None
    if not isinstance(data, dict) or data.get("schemaVersion") != RECEIPT_SCHEMA_VERSION:
        return None
    return data


def write_receipt(receipt: dict[str, Any], *, dry_run: bool = False) -> Path:
    path = receipt_path()
    if dry_run:
        log(f"[dry-run] would write receipt {path}")
        return path
    path.parent.mkdir(parents=True, exist_ok=True)
    current = read_receipt()
    if current is not None:
        previous = previous_receipt_path()
        previous.write_text(json.dumps(current, indent=2, sort_keys=True) + "\n", encoding="utf-8")
    payload = dict(receipt)
    payload["schemaVersion"] = RECEIPT_SCHEMA_VERSION
    payload["brand"] = BRAND_NAME
    path.write_text(json.dumps(payload, indent=2, sort_keys=True) + "\n", encoding="utf-8")
    return path


def owned_file_record(path: Path) -> dict[str, Any]:
    record: dict[str, Any] = {"path": str(path)}
    if path.is_file():
        record["sha256"] = file_sha256(path)
        record["bytes"] = path.stat().st_size
    elif path.exists():
        record["kind"] = "directory"
    else:
        record["missing"] = True
    return record


def build_install_receipt(
    *,
    version: str | None,
    client: str | None,
    java: Path | None,
    mcp_jar: Path | None,
    cli_jar: Path | None,
    cli_launcher: Path | None,
    skills_paths: list[Path],
    args_file: Path | None,
    host_config: Path | None = None,
) -> dict[str, Any]:
    owned: list[dict[str, Any]] = []
    components: dict[str, Any] = {}
    if java is not None and java.exists():
        components["java"] = {"status": "healthy", "path": str(java), "feature": java_feature(java)}
        owned.append(owned_file_record(java))
    if mcp_jar is not None and mcp_jar.exists():
        components["shaft-mcp"] = {
            "status": "healthy",
            "version": version,
            "path": str(mcp_jar),
            "client": client,
        }
        owned.append(owned_file_record(mcp_jar))
        if args_file is not None and args_file.exists():
            owned.append(owned_file_record(args_file))
            components["shaft-mcp"]["argsFile"] = str(args_file)
    if cli_jar is not None and cli_jar.exists():
        components["shaft-cli"] = {"status": "healthy", "version": version, "path": str(cli_jar)}
        owned.append(owned_file_record(cli_jar))
        if cli_launcher is not None and cli_launcher.exists():
            owned.append(owned_file_record(cli_launcher))
            components["shaft-cli"]["launcher"] = str(cli_launcher)
    if skills_paths:
        components["shaft-skills"] = {
            "status": "healthy",
            "paths": [str(p) for p in skills_paths],
        }
        for skills_path in skills_paths:
            if skills_path.exists():
                owned.append(owned_file_record(skills_path))
    if client and client != "intellij-plugin" and host_config is not None:
        components["host-config"] = {"status": "healthy", "client": client, "path": str(host_config)}
        if java is not None:
            components["host-config"]["command"] = str(java)
        if args_file is not None:
            components["host-config"]["argsFile"] = str(args_file)
    return {
        "schemaVersion": RECEIPT_SCHEMA_VERSION,
        "brand": BRAND_NAME,
        "installedAt": time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime()),
        "version": version,
        "client": client,
        "components": components,
        "ownedFiles": owned,
        "userGuide": GUIDE_URL,
    }


def _probe_java_status(record: dict[str, Any]) -> tuple[str, str | None]:
    path = Path(str(record.get("path") or ""))
    if not path.is_file() or not is_java25(path):
        return "recovery-required", "Java 25 binary missing or not Java 25"
    return "healthy", None


def _probe_shaft_mcp_status(record: dict[str, Any], receipt: dict[str, Any]) -> tuple[str, str | None]:
    path = Path(str(record.get("path") or ""))
    if not path.is_file():
        return "recovery-required", "shaft-mcp jar missing"
    expected = None
    for owned in receipt.get("ownedFiles") or []:
        if isinstance(owned, dict) and owned.get("path") == str(path):
            expected = owned.get("sha256")
            break
    if expected and file_sha256(path) != expected:
        return "recovery-required", "shaft-mcp jar hash drift"
    return "healthy", None


def _probe_shaft_cli_status(record: dict[str, Any]) -> tuple[str, str | None]:
    path = Path(str(record.get("path") or ""))
    if path and not path.is_file():
        return "recovery-required", "shaft-cli jar missing"
    return "healthy", None


def _probe_shaft_skills_status(record: dict[str, Any]) -> tuple[str, str | None]:
    paths = record.get("paths") or []
    if not paths or not any(Path(str(p)).exists() for p in paths):
        return "recovery-required", "skills directory missing"
    return "healthy", None


def _probe_host_config_status(record: dict[str, Any]) -> tuple[str, str | None]:
    detail = host_config_drift(record)
    if detail:
        return "recovery-required", detail
    return "healthy", None


_COMPONENT_STATUS_PROBERS = {
    "java": lambda record, _receipt: _probe_java_status(record),
    "shaft-mcp": lambda record, receipt: _probe_shaft_mcp_status(record, receipt),
    "shaft-cli": lambda record, _receipt: _probe_shaft_cli_status(record),
    "shaft-skills": lambda record, _receipt: _probe_shaft_skills_status(record),
    "host-config": lambda record, _receipt: _probe_host_config_status(record),
}


def probe_component(name: str, receipt: dict[str, Any] | None) -> dict[str, Any]:
    components = (receipt or {}).get("components") if isinstance(receipt, dict) else None
    if not isinstance(components, dict) or name not in components:
        return {"status": "absent", "taskImpact": "optional" if name != "shaft-mcp" else "required"}
    record = dict(components[name])
    prober = _COMPONENT_STATUS_PROBERS.get(name)
    status, detail = prober(record, receipt or {}) if prober else ("healthy", None)
    record["status"] = status
    if detail:
        record["detail"] = detail
    record.setdefault("taskImpact", "required" if name in {"java", "shaft-mcp"} else "optional")
    return record


def host_config_drift(record: dict[str, Any]) -> str | None:
    """Re-read the client configuration and confirm it still launches the receipt's shaft-mcp."""
    client = str(record.get("client") or "")
    path = Path(str(record.get("path") or ""))
    java = Path(str(record.get("command") or ""))
    args_file = Path(str(record.get("argsFile") or ""))
    if not record.get("path") or not record.get("command") or not record.get("argsFile"):
        return "receipt does not record the host configuration; repair host-config"
    if not path.is_file():
        return f"host configuration missing: {path}"
    try:
        if client in {"codex", "grok"}:
            verify_grok_entry(path, java, args_file)
        else:
            verify_json_entry(path, "servers" if client == "copilot-intellij" else "mcpServers", java, args_file)
    except (InstallError, OSError, ValueError) as exc:
        return f"host configuration drift in {path}: {exc}"
    if not java.is_file():
        return f"configured Java command is missing: {java}"
    if not args_file.is_file():
        return f"configured launcher arguments file is missing: {args_file}"
    return None


def heal_handoff_path() -> Path:
    return application_data_root() / HEAL_HANDOFF_NAME


def write_heal_handoff(command: str, argv: list[str], error: "InstallError") -> Path | None:
    """CE-standard failure handoff: exact error, exact retry command, and an agent prompt."""
    retry = "python3 scripts/mcp/install_shaft_agentic_tools.py " + " ".join(shlex.quote(item) for item in argv)
    text = (
        f"# {BRAND_NAME} agentic tools heal handoff\n\n"
        f"- command: `{command}`\n"
        f"- exit code: {error.code}\n"
        f"- error: {error}\n"
        f"- retry after fixing the cause: `{retry.strip()}`\n"
        "- verify: `python3 scripts/mcp/install_shaft_agentic_tools.py doctor`\n\n"
        "## Agent prompt\n\n"
        f"The {BRAND_NAME} agentic tools {command} failed with the error above. Fix the cause it names "
        "(for example a malformed or conflicting MCP client configuration file: back it up, repair the JSON "
        "or TOML, and keep the user's other servers), then run the retry command and doctor. "
        "Do not hand-edit the shaft-mcp entry or invent an alternate installer.\n"
        f"\nGuide: {GUIDE_URL}\n"
    )
    path = heal_handoff_path()
    try:
        path.parent.mkdir(parents=True, exist_ok=True)
        write_text_atomically(path, text)
    except OSError:
        return None
    return path


def clear_heal_handoff() -> None:
    try:
        heal_handoff_path().unlink(missing_ok=True)
    except OSError:
        debug("heal handoff could not be removed")


def doctor_report(*, agent_summary: bool = False) -> dict[str, Any]:
    receipt = read_receipt()
    components = {name: probe_component(name, receipt) for name in REPAIRABLE_COMPONENTS}
    healthy = all(
        item.get("status") == "healthy"
        or (item.get("status") == "absent" and item.get("taskImpact") == "optional")
        for item in components.values()
    )
    # Required components that are absent without a receipt fail doctor.
    if receipt is None:
        healthy = False
        components = {
            name: {"status": "absent", "taskImpact": "required" if name in {"java", "shaft-mcp"} else "optional",
                   "detail": "no install receipt"}
            for name in REPAIRABLE_COMPONENTS
        }
    handoff = heal_handoff_path()
    if handoff.is_file() and not handoff.is_symlink():
        healthy = False
        message = f"Complete the agent heal using {handoff}, then rerun doctor."
        for item in components.values():
            if item.get("status") != "healthy" and not (
                    item.get("status") == "absent" and item.get("taskImpact") == "optional"):
                item["fixNext"] = message
    result = {
        "brand": BRAND_NAME,
        "status": "healthy" if healthy else "recovery-required",
        "components": components,
        "fixNext": (f"Complete the agent heal using {handoff}, then rerun doctor."
                    if handoff.is_file() and not handoff.is_symlink() else None),
        "receipt": str(receipt_path()) if receipt else None,
        "version": (receipt or {}).get("version"),
        "userGuide": GUIDE_URL,
    }
    if agent_summary:
        # Four-line CE-style summary.
        core = components.get("shaft-mcp") or {}
        drift = "none" if healthy else "present"
        print(f"doctor: {'pass' if healthy else 'fail'}")
        print(f"component: shaft-mcp {core.get('status', 'absent')}")
        print(f"hash: {(receipt or {}).get('version') or 'none'}")
        print(f"drift: {drift}")
    return result


def print_component_table(components: dict[str, Any]) -> None:
    print(f"{BRAND_NAME} Agentic Tools")
    print(f"{'Component':<16} {'Status':<20} Detail")
    print(f"{'-'*16} {'-'*20} {'-'*24}")
    for name, item in components.items():
        detail = str(item.get("detail") or item.get("path") or item.get("version") or "")
        print(f"{name:<16} {str(item.get('status')):<20} {detail}")


def cmd_status(args: argparse.Namespace) -> int:
    report = doctor_report(agent_summary=False)
    if args.json:
        print(json.dumps(report, separators=(",", ":")))
        return 0 if report["status"] == "healthy" else 1
    print_component_table(report["components"])
    print(f"Guide      {GUIDE_URL}")
    if report.get("receipt"):
        print(f"Receipt    {report['receipt']}")
        print(f"Version    {report.get('version') or 'unknown'}")
    else:
        print("Receipt    (none — run install first)")
    return 0 if report["status"] == "healthy" else 1


def cmd_doctor(args: argparse.Namespace) -> int:
    report = doctor_report(agent_summary=bool(args.agent_summary))
    if args.agent_summary:
        return 0 if report["status"] == "healthy" else 1
    if args.json:
        print(json.dumps(report, separators=(",", ":")))
        return 0 if report["status"] == "healthy" else 1
    print_component_table(report["components"])
    print(f"Doctor: {'healthy' if report['status'] == 'healthy' else 'recovery-required'}")
    print(f"Guide      {GUIDE_URL}")
    if report.get("fixNext"):
        print(f"fix-next: {report['fixNext']}")
    elif report["status"] != "healthy":
        print("fix-next: python3 scripts/mcp/install_shaft_agentic_tools.py repair --component <name>")
        print("           or re-run: python3 scripts/mcp/install_shaft_agentic_tools.py install ...")
    return 0 if report["status"] == "healthy" else 1


def cmd_repair(args: argparse.Namespace) -> int:
    component = args.component
    if component not in REPAIRABLE_COMPONENTS:
        fail(f"Unknown component: {component}", 2)
    receipt = read_receipt()
    if receipt is None and component != "shaft-skills":
        fail("No install receipt; run install before repair.", 4)
    if args.dry_run:
        log(f"[dry-run] would repair component {component}")
        return 0
    # Re-run the overlapping install path for the requested component.
    install_argv: list[str] = []
    client = (receipt or {}).get("client")
    version = (receipt or {}).get("version")
    if component in {"java", "shaft-mcp", "host-config"}:
        if not client:
            fail("Receipt has no client; cannot repair MCP/host-config.", 4)
        install_argv.extend(["--client", str(client)])
        if version:
            install_argv.extend(["--version", str(version)])
        if component == "host-config":
            install_argv.append("--skip-shaft-skills")
    elif component == "shaft-cli":
        install_argv.append("--install-shaft-cli")
        if version:
            install_argv.extend(["--version", str(version)])
    elif component == "shaft-skills":
        install_argv.append("--install-shaft-skills")
    if args.json:
        install_argv.append("--json")
    repair_args = parse_install_args(install_argv)
    install(repair_args)
    return 0


def cmd_rollback(args: argparse.Namespace) -> int:
    previous = previous_receipt_path()
    if not previous.is_file():
        fail("No previous receipt to roll back to.", 4)
    try:
        prior = json.loads(previous.read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError):
        fail("Previous receipt is unreadable; cannot roll back.", 4)
    if not isinstance(prior, dict):
        fail("Previous receipt is malformed; cannot roll back.", 4)
    prior_components = prior.get("components") if isinstance(prior.get("components"), dict) else {}
    if args.dry_run:
        log(f"[dry-run] would reinstall {BRAND_NAME} {prior.get('version') or 'unknown'} from {previous}")
        return 0
    current = receipt_path()
    if current.is_file():
        backup = application_data_root() / "install-receipt.rolled-forward.json"
        backup.write_text(current.read_text(encoding="utf-8"), encoding="utf-8")
    # Reinstall the previous version so files and host configuration match the restored receipt.
    install_argv: list[str] = []
    if prior.get("client") and "shaft-mcp" in prior_components:
        install_argv.extend(["--client", str(prior["client"])])
    if prior.get("version"):
        install_argv.extend(["--version", str(prior["version"])])
    if "shaft-cli" in prior_components:
        install_argv.append("--install-shaft-cli")
    install_argv.append("--install-shaft-skills" if "shaft-skills" in prior_components else "--skip-shaft-skills")
    if "--client" not in install_argv and "--install-shaft-cli" not in install_argv:
        current.parent.mkdir(parents=True, exist_ok=True)
        current.write_text(json.dumps(prior, indent=2, sort_keys=True) + "\n", encoding="utf-8")
    else:
        # The restored receipt describes only the previous install, not a merge with the newer one.
        current.unlink(missing_ok=True)
        install(parse_install_args(install_argv + ["--json"]) if args.json else parse_install_args(install_argv))
    previous.unlink(missing_ok=True)
    data = read_receipt() or {}
    result = {"status": "rolled-back", "version": data.get("version"), "receipt": str(current)}
    if args.json:
        print(json.dumps(result, separators=(",", ":")))
    else:
        print(f"{BRAND_NAME}: rolled back to receipt version {data.get('version') or 'unknown'}.")
        print(f"Receipt    {current}")
    return 0


def cmd_uninstall(args: argparse.Namespace) -> int:
    receipt = read_receipt()
    if receipt is None:
        fail("No install receipt; nothing to uninstall.", 4)
    owned = receipt.get("ownedFiles") or []
    removed: list[str] = []
    for item in owned:
        if not isinstance(item, dict):
            continue
        target = Path(str(item.get("path") or ""))
        if not target.exists():
            continue
        if args.dry_run:
            log(f"[dry-run] would remove {target}")
            removed.append(str(target))
            continue
        if target.is_file() or target.is_symlink():
            target.unlink(missing_ok=True)
            removed.append(str(target))
        # Directories (skills): only remove if empty of foreign content — leave in place.
    host = (receipt.get("components") or {}).get("host-config")
    if isinstance(host, dict) and host.get("path"):
        host_path = Path(str(host["path"]))
        if args.dry_run:
            log(f"[dry-run] would remove {SERVER_NAME} from {host_path}")
        elif remove_host_entry(str(host.get("client") or ""), host_path):
            removed.append(f"{host_path}#{SERVER_NAME}")
    if not args.dry_run:
        receipt_path().unlink(missing_ok=True)
        clear_heal_handoff()
    result = {"status": "uninstalled", "removed": removed, "brand": BRAND_NAME}
    if args.json:
        print(json.dumps(result, separators=(",", ":")))
    else:
        print(f"{BRAND_NAME}: uninstalled {len(removed)} owned file(s).")
        print(f"Guide      {GUIDE_URL}")
    return 0


def remove_host_entry(client: str, path: Path) -> bool:
    """Remove only the shaft-mcp entry, keeping every other server and setting in the file."""
    if not path.is_file():
        return False
    if client in {"codex", "grok"}:
        text = path.read_text(encoding="utf-8")
        header = _GROK_SERVER_HEADER.search(text)
        if not header:
            return False
        next_header = re.search(r"(?m)^\s*\[", text[header.end():])
        end = header.end() + next_header.start() if next_header else len(text)
        updated = text[:header.start()] + text[end:].lstrip("\n")
        write_text_atomically(path, updated)
        return True
    try:
        root = read_json_object(path)
    except (json.JSONDecodeError, InstallError):
        log(f"Left {path} unchanged: it is not valid JSON.")
        return False
    servers = root.get("servers" if client == "copilot-intellij" else "mcpServers")
    if not isinstance(servers, dict) or SERVER_NAME not in servers:
        return False
    del servers[SERVER_NAME]
    write_json_atomically(path, root)
    return True


def host_config_path(client: str | None) -> Path | None:
    """The file configure_client writes for this client (project override aware)."""
    if not client or client == "intellij-plugin":
        return None
    if client == "grok":
        return grok_write_path()
    if client == "antigravity":
        return antigravity_write_path()
    return configuration_path(client).resolve()


def merge_with_previous_receipt(receipt: dict[str, Any]) -> dict[str, Any]:
    """Component-scoped installs (repair) keep the components they did not touch."""
    current = read_receipt()
    if current is None:
        return receipt
    components = dict(current.get("components") or {})
    components.update(receipt.get("components") or {})
    paths = {item.get("path") for item in receipt.get("ownedFiles") or [] if isinstance(item, dict)}
    owned = [item for item in current.get("ownedFiles") or []
             if isinstance(item, dict) and item.get("path") not in paths]
    merged = dict(receipt)
    merged["components"] = components
    merged["ownedFiles"] = owned + list(receipt.get("ownedFiles") or [])
    merged["client"] = receipt.get("client") or current.get("client")
    merged["version"] = receipt.get("version") or current.get("version")
    return merged


def build_install_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        description=(
            "Install and configure shaft-mcp for a supported MCP client.\n"
            "Covers SHAFT's three product stages: Design, Automation, Reporting."
        ),
        epilog=THREE_STAGE_UX_EPILOG,
        formatter_class=argparse.RawTextHelpFormatter,
    )
    parser.add_argument("--client", choices=TARGETS)
    env_version = (os.environ.get("SHAFT_MCP_VERSION") or "").strip()
    parser.add_argument("--version", nargs="?", const="LATEST", default=env_version or "LATEST")
    parser.add_argument("--json", action="store_true", help="Print machine-readable install details to stdout.")
    parser.add_argument(
        "--dry-run",
        action="store_true",
        help="Show what install would do without writing files or a receipt.",
    )
    parser.add_argument(
        "--install-shaft-skills",
        action="store_true",
        help="Install SHAFT agent skills into the current directory without prompting.",
    )
    parser.add_argument(
        "--skip-shaft-skills",
        action="store_true",
        help="Do not install SHAFT agent skills into the current directory.",
    )
    parser.add_argument(
        "--install-shaft-cli",
        action="store_true",
        help="Also install shaft-cli, a command-line client over the shaft-mcp tool set.",
    )
    for target, _ in TARGET_CHOICES:
        parser.add_argument(f"--{target}", action="store_true", dest=target.replace("-", "_"))
    parser.add_argument(
        "target",
        nargs="?",
        help="Optional target name: codex, claude, claude-desktop, copilot, copilot-intellij, grok, or antigravity.",
    )
    return parser


def normalize_install_namespace(args: argparse.Namespace) -> argparse.Namespace:
    # Normalize empty or whitespace-only version to LATEST
    if isinstance(args.version, str):
        args.version = (args.version.strip() or "LATEST")

    selected: list[str] = []
    if args.client:
        selected.append(args.client)
    if args.target:
        selected.append(normalize_client(args.target) or "")
    for target, _ in TARGET_CHOICES:
        if getattr(args, target.replace("-", "_"), False):
            selected.append(target)

    selected = [target for target in selected if target]
    if len(set(selected)) > 1:
        fail("Specify only one MCP client target.", 2)
    if args.install_shaft_skills and args.skip_shaft_skills:
        fail("Specify only one of --install-shaft-skills or --skip-shaft-skills.", 2)
    args.client = selected[0] if selected else None
    args.install_mcp = args.client is not None
    has_component_selector = args.install_mcp or args.install_shaft_cli or args.install_shaft_skills
    if not has_component_selector:
        if not sys.stdin.isatty():
            fail("Pass a component selector when running non-interactively.", 2)
        args.install_mcp = choose_component("Install and configure shaft-mcp?", True)
        args.install_shaft_cli = choose_component("Install shaft-cli?", False)
        args.install_shaft_skills = False if args.skip_shaft_skills else choose_component(
            "Install SHAFT skills into the current directory?", True)
    if args.install_mcp and args.client is None:
        args.client = choose_client()
    if args.client is not None and args.client not in TARGETS:
        fail("Usage: install_shaft_agentic_tools.py [--client <codex|claude|claude-desktop|copilot|copilot-intellij|grok|antigravity|intellij-plugin>]", 2)
    if not hasattr(args, "dry_run"):
        args.dry_run = False
    args.command = getattr(args, "command", "install")
    return args


def parse_install_args(argv: list[str]) -> argparse.Namespace:
    return normalize_install_namespace(build_install_parser().parse_args(argv))


def parse_args(argv: list[str]) -> argparse.Namespace:
    """
    Parse CLI args.

    Lifecycle commands (#6644): install|status|doctor|repair|rollback|uninstall.
    Legacy one-liner / flag form (no subcommand) remains an implicit install so
    existing URLs and IntelliJ invocations keep working.
    """
    if argv and argv[0] in LIFECYCLE_COMMANDS:
        command = argv[0]
        rest = argv[1:]
        if command == "install":
            args = parse_install_args(rest)
            args.command = "install"
            return args
        parser = argparse.ArgumentParser(prog=f"install_shaft_agentic_tools.py {command}")
        if command in {"status", "doctor"}:
            parser.add_argument("--json", action="store_true")
            parser.add_argument(
                "--agent-summary",
                action="store_true",
                help="Print at most four lines: pass/fail, component, hash, drift.",
            )
        elif command == "repair":
            parser.add_argument("--component", required=True, choices=REPAIRABLE_COMPONENTS)
            parser.add_argument("--json", action="store_true")
            parser.add_argument("--dry-run", action="store_true")
        elif command in {"rollback", "uninstall"}:
            parser.add_argument("--json", action="store_true")
            parser.add_argument("--dry-run", action="store_true")
        args = parser.parse_args(rest)
        args.command = command
        return args
    args = parse_install_args(argv)
    args.command = "install"
    return args


def system_name() -> str:
    name = platform.system()
    if name not in {"Windows", "Darwin", "Linux"}:
        fail(f"Unsupported operating system: {name}", 3)
    return name


def architecture() -> tuple[str, str]:
    machine = platform.machine().lower()
    if machine in {"amd64", "x86_64"}:
        return "x64", "x86_64"
    if machine in {"arm64", "aarch64"}:
        return "aarch64", "aarch64"
    fail(f"Unsupported architecture: {platform.machine()}", 3)


def home() -> Path:
    return Path.home()


def bootstrap_root() -> Path:
    override = os.environ.get("SHAFT_MCP_BOOTSTRAP_HOME")
    if override:
        return Path(override).expanduser().resolve()
    name = system_name()
    if name == "Windows":
        base = Path(os.environ.get("LOCALAPPDATA") or home() / "AppData" / "Local")
        return base / "ShaftHQ" / "shaft-mcp" / "bootstrap"
    if name == "Darwin":
        return home() / "Library" / "Caches" / "ShaftHQ" / "shaft-mcp-bootstrap"
    base = Path(os.environ.get("XDG_CACHE_HOME") or home() / ".cache")
    return base / "shafthq" / "shaft-mcp-bootstrap"


def application_data_root() -> Path:
    name = system_name()
    if name == "Windows":
        base = Path(os.environ.get("LOCALAPPDATA") or home() / "AppData" / "Local")
        return base / "ShaftHQ" / "shaft-mcp"
    if name == "Darwin":
        return home() / "Library" / "Application Support" / "ShaftHQ" / "shaft-mcp"
    base = Path(os.environ.get("XDG_DATA_HOME") or home() / ".local" / "share")
    return base / "shafthq" / "shaft-mcp"


def shaft_cli_application_data_root() -> Path:
    name = system_name()
    if name == "Windows":
        base = Path(os.environ.get("LOCALAPPDATA") or home() / "AppData" / "Local")
        return base / "ShaftHQ" / "shaft-cli"
    if name == "Darwin":
        return home() / "Library" / "Application Support" / "ShaftHQ" / "shaft-cli"
    base = Path(os.environ.get("XDG_DATA_HOME") or home() / ".local" / "share")
    return base / "shafthq" / "shaft-cli"


def maven_local_repository() -> Path:
    """
    The local Maven repository runtime dependencies are installed into.

    Sharing the standard Maven layout means future SHAFT projects built with
    Maven reuse the exact same artifacts instead of re-downloading them, and a
    shaft-mcp reinstall skips every dependency a Maven build already fetched.
    Resolution order matches Maven's own: explicit override, then the
    <localRepository> configured in ~/.m2/settings.xml, then ~/.m2/repository.
    """
    override = os.environ.get("SHAFT_MCP_MAVEN_LOCAL_REPOSITORY")
    if override:
        return Path(override).expanduser().resolve()
    m2 = home() / ".m2"
    configured = configured_local_repository(m2 / "settings.xml")
    if configured is not None:
        return configured
    return m2 / "repository"


def configured_local_repository(settings_xml: Path) -> Path | None:
    # The single localRepository text element is extracted with a regex rather than an XML
    # parser: xml.etree is flagged as unsafe for this and settings.xml needs no structure
    # beyond this one flat tag.
    try:
        if not settings_xml.is_file():
            return None
        text = settings_xml.read_text(encoding="utf-8")
    except OSError:
        return None
    match = re.search(r"<localRepository>([^<]+)</localRepository>", text)
    if not match or not match.group(1).strip():
        return None
    value = match.group(1).strip().replace("${user.home}", str(home()))
    if "${" in value:
        # Unresolvable Maven property interpolation; fall back to the default.
        return None
    return Path(value).expanduser().resolve()


def _is_http_404(exc: BaseException) -> bool:
    return isinstance(exc, urllib.error.HTTPError) and exc.code == 404


def raise_download_error(error: BaseException | None, url: str) -> None:
    if error is None:
        fail(f"Failed to download {url} without an error.", 4)
    else:
        raise error


def require_allowed_url(url: str) -> str:
    parsed = urllib.parse.urlparse(url)
    scheme = parsed.scheme.lower()
    if scheme == "https" and parsed.netloc:
        return url
    if scheme == "file" and parsed.path:
        return url
    fail(f"Refusing URL scheme {parsed.scheme or 'missing'}: HTTPS or local file only.", 4)
    return url


def download_bytes(url: str, attempts: int = 5) -> bytes:
    require_allowed_url(url)
    headers = {"User-Agent": "shaft-agentic-tools-installer"}
    last_error: BaseException | None = None
    for attempt in range(1, attempts + 1):
        try:
            request = urllib.request.Request(url, headers=headers)
            with urllib.request.urlopen(request, timeout=120) as response:  # nosec B310 - HTTPS or local file via require_allowed_url
                return response.read()
        except urllib.error.HTTPError as exc:
            if exc.code == 404:
                raise
            last_error = exc
        except (urllib.error.URLError, TimeoutError, ssl.SSLError) as exc:
            if _is_http_404(exc):
                raise
            last_error = exc
        time.sleep(min(attempt * 2, 10))
    raise_download_error(last_error, url)


def download_file(
        url: str,
        output: Path,
        label: str | None = None,
        show_progress: bool = True,
        announce: bool = True) -> None:
    require_allowed_url(url)
    output.parent.mkdir(parents=True, exist_ok=True)
    temporary = output.with_name(f".{output.name}.{os.getpid()}.tmp")
    headers = {"User-Agent": "shaft-agentic-tools-installer"}
    last_error: BaseException | None = None
    display = label or output.name
    for attempt in range(1, 6):
        try:
            request = urllib.request.Request(url, headers=headers)
            if announce:
                log(f"Downloading {display}...")
            with urllib.request.urlopen(request, timeout=120) as response, temporary.open("wb") as target:  # nosec B310 - HTTPS or local file via require_allowed_url
                length = response.headers.get("Content-Length")
                total = int(length) if length and length.isdigit() else None
                downloaded = 0
                last_update = 0.0
                while True:
                    chunk = response.read(1024 * 1024)
                    if not chunk:
                        break
                    target.write(chunk)
                    downloaded += len(chunk)
                    now = time.monotonic()
                    if show_progress and (now - last_update >= 0.1):
                        progress(display, downloaded, total)
                        last_update = now
                if show_progress:
                    progress(display, downloaded, total, final=True)
            os.replace(temporary, output)
            return
        except urllib.error.HTTPError as exc:
            if exc.code == 404:
                temporary.unlink(missing_ok=True)
                raise
            last_error = exc
            temporary.unlink(missing_ok=True)
            time.sleep(min(attempt * 2, 10))
        except (urllib.error.URLError, TimeoutError, OSError, ssl.SSLError) as exc:
            if _is_http_404(exc):
                temporary.unlink(missing_ok=True)
                raise
            last_error = exc
            temporary.unlink(missing_ok=True)
            time.sleep(min(attempt * 2, 10))
    raise_download_error(last_error, url)


def url_text(url: str) -> str:
    return download_bytes(url).decode("utf-8")


def url_text_or_none(url: str) -> str | None:
    try:
        return download_bytes(url).decode("utf-8")
    except urllib.error.HTTPError as exc:
        if exc.code == 404:
            return None
        raise


def file_digest(path: Path, algorithm: str) -> str:
    digest = sha256() if algorithm == "sha256" else sha1()
    with path.open("rb") as source:
        for chunk in iter(lambda: source.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


def file_sha256(path: Path) -> str:
    return file_digest(path, "sha256")


def java_feature(java: Path) -> int | None:
    try:
        result = subprocess.run(  # nosec B603 - java is the Adoptium/PATH binary this installer resolved
            [str(java), "-version"], text=True, capture_output=True, timeout=20)
    except (OSError, subprocess.SubprocessError):
        return None
    if result.returncode != 0:
        return None
    output = f"{result.stdout}\n{result.stderr}"
    match = re.search(r'version "([^"]+)"', output) or re.search(r"openjdk\s+([0-9][^\s]*)", output)
    if not match:
        return None
    raw = match.group(1)
    if raw.startswith("1."):
        parts = raw.split(".")
        return int(parts[1]) if len(parts) > 1 and parts[1].isdigit() else None
    first = re.sub(r"[._-].*$", "", raw)
    return int(first) if first.isdigit() else None


def is_java25(java: Path) -> bool:
    return java_feature(java) == 25


def find_cached_java(root: Path) -> Path | None:
    if not root.is_dir():
        return None
    executable = "java.exe" if system_name() == "Windows" else "java"
    for candidate in root.rglob(executable):
        if candidate.parent.name != "bin":
            continue
        feature = java_feature(candidate)
        debug(f"Java candidate {candidate} has feature version {feature}.")
        if feature == 25:
            return candidate.resolve()
    return None


def extract_archive(archive: Path, destination: Path) -> None:
    if archive.suffix == ".zip":
        with zipfile.ZipFile(archive) as zip_file:
            zip_file.extractall(destination)
        return
    with tarfile.open(archive, "r:*") as tar_file:
        tar_file.extractall(destination)


def adoptium_os_arch() -> tuple[str, str, str]:
    name = system_name()
    arch, _ = architecture()
    if name == "Windows":
        return "windows", arch, "zip"
    if name == "Darwin":
        return "mac", arch, "tar.gz"
    return "linux", arch, "tar.gz"


def download_java25(root: Path) -> Path:
    os_name, arch, extension = adoptium_os_arch()
    tools_dir = root / "tools"
    java_root = tools_dir / "jdk" / f"temurin-25-{os_name}-{arch}"
    cached = find_cached_java(java_root)
    if cached:
        return cached

    archive = root / "downloads" / f"temurin-jdk-25-{os_name}-{arch}.{extension}"
    url = f"https://api.adoptium.net/v3/binary/latest/25/ga/{os_name}/{arch}/jdk/hotspot/normal/eclipse"
    if java_root.exists():
        shutil.rmtree(java_root)
    java_root.mkdir(parents=True, exist_ok=True)
    download_file(url, archive, f"Java 25 for {os_name} {arch}")
    extract_archive(archive, java_root)
    java = find_cached_java(java_root)
    if not java:
        fail("Downloaded Java archive did not contain bin/java.", 3)
    return java


def get_java25(root: Path) -> Path:
    force = os.environ.get("SHAFT_MCP_FORCE_BOOTSTRAP_JAVA") == "1"
    executable = "java.exe" if system_name() == "Windows" else "java"
    if not force:
        java_home = os.environ.get("JAVA_HOME")
        if java_home:
            candidate = Path(java_home) / "bin" / executable
            debug(f"Checking JAVA_HOME Java candidate {candidate}.")
            if candidate.is_file() and is_java25(candidate):
                log(f"Java 25 found via JAVA_HOME at {candidate} (download skipped).")
                return candidate.resolve()
        path_java = shutil.which(executable) or shutil.which("java")
        debug(f"Checking PATH Java candidate {path_java}.")
        if path_java and is_java25(Path(path_java)):
            log(f"Java 25 found on PATH at {path_java} (download skipped).")
            return Path(path_java).resolve()
        cached = find_cached_java(root / "tools" / "jdk")
        if cached:
            log(f"Java 25 found in the SHAFT bootstrap cache at {cached} (download skipped).")
            return cached
    return download_java25(root)


def java_home_for(java: Path) -> Path:
    return java.resolve().parent.parent


def resolve_shaft_mcp_version(requested_version: str | None, repository: str, root: Path) -> str:
    requested = (requested_version or "LATEST").strip()
    if not requested or requested == "":
        requested = "LATEST"
    if requested and requested != "LATEST":
        return requested
    log("Resolving io.github.shafthq:shaft-mcp:LATEST...")
    metadata_url = f"{repository}/{ARTIFACT_PATH}/maven-metadata.xml"
    metadata_path = root / "downloads" / "shaft-mcp-maven-metadata.xml"
    download_file(metadata_url, metadata_path, "shaft-mcp Maven metadata", show_progress=False)
    try:
        xml_root = ET.fromstring(metadata_path.read_text(encoding="utf-8"))  # nosec B314 - HTTPS Maven metadata this installer downloaded
    except ET.ParseError as exc:
        fail(f"Maven Central metadata is malformed: {exc}", 4)
    versioning = xml_root.find("versioning")
    values: list[str] = []
    if versioning is not None:
        for element_name in ("release", "latest"):
            element = versioning.find(element_name)
            if element is not None and element.text and element.text.strip():
                values.append(element.text.strip())
        versions = versioning.find("versions")
        if versions is not None:
            for version in versions.findall("version"):
                if version.text and version.text.strip():
                    values.append(version.text.strip())
    if not values:
        fail("Maven Central metadata did not contain a shaft-mcp version.", 4)
    return values[0]


def expected_sha256(url: str) -> str:
    value = url_text(f"{url}.sha256").strip().split()[0].lower()
    if not re.fullmatch(r"[0-9a-f]{64}", value):
        fail(f"Invalid SHA-256 checksum for {url}.", 4)
    return value


def expected_checksum(url: str) -> tuple[str, str]:
    for algorithm, pattern in (("sha256", r"[0-9a-f]{64}"), ("sha1", r"[0-9a-f]{40}")):
        value = url_text_or_none(f"{url}.{algorithm}")
        if value is None:
            continue
        digest = value.strip().split()[0].lower()
        if not re.fullmatch(pattern, digest):
            fail(f"Invalid {algorithm.upper()} checksum for {url}.", 4)
        return algorithm, digest
    fail(f"No SHA-256 or SHA-1 checksum was found for {url}.", 4)


def install_shaft_mcp_jar(version: str, repository: str, root: Path) -> Path:
    version_path = f"{ARTIFACT_PATH}/{version}"
    filename = f"shaft-mcp-{version}.jar"
    url = f"{repository}/{version_path}/{filename}"
    expected = expected_sha256(url)
    target_dir = application_data_root() / "versions" / version
    target = target_dir / "shaft-mcp.jar"
    maven_target = maven_local_repository() / version_path / filename
    target_current = target.is_file() and file_sha256(target) == expected
    maven_current = maven_target.is_file() and file_sha256(maven_target) == expected
    if target_current and maven_current:
        log(f"shaft-mcp {version} is already installed and up to date (download skipped).")
        return target.resolve()

    if target_current or maven_current:
        # One verified copy already exists locally; mirror it instead of re-downloading.
        source = target if target_current else maven_target
        log(f"shaft-mcp {version} found locally at {source} (download skipped).")
    else:
        source = root / "downloads" / filename
        download_file(url, source, f"io.github.shafthq:shaft-mcp:{version}")
        if file_sha256(source) != expected:
            fail(f"Checksum verification failed for {source}.", 4)

    if not target_current:
        copy_verified(source, target, expected, "Installed shaft-mcp JAR")
    if not maven_current:
        copy_verified(source, maven_target, expected, "Local Maven repository shaft-mcp JAR")
    return target.resolve()


def install_shaft_cli_jar(version: str, repository: str, root: Path) -> Path:
    """
    Downloads and verifies shaft-cli's self-contained shaded jar. Unlike shaft-mcp's thin
    jar, shaft-cli has no runtime-dependency manifest to resolve and is not a build-time
    Maven dependency, so there is no local Maven repository mirror to maintain here.
    """
    version_path = f"{SHAFT_CLI_ARTIFACT_PATH}/{version}"
    filename = f"shaft-cli-{version}.jar"
    url = f"{repository}/{version_path}/{filename}"
    expected = expected_sha256(url)
    target_dir = shaft_cli_application_data_root() / "versions" / version
    target = target_dir / "shaft-cli.jar"
    if target.is_file() and file_sha256(target) == expected:
        log(f"shaft-cli {version} is already installed and up to date (download skipped).")
        return target.resolve()

    source = root / "downloads" / filename
    download_file(url, source, f"io.github.shafthq:shaft-cli:{version}")
    if file_sha256(source) != expected:
        fail(f"Checksum verification failed for {source}.", 4)

    copy_verified(source, target, expected, "Installed shaft-cli JAR")
    return target.resolve()


def copy_verified(source: Path, target: Path, expected_sha256_digest: str, label: str) -> None:
    target.parent.mkdir(parents=True, exist_ok=True)
    temporary = target.with_name(f".{target.name}.{os.getpid()}.tmp")
    shutil.copyfile(source, temporary)
    if file_sha256(temporary) != expected_sha256_digest:
        temporary.unlink(missing_ok=True)
        fail(f"{label} failed SHA-256 verification.", 4)
    if not replace_with_retry(temporary, target):
        temporary.unlink(missing_ok=True)
        fail(f"{label} could not be written: {target} is locked by another process (for example a "
             "running shaft-mcp or IDE JVM). Close it and re-run this installer.", 4)
    if file_sha256(target) != expected_sha256_digest:
        fail(f"{label} failed SHA-256 verification.", 4)


def parse_runtime_dependency_manifest(text: str) -> list[tuple[str, str, str, str | None]]:
    dependencies: list[tuple[str, str, str, str | None]] = []
    seen: set[tuple[str, str, str, str | None]] = set()
    for raw_line in text.splitlines():
        line = raw_line.strip()
        if not line or line.startswith("The following"):
            continue
        token = line.split()[0]
        parts = token.split(":")
        if len(parts) == 5:
            group_id, artifact_id, packaging, version, scope = parts
            classifier = None
        elif len(parts) == 6:
            group_id, artifact_id, packaging, classifier, version, scope = parts
        else:
            fail(f"Malformed runtime dependency coordinate: {line}", 4)
        if packaging != "jar" or scope == "test":
            continue
        dependency = (group_id, artifact_id, version, classifier)
        if dependency not in seen:
            seen.add(dependency)
            dependencies.append(dependency)
    if not dependencies:
        fail("shaft-mcp runtime dependency manifest did not contain any JAR dependencies.", 4)
    return dependencies


def read_runtime_dependencies(jar: Path) -> list[tuple[str, str, str, str | None]]:
    try:
        with zipfile.ZipFile(jar) as archive:
            manifest = archive.read(RUNTIME_DEPENDENCIES_ENTRY).decode("utf-8")
    except KeyError:
        fail(f"Installed shaft-mcp JAR is missing {RUNTIME_DEPENDENCIES_ENTRY}.", 4)
    except zipfile.BadZipFile as exc:
        fail(f"Installed shaft-mcp JAR is not readable: {exc}", 4)
    return parse_runtime_dependency_manifest(manifest)


def dependency_url(repository: str, dependency: tuple[str, str, str, str | None]) -> tuple[str, str]:
    group_id, artifact_id, version, classifier = dependency
    filename = f"{artifact_id}-{version}{'-' + classifier if classifier else ''}.jar"
    group_path = group_id.replace(".", "/")
    return f"{repository}/{group_path}/{artifact_id}/{version}/{filename}", filename


def install_repository_file(url: str, target: Path, label: str, announce: bool = True) -> tuple[Path, bool]:
    """
    Ensures the repository file exists at target with a verified checksum.

    Returns the resolved path and whether a download actually happened, so
    callers can report how much work an already-provisioned machine skipped.
    """
    algorithm, expected = expected_checksum(url)
    if target.is_file() and file_digest(target, algorithm) == expected:
        return target.resolve(), False

    target.parent.mkdir(parents=True, exist_ok=True)
    temporary = target.with_name(f".{target.name}.{os.getpid()}.tmp")
    download_file(url, temporary, label, show_progress=False, announce=announce)
    if file_digest(temporary, algorithm) != expected:
        temporary.unlink(missing_ok=True)
        fail(f"Checksum verification failed for {label}.", 4)
    if not replace_with_retry(temporary, target):
        # The target is locked by another process (a running shaft-mcp/IDE JVM, or antivirus) or
        # is a deliberately locally-built artifact with the same version. A usable jar is already
        # in place, so keep it and continue instead of aborting the whole install (issue #3426 A6).
        temporary.unlink(missing_ok=True)
        log(f"WARNING: kept the existing local {label} at {target} because the file is in use "
            "and could not be replaced. If shaft-mcp later fails to start, close running Java/IDE "
            "processes and re-run this installer.")
    return target.resolve(), True


def replace_with_retry(temporary: Path, target: Path, attempts: int = 4) -> bool:
    """
    Atomically replaces target with temporary, retrying transient Windows sharing violations
    (antivirus scans, indexers). Returns False only when target exists and stays locked, so the
    caller can decide to keep the existing file; a missing/unreadable target still raises.
    """
    for attempt in range(1, attempts + 1):
        try:
            os.replace(temporary, target)
            return True
        except PermissionError:
            if attempt == attempts:
                if target.is_file():
                    return False
                raise
            time.sleep(attempt * 0.5)
    return False


def install_runtime_dependencies(jar: Path, repository: str) -> list[Path]:
    maven_repository = maven_local_repository()
    dependencies = read_runtime_dependencies(jar)
    installed: list[tuple[Path, bool] | None] = [None] * len(dependencies)
    log(f"Resolving {len(dependencies)} shaft-mcp runtime dependencies into {maven_repository}...")

    def install_dependency(dependency: tuple[str, str, str, str | None]) -> tuple[Path, bool]:
        url, filename = dependency_url(repository, dependency)
        group_id, artifact_id, version, _ = dependency
        target = maven_repository / group_id.replace(".", "/") / artifact_id / version / filename
        return install_repository_file(url, target, f"{group_id}:{artifact_id}:{version}", announce=False)

    workers = min(8, max(1, len(dependencies)))
    with ThreadPoolExecutor(max_workers=workers) as executor:
        futures = {
            executor.submit(install_dependency, dependency): index
            for index, dependency in enumerate(dependencies)
        }
        completed_count = 0
        for completed in as_completed(futures):
            installed[futures[completed]] = completed.result()
            completed_count += 1
            progress_count("Runtime dependencies", completed_count, len(dependencies),
                    completed_count == len(dependencies))
    resolved = [entry[0] for entry in installed if entry is not None]
    downloaded_count = sum(1 for entry in installed if entry is not None and entry[1])
    skipped_count = len(resolved) - downloaded_count
    log(f"Runtime dependencies ready in the local Maven repository: {downloaded_count} downloaded, "
        f"{skipped_count} already up to date (skipped).")
    return resolved


def is_shaft_skills_source(path: Path) -> bool:
    return path.is_dir() and (path / SHAFT_SKILLS_ROUTER / "SKILL.md").is_file()


def is_owned_retired_shaft_skill(directory: Path, name: str) -> bool:
    """Recognize the two-file signature used by SHAFT's retired skill packages."""
    skill = directory / "SKILL.md"
    descriptor = directory / "agents" / "openai.yaml"
    if directory.is_symlink() or not (directory.is_dir() and skill.is_file() and descriptor.is_file()):
        return False
    try:
        content = skill.read_text(encoding="utf-8")
    except (OSError, UnicodeDecodeError):
        return False
    return re.search(rf"(?m)^name:\s*{re.escape(name)}\s*$", content) is not None


def remove_retired_shaft_skills(target: Path) -> None:
    """Remove only verifiably SHAFT-owned legacy skill directories from a target."""
    target = target.resolve()
    for name in RETIRED_SHAFT_SKILL_DIRECTORIES:
        candidate = target / name
        if is_owned_retired_shaft_skill(candidate, name):
            shutil.rmtree(candidate)


def shaft_skill_files(source: Path) -> tuple[str, ...]:
    """Discover the portable pack from its canonical skill directories."""
    source = source.resolve()
    skill_directories = {skill.parent for skill in source.glob("*/SKILL.md")}
    files = {path for path in source.iterdir() if path.is_file()}
    references = source / "references"
    if references.is_dir():
        files.update(path for path in references.rglob("*") if path.is_file())
    for directory in skill_directories:
        files.update(path for path in directory.rglob("*") if path.is_file())
    return tuple(sorted(path.relative_to(source).as_posix() for path in files))


def remote_shaft_skill_files() -> tuple[str, ...]:
    """Discover the current portable pack from GitHub's recursive tree."""
    ref = os.environ.get("SHAFT_MCP_INSTALLER_REF", "main").strip() or "main"
    quoted_ref = urllib.parse.quote(ref, safe="")
    url = f"https://api.github.com/repos/ShaftHQ/SHAFT_ENGINE/git/trees/{quoted_ref}?recursive=1"
    try:
        payload = json.loads(download_bytes(url).decode("utf-8"))
    except (UnicodeDecodeError, json.JSONDecodeError) as exc:
        fail(f"SHAFT skills manifest discovery returned invalid JSON: {exc}", 4)
    if payload.get("truncated"):
        fail("SHAFT skills manifest discovery was truncated by GitHub.", 4)
    tree_paths = tuple(sorted(
        entry["path"] for entry in payload.get("tree", ())
        if entry.get("type") == "blob" and entry.get("path", "").startswith("shaft-skills/")
    ))
    skill_directories = {
        path.split("/", 2)[1]
        for path in tree_paths
        if path.count("/") == 2 and path.endswith("/SKILL.md")
    }
    files = tuple(
        path.removeprefix("shaft-skills/")
        for path in tree_paths
        if (relative := path.removeprefix("shaft-skills/"))
        and ("/" not in relative
             or relative.startswith("references/")
             or relative.split("/", 1)[0] in skill_directories)
    )
    if f"{SHAFT_SKILLS_ROUTER}/SKILL.md" not in files:
        fail("SHAFT skills manifest did not contain the shaft-developer router.", 4)
    return files


def local_shaft_skills_source() -> Path | None:
    script = Path(__file__).resolve()
    for parent in script.parents:
        candidate = parent / SHAFT_SKILLS_DIRECTORY
        if is_shaft_skills_source(candidate):
            return candidate.resolve()
    return None


def copy_shaft_skills(source: Path, target: Path) -> Path:
    source = source.resolve()
    target = target.resolve()
    if source == target:
        return target
    target.mkdir(parents=True, exist_ok=True)
    for relative in shaft_skill_files(source):
        destination = target / relative
        destination.parent.mkdir(parents=True, exist_ok=True)
        shutil.copy2(source / relative, destination)
    return target


def shaft_skills_raw_file_url(relative: str) -> str:
    ref = os.environ.get("SHAFT_MCP_INSTALLER_REF", "main").strip() or "main"
    quoted_ref = urllib.parse.quote(ref, safe="/")
    quoted_relative = urllib.parse.quote(relative, safe="/")
    return f"https://raw.githubusercontent.com/ShaftHQ/SHAFT_ENGINE/{quoted_ref}/{SHAFT_SKILLS_DIRECTORY}/{quoted_relative}"


def shaft_agent_validation_raw_file_url(relative: str) -> str:
    ref = os.environ.get("SHAFT_MCP_INSTALLER_REF", "main").strip() or "main"
    quoted_ref = urllib.parse.quote(ref, safe="/")
    quoted_relative = urllib.parse.quote(relative, safe="/")
    return f"https://raw.githubusercontent.com/ShaftHQ/SHAFT_ENGINE/{quoted_ref}/{quoted_relative}"


def download_shaft_skills_files(target: Path) -> Path:
    target = target.resolve()
    target.mkdir(parents=True, exist_ok=True)
    for relative in remote_shaft_skill_files():
        destination = (target / relative).resolve()
        if target != destination and target not in destination.parents:
            fail(f"SHAFT skills manifest contains an unsafe path: {relative}", 4)
        download_file(
            shaft_skills_raw_file_url(relative),
            destination,
            f"SHAFT skills {relative}",
            show_progress=False,
        )
    if not is_shaft_skills_source(target):
        fail("SHAFT skills package did not contain the expected skill files.", 4)
    return target


def has_agent_guidance_scaffold(target: Path) -> bool:
    """Whether the target project already carries the AGENTS.md guidance
    scaffold this validator suite is designed to check. The suite validates
    project-specific conventions (README.md content, host-context files,
    guidance file budgets, etc.) that only exist in a project that has
    deliberately adopted this scaffold -- installing it into an unrelated
    project makes the onboarding-referenced validator crash on missing files
    it has no reason to expect."""
    return (target / AGENT_GUIDANCE_SCAFFOLD_MARKER).is_file()


def download_agent_validation_script_files(target: Path) -> Path:
    target = target.resolve()
    target.mkdir(parents=True, exist_ok=True)
    for relative in AGENT_VALIDATION_SCRIPT_FILES:
        destination = (target / relative).resolve()
        if target != destination and target not in destination.parents:
            fail(f"Agent validation script manifest contains an unsafe path: {relative}", 4)
        download_file(
            shaft_agent_validation_raw_file_url(relative),
            destination,
            f"Agent validation script {relative.split('/')[-1]}",
            show_progress=False,
        )
    for relative in RETIRED_AGENT_VALIDATION_SCRIPT_FILES:
        retired = (target / relative).resolve()
        if target != retired and target not in retired.parents:
            fail(f"Retired agent validation path escapes target: {relative}", 4)
        retired.unlink(missing_ok=True)
    # Verify the main entry point was downloaded
    main_script = target / "scripts" / "ci" / "validate_agent_setup.py"
    if not main_script.is_file():
        fail("Agent validation script did not download correctly.", 4)
    return target / "scripts" / "ci"


def is_link_or_junction(path: Path) -> bool:
    """Whether a path can redirect native skill installation outside the project."""
    try:
        if path.is_symlink():
            return True
        if os.name != "nt":
            return False
        attributes = getattr(os.lstat(path), "st_file_attributes", 0)
        return bool(attributes & getattr(stat, "FILE_ATTRIBUTE_REPARSE_POINT", 0x0400))
    except FileNotFoundError:
        return False
    except OSError:
        return True


def native_skill_target(current_directory: Path, directory: str) -> Path:
    """Return an unlinked native skill path without resolving any native component."""
    target = Path(os.path.abspath(current_directory))
    for component in Path(directory).parts:
        target /= component
        if is_link_or_junction(target):
            fail(f"Refusing linked native skill path: {target}", 4)
    return target


def shaft_skills_targets(current_directory: Path, client: str | None) -> list[Path]:
    directories = (SHAFT_SKILLS_NATIVE_DIRECTORIES.get(client, SHAFT_SKILLS_ALL_NATIVE_DIRECTORIES)
                   if client else SHAFT_SKILLS_ALL_NATIVE_DIRECTORIES)
    return [native_skill_target(current_directory, directory) for directory in directories]


def install_shaft_skills(current_directory: Path, root: Path, client: str | None = None) -> list[Path]:
    targets = shaft_skills_targets(current_directory, client)
    for target in targets:
        remove_retired_shaft_skills(target)
    source = local_shaft_skills_source()
    if source is not None:
        return [copy_shaft_skills(source, target) for target in targets]
    downloaded = download_shaft_skills_files(targets[0])
    return [downloaded, *(copy_shaft_skills(downloaded, target) for target in targets[1:])]


def should_install_shaft_skills(args: argparse.Namespace, current_directory: Path) -> bool:
    return args.install_shaft_skills


def java_argfile_quote(value: str) -> str:
    return '"' + value.replace("\\", "/").replace('"', '\\"') + '"'


def write_launcher_args(jar: Path, dependencies: list[Path]) -> Path:
    args_file = jar.parent / "shaft-mcp.args"
    runtime_root = application_data_root() / "work"
    runtime_root.mkdir(parents=True, exist_ok=True)
    classpath = os.pathsep.join(str(path.resolve()) for path in [jar, *dependencies])
    # Never pin -Duser.dir or the workspace root here: agent clients (Claude Code, Codex,
    # Copilot CLI) launch stdio MCP servers from the user's open project, and that launch
    # directory must become the MCP workspace so generated tests land in the real project.
    # The fallback property only kicks in when the launch directory is unusable (protected
    # system paths, the bare user home) - e.g. Claude Desktop.
    content = "\n".join((
        java_argfile_quote(f"-D{FALLBACK_WORKSPACE_SYSTEM_PROPERTY}={runtime_root}"),
        "-cp",
        java_argfile_quote(classpath),
        MAIN_CLASS,
    )) + "\n"
    temporary = args_file.with_name(f".{args_file.name}.{os.getpid()}.tmp")
    temporary.write_text(content, encoding="utf-8", newline="\n")
    os.replace(temporary, args_file)
    return args_file.resolve()


def write_text_with_replace(path: Path, content: str) -> None:
    temporary = path.with_name(f".{path.name}.{os.getpid()}.tmp")
    temporary.write_text(content, encoding="utf-8", newline="\n")
    os.replace(temporary, path)


def write_shaft_cli_launcher(java: Path, jar: Path) -> Path:
    """
    Writes shaft-cli.args (a Java @argfile, resolved the same way agent clients resolve
    shaft-mcp.args) plus a directly runnable wrapper script next to the jar, so both a
    future programmatic launcher and a human at a terminal have a stable entry point.
    """
    args_file = jar.parent / "shaft-cli.args"
    args_content = "\n".join(("-jar", java_argfile_quote(str(jar.resolve())))) + "\n"
    write_text_with_replace(args_file, args_content)

    java_path = str(java.resolve())
    args_path = str(args_file.resolve())
    if system_name() == "Windows":
        wrapper = jar.parent / "shaft-cli.cmd"
        wrapper_content = f'@echo off\r\n"{java_path}" @"{args_path}" %*\r\n'
    else:
        wrapper = jar.parent / "shaft-cli"
        wrapper_content = f'#!/usr/bin/env sh\nexec "{java_path}" @"{args_path}" "$@"\n'
    write_text_with_replace(wrapper, wrapper_content)
    if system_name() != "Windows":
        wrapper.chmod(wrapper.stat().st_mode | 0o111)
    return wrapper.resolve()


def read_lines(stream: Any, target: queue.Queue[str], sink: list[str] | None = None) -> None:
    try:
        for line in iter(stream.readline, ""):
            text = line.rstrip("\r\n")
            if sink is not None:
                sink.append(text)
            else:
                target.put(text)
    finally:
        try:
            stream.close()
        except Exception:
            debug("ignored close of dead probe pipe")


def await_probe_response(lines: queue.Queue[str], process: subprocess.Popen[str], request_id: int, stderr: list[str]) -> dict[str, Any]:
    deadline = time.monotonic() + 45
    while time.monotonic() < deadline:
        try:
            line = lines.get(timeout=0.1)
        except queue.Empty:
            if process.poll() is not None:
                detail = "\n".join(stderr[-20:]).strip()
                suffix = f" stderr:\n{detail}" if detail else ""
                fail(f"shaft-mcp exited before completing the installer probe.{suffix}", 4)
            continue
        if not line.strip():
            continue
        try:
            response = json.loads(line)
        except json.JSONDecodeError:
            debug(f"Ignoring non-JSON probe stdout: {line}")
            continue
        if response.get("id") == request_id:
            if "error" in response:
                fail(f"shaft-mcp installer probe failed: {json.dumps(response['error'], separators=(',', ':'))}", 4)
            return response
    fail("Timed out while probing the installed shaft-mcp launcher.", 4)


def probe_stdio(java: Path, args_file: Path) -> None:
    process = subprocess.Popen(  # nosec B603 - java and args file are installer-resolved
        [str(java), f"@{args_file}"],
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
        encoding="utf-8",
        errors="replace",
        bufsize=1,
    )
    lines: queue.Queue[str] = queue.Queue()
    stderr: list[str] = []
    if process.stdout is None or process.stderr is None or process.stdin is None:
        fail("shaft-mcp probe process did not expose stdio pipes.", 4)
    stdout_thread = threading.Thread(target=read_lines, args=(process.stdout, lines), daemon=True)
    stderr_thread = threading.Thread(target=read_lines, args=(process.stderr, queue.Queue(), stderr), daemon=True)
    stdout_thread.start()
    stderr_thread.start()
    try:
        initialize = {
            "jsonrpc": "2.0",
            "id": 1,
            "method": "initialize",
            "params": {
                "protocolVersion": "2025-03-26",
                "capabilities": {},
                "clientInfo": {"name": "shaft-mcp-installer", "version": "1.0"},
            },
        }
        process.stdin.write(json.dumps(initialize, separators=(",", ":")) + "\n")
        process.stdin.flush()
        response = await_probe_response(lines, process, 1, stderr)
        server_info = response.get("result", {}).get("serverInfo", {})
        if server_info.get("name") != SERVER_NAME:
            fail("Installed JAR returned an unexpected MCP server identity.", 4)

        process.stdin.write('{"jsonrpc":"2.0","method":"notifications/initialized","params":{}}\n')
        process.stdin.write('{"jsonrpc":"2.0","id":2,"method":"tools/list","params":{}}\n')
        process.stdin.flush()
        tools_response = await_probe_response(lines, process, 2, stderr)
        tools = tools_response.get("result", {}).get("tools")
        if not isinstance(tools, list) or not tools:
            fail("Installed JAR returned no MCP tools.", 4)
    finally:
        if process.poll() is None:
            process.kill()
            try:
                process.wait(timeout=5)
            except subprocess.TimeoutExpired:
                debug("killed process may already be gone")


def command_path(name: str) -> Path | None:
    found = shutil.which(name)
    return Path(found).resolve() if found else None


def require_command(name: str, display_name: str) -> Path:
    found = command_path(name)
    if not found:
        fail(f"{display_name} is not installed or its command is unavailable.", 3)
    return found


def run_checked(command: list[str], message: str, allow_failure: bool = False) -> subprocess.CompletedProcess[str]:
    try:
        result = subprocess.run(  # nosec B603 - command is a list of installer-resolved binaries
            command, text=True, capture_output=True, timeout=60)
    except (OSError, subprocess.SubprocessError) as exc:
        if allow_failure:
            return subprocess.CompletedProcess(command, 1, "", str(exc))
        fail(f"{message} {exc}", 5)
    if result.returncode != 0 and not allow_failure:
        detail = (result.stdout + result.stderr).strip()
        fail(f"{message} {detail}".strip(), 5)
    return result


def configuration_path(client: str) -> Path:
    name = system_name()
    user_home = home()
    if client == "codex":
        return Path(os.environ.get("CODEX_HOME") or user_home / ".codex") / "config.toml"
    if client == "claude":
        return user_home / ".claude.json"
    if client == "claude-desktop":
        if name == "Windows":
            appdata = os.environ.get("APPDATA")
            if not appdata:
                fail("APPDATA is unavailable.", 3)
            return Path(appdata) / "Claude" / "claude_desktop_config.json"
        if name == "Darwin":
            return user_home / "Library" / "Application Support" / "Claude" / "claude_desktop_config.json"
        fail("Claude Desktop automatic configuration is supported on Windows and macOS.", 3)
    if client == "copilot":
        return Path(os.environ.get("COPILOT_HOME") or user_home / ".copilot") / "mcp-config.json"
    if client == "copilot-intellij":
        if name == "Windows":
            local_app_data = os.environ.get("LOCALAPPDATA")
            if not local_app_data:
                fail("LOCALAPPDATA is unavailable.", 3)
            return Path(local_app_data) / "github-copilot" / "intellij" / "mcp.json"
        return Path(os.environ.get("XDG_CONFIG_HOME") or user_home / ".config") / "github-copilot" / "intellij" / "mcp.json"
    if client == "grok":
        return Path(os.environ.get("GROK_HOME") or user_home / ".grok") / "config.toml"
    if client == "antigravity":
        return user_home / ".gemini" / "config" / "mcp_config.json"
    fail(f"Unsupported client: {client}", 2)


def read_json_object(path: Path) -> dict[str, Any]:
    if not path.exists():
        return {}
    raw = path.read_text(encoding="utf-8")
    if not raw.strip():
        return {}
    value = json.loads(raw)
    if not isinstance(value, dict):
        fail(f"Configuration root must be a JSON object: {path}", 5)
    return value


def write_json_atomically(path: Path, value: dict[str, Any]) -> None:
    path = path.resolve()
    path.parent.mkdir(parents=True, exist_ok=True)
    fd, temporary_name = tempfile.mkstemp(prefix=f".{path.name}.", suffix=".tmp", dir=path.parent)
    temporary = Path(temporary_name)
    try:
        with os.fdopen(fd, "w", encoding="utf-8", newline="\n") as output:
            json.dump(value, output, indent=2)
            output.write("\n")
        read_json_object(temporary)
        os.replace(temporary, path)
    finally:
        temporary.unlink(missing_ok=True)


def write_text_atomically(path: Path, text: str) -> None:
    path = path.resolve()
    path.parent.mkdir(parents=True, exist_ok=True)
    fd, temporary_name = tempfile.mkstemp(prefix=f".{path.name}.", suffix=".tmp", dir=path.parent)
    temporary = Path(temporary_name)
    try:
        with os.fdopen(fd, "w", encoding="utf-8", newline="\n") as output:
            output.write(text)
        os.replace(temporary, path)
    finally:
        temporary.unlink(missing_ok=True)


def stdio_entry(java: Path, args_file: Path, copilot: bool = False) -> dict[str, Any]:
    entry: dict[str, Any] = {"command": str(java), "args": [f"@{args_file}"]}
    if copilot:
        entry["type"] = "local"
        entry["tools"] = ["*"]
    return entry


def verify_json_entry(path: Path, root_property: str, java: Path, args_file: Path) -> None:
    root = read_json_object(path)
    servers = root.get(root_property)
    if not isinstance(servers, dict) or SERVER_NAME not in servers:
        fail(f"The resulting configuration does not contain {SERVER_NAME}.", 5)
    entry = servers[SERVER_NAME]
    if not isinstance(entry, dict):
        fail("The resulting shaft-mcp entry is not an object.", 5)
    if entry.get("command") != str(java):
        fail("The resulting shaft-mcp Java command is incorrect.", 5)
    if entry.get("args") != [f"@{args_file}"]:
        fail("The resulting shaft-mcp launcher arguments are incorrect.", 5)


def update_json_configuration(path: Path, mutation: Any, verification: Any) -> None:
    path = path.resolve()
    path.parent.mkdir(parents=True, exist_ok=True)
    backup = path.parent / f".{path.name}.{os.getpid()}.shaft-mcp-backup"
    existed = path.exists()
    if existed:
        shutil.copyfile(path, backup)
    try:
        mutation()
        verification()
        backup.unlink(missing_ok=True)
    except Exception as exc:
        if existed:
            shutil.copyfile(backup, path)
        else:
            path.unlink(missing_ok=True)
        backup.unlink(missing_ok=True)
        if isinstance(exc, InstallError):
            fail(f"Configuration update failed and was rolled back. {exc}", exc.code)
        fail(f"Configuration update failed and was rolled back. {exc}", 5)


def project_entry_exists(path: Path, client: str) -> bool:
    if not path.is_file():
        return False
    if client == "grok":
        try:
            raw = path.read_text(encoding="utf-8")
            parsed = tomllib.loads(raw) if raw.strip() else {}
        except (OSError, tomllib.TOMLDecodeError):
            return False
        servers = parsed.get("mcp_servers") if isinstance(parsed, dict) else None
        return isinstance(servers, dict) and SERVER_NAME in servers
    if client == "codex":
        text = path.read_text(encoding="utf-8", errors="replace")
        return bool(re.search(r'(?m)^\s*(?:\[\s*mcp_servers\.(?:"shaft-mcp"|shaft-mcp)\s*]|mcp_servers\.(?:"shaft-mcp"|shaft-mcp)\s*=)', text))
    try:
        root = read_json_object(path)
    except json.JSONDecodeError:
        fail(f"Project MCP configuration is malformed: {path}", 5)
    for property_name in ("mcpServers", "servers"):
        node = root.get(property_name)
        if isinstance(node, dict) and SERVER_NAME in node:
            return True
    return False


def project_candidates(directory: Path, client: str) -> list[Path]:
    if client == "codex":
        return [directory / ".codex" / "config.toml"]
    if client == "grok":
        return [directory / ".grok" / "config.toml"]
    if client == "antigravity":
        return [directory / ".agents" / "mcp_config.json"]
    if client in {"claude", "claude-desktop"}:
        return [directory / ".mcp.json"]
    if client == "copilot":
        return [directory / ".github" / "mcp.json", directory / ".mcp.json"]
    if client == "copilot-intellij":
        return [
            directory / ".github" / "copilot" / "mcp.json",
            directory / ".github" / "mcp.json",
            directory / ".mcp.json",
        ]
    return []


def detect_project_override(client: str) -> None:
    if client == "grok":
        path = grok_write_path()
        if path != configuration_path("grok").resolve() and project_entry_exists(path, "grok"):
            log(f"Existing project configuration at {path} defines {SERVER_NAME}; it will be updated in-place.")
        return
    user_home = home().resolve()
    directory = Path.cwd().resolve()
    while True:
        for candidate in project_candidates(directory, client):
            if project_entry_exists(candidate, client):
                log(f"Existing project configuration at {candidate} defines {SERVER_NAME}; it will be updated in-place.")
        if directory == user_home or directory.parent == directory:
            break
        directory = directory.parent


def configure_codex(java: Path, args_file: Path) -> None:
    codex = require_command("codex", "Codex")
    run_checked([str(codex), "mcp", "remove", SERVER_NAME], "Codex could not remove the previous shaft-mcp entry.", True)
    run_checked([str(codex), "mcp", "add", SERVER_NAME, "--", str(java), f"@{args_file}"], "Codex MCP configuration command failed.")
    result = run_checked([str(codex), "mcp", "get", SERVER_NAME, "--json"], "Codex could not verify the shaft-mcp entry.")
    try:
        entry = json.loads(result.stdout).get("transport", {})
    except json.JSONDecodeError as exc:
        fail(f"Codex verification returned malformed JSON: {exc}", 5)
    if entry.get("command") != str(java) or entry.get("args") != [f"@{args_file}"]:
        fail("Codex verification returned an unexpected shaft-mcp command.", 5)
    ensure_codex_auto_approval(configuration_path("codex"))


def ensure_codex_auto_approval(path: Path) -> None:
    text = path.read_text(encoding="utf-8")
    header = re.search(r'(?m)^\s*\[\s*mcp_servers\.(?:"shaft-mcp"|shaft-mcp)\s*]\s*\r?\n?', text)
    if not header:
        fail(f"Codex configuration does not contain {SERVER_NAME}: {path}", 5)
    next_header = re.search(r"(?m)^\s*\[", text[header.end():])
    section_end = header.end() + next_header.start() if next_header else len(text)
    section = text[header.end():section_end]
    setting = 'default_tools_approval_mode = "auto"'
    pattern = re.compile(r"(?m)^\s*default_tools_approval_mode\s*=.*$")
    if pattern.search(section):
        section = pattern.sub(setting, section, count=1)
    else:
        section = setting + "\n" + section
    write_text_atomically(path, text[:header.end()] + section + text[section_end:])


def configure_claude_code(java: Path, args_file: Path) -> None:
    claude = require_command("claude", "Claude Code")
    configuration = configuration_path("claude")
    try:
        root = read_json_object(configuration)
    except json.JSONDecodeError:
        fail("Claude Code configuration is malformed.", 5)
    existing = isinstance(root.get("mcpServers"), dict) and SERVER_NAME in root["mcpServers"]
    if existing:
        run_checked([str(claude), "mcp", "remove", SERVER_NAME, "-s", "user"],
                    "Claude Code could not remove the previous shaft-mcp entry.", True)
    run_checked([str(claude), "mcp", "add", "-s", "user", SERVER_NAME, "--", str(java), f"@{args_file}"], "Claude Code MCP configuration command failed.")
    verify_json_entry(configuration, "mcpServers", java, args_file)


def configure_claude_desktop(java: Path, args_file: Path) -> None:
    configuration = configuration_path("claude-desktop")

    def mutate() -> None:
        root = read_json_object(configuration)
        servers = root.setdefault("mcpServers", {})
        if not isinstance(servers, dict):
            fail("Configuration property must be an object: mcpServers", 5)
        servers[SERVER_NAME] = stdio_entry(java, args_file)
        write_json_atomically(configuration, root)

    update_json_configuration(configuration, mutate, lambda: verify_json_entry(configuration, "mcpServers", java, args_file))


def configure_copilot(java: Path, args_file: Path) -> None:
    copilot = require_command("copilot", "GitHub Copilot CLI")
    run_checked([str(copilot), "--version"], "GitHub Copilot CLI is not available.")
    configuration = configuration_path("copilot")

    def mutate() -> None:
        root = read_json_object(configuration)
        servers = root.setdefault("mcpServers", {})
        if not isinstance(servers, dict):
            fail("Configuration property must be an object: mcpServers", 5)
        servers[SERVER_NAME] = stdio_entry(java, args_file, copilot=True)
        write_json_atomically(configuration, root)

    update_json_configuration(configuration, mutate, lambda: verify_json_entry(configuration, "mcpServers", java, args_file))


def configure_copilot_intellij(java: Path, args_file: Path) -> None:
    configuration = configuration_path("copilot-intellij")

    def mutate() -> None:
        root = read_json_object(configuration)
        servers = root.setdefault("servers", {})
        if not isinstance(servers, dict):
            fail("Configuration property must be an object: servers", 5)
        servers[SERVER_NAME] = {"type": "stdio", "command": str(java), "args": [f"@{args_file}"]}
        write_json_atomically(configuration, root)

    update_json_configuration(configuration, mutate, lambda: verify_json_entry(configuration, "servers", java, args_file))


_GROK_SERVER_HEADER = re.compile(
    r'(?m)^\s*\[\s*mcp_servers\.(?:"shaft-mcp"|shaft-mcp)\s*]'
)


def _toml_basic_string(value: str) -> str:
    return json.dumps(value, ensure_ascii=False)


def grok_shaft_mcp_section(java: Path, args_file: Path) -> str:
    return (
        "[mcp_servers.shaft-mcp]\n"
        f"command = {_toml_basic_string(str(java))}\n"
        f"args = [{_toml_basic_string(f'@{args_file}')}]\n"
        "enabled = true\n"
    )


def read_toml_mapping(path: Path) -> dict[str, Any]:
    if not path.exists():
        return {}
    raw = path.read_text(encoding="utf-8")
    if not raw.strip():
        return {}
    try:
        value = tomllib.loads(raw)
    except tomllib.TOMLDecodeError as exc:
        fail(f"Grok configuration is malformed: {path}. {exc}", 5)
    if not isinstance(value, dict):
        fail(f"Configuration root must be a TOML table: {path}", 5)
    return value


def replace_or_append_grok_section(text: str, section: str) -> str:
    header = _GROK_SERVER_HEADER.search(text)
    if not header:
        prefix = text
        if prefix and not prefix.endswith("\n"):
            prefix += "\n"
        if prefix and not prefix.endswith("\n\n"):
            prefix += "\n"
        return prefix + section
    next_header = re.search(r"(?m)^\s*\[", text[header.end():])
    section_end = header.end() + next_header.start() if next_header else len(text)
    remainder = text[section_end:].lstrip("\n")
    return text[:header.start()] + section + (("\n" + remainder) if remainder else "")


def verify_grok_entry(path: Path, java: Path, args_file: Path) -> None:
    servers = read_toml_mapping(path).get("mcp_servers")
    if not isinstance(servers, dict) or SERVER_NAME not in servers:
        fail(f"The resulting configuration does not contain {SERVER_NAME}.", 5)
    entry = servers[SERVER_NAME]
    if not isinstance(entry, dict):
        fail("The resulting shaft-mcp entry is not an object.", 5)
    if entry.get("command") != str(java):
        fail("The resulting shaft-mcp Java command is incorrect.", 5)
    if entry.get("args") != [f"@{args_file}"]:
        fail("The resulting shaft-mcp launcher arguments are incorrect.", 5)


def git_root(start: Path) -> Path | None:
    directory = start.resolve()
    while True:
        if (directory / ".git").exists():
            return directory
        if directory.parent == directory:
            return None
        directory = directory.parent


def grok_write_path() -> Path:
    user_config = configuration_path("grok").resolve()
    directory = Path.cwd().resolve()
    stop = git_root(directory)
    while True:
        for candidate in project_candidates(directory, "grok"):
            resolved = candidate.resolve()
            if resolved == user_config:
                continue
            if project_entry_exists(candidate, "grok"):
                return resolved
        if stop is None or directory == stop or directory.parent == directory:
            break
        directory = directory.parent
    return user_config


def antigravity_write_path() -> Path:
    """Prefer a workspace mcp_config that already names shaft-mcp.

    Antigravity CLI reads ~/.gemini/config/mcp_config.json and
    <workspace>/.agents/mcp_config.json (mcpServers JSON).
    """
    user_config = configuration_path("antigravity").resolve()
    directory = Path.cwd().resolve()
    stop = git_root(directory)
    while True:
        for candidate in project_candidates(directory, "antigravity"):
            resolved = candidate.resolve()
            if resolved == user_config:
                continue
            if project_entry_exists(candidate, "antigravity"):
                return resolved
        if stop is None or directory == stop or directory.parent == directory:
            break
        directory = directory.parent
    return user_config


def configure_antigravity(java: Path, args_file: Path) -> None:
    configuration = antigravity_write_path()

    def mutate() -> None:
        root = read_json_object(configuration)
        servers = root.setdefault("mcpServers", {})
        if not isinstance(servers, dict):
            fail("Configuration property must be an object: mcpServers", 5)
        servers[SERVER_NAME] = stdio_entry(java, args_file)
        write_json_atomically(configuration, root)

    update_json_configuration(
        configuration, mutate, lambda: verify_json_entry(configuration, "mcpServers", java, args_file)
    )


def configure_grok(java: Path, args_file: Path) -> None:
    configuration = grok_write_path()
    if configuration.exists() and configuration.stat().st_size > 0:
        read_toml_mapping(configuration)

    def mutate() -> None:
        existed = configuration.exists() and configuration.stat().st_size > 0
        if existed:
            existing = configuration.read_text(encoding="utf-8")
            parsed = read_toml_mapping(configuration)
            servers = parsed.get("mcp_servers")
            if (
                isinstance(servers, dict)
                and SERVER_NAME in servers
                and not _GROK_SERVER_HEADER.search(existing)
            ):
                fail(
                    f"Grok configuration already defines {SERVER_NAME} in a form "
                    f"that cannot be updated in-place: {configuration}",
                    5,
                )
            text = replace_or_append_grok_section(existing, grok_shaft_mcp_section(java, args_file))
        else:
            text = grok_shaft_mcp_section(java, args_file)
        write_text_atomically(configuration, text if text.endswith("\n") else text + "\n")

    update_json_configuration(
        configuration, mutate, lambda: verify_grok_entry(configuration, java, args_file)
    )


def configure_client(client: str, java: Path, args_file: Path) -> None:
    if client == "intellij-plugin":
        return
    if client == "codex":
        configure_codex(java, args_file)
    elif client == "claude":
        configure_claude_code(java, args_file)
    elif client == "claude-desktop":
        configure_claude_desktop(java, args_file)
    elif client == "copilot":
        configure_copilot(java, args_file)
    elif client == "copilot-intellij":
        configure_copilot_intellij(java, args_file)
    elif client == "grok":
        configure_grok(java, args_file)
    elif client == "antigravity":
        configure_antigravity(java, args_file)
    else:
        fail(f"Unsupported client: {client}", 2)


def activation_hint(client: str) -> str:
    if client == "intellij-plugin":
        return "Return to the SHAFT IntelliJ IDEA plugin and test the connection."
    if client == "claude-desktop":
        return "Restart Claude Desktop, then open a new chat and use the shaft-mcp tools."
    if client == "copilot-intellij":
        return "Restart IntelliJ IDEA or reload Copilot Chat, then use the shaft-mcp tools."
    if client == "antigravity":
        return "Start a fresh Antigravity CLI session, then use the shaft-mcp tools."
    return "Start a fresh client session, then use the shaft-mcp tools."


def install(args: argparse.Namespace) -> None:
    banner()
    dry_run = bool(getattr(args, "dry_run", False))
    if dry_run:
        log("[dry-run] no files or receipt will be written")
    overall_phases = sum(
        (
            1 if args.install_mcp else 0,
            1 if args.install_mcp else 0,
            1 if args.install_shaft_cli else 0,
            1 if should_install_shaft_skills(args, Path.cwd().resolve()) else 0,
        )
    ) or 1
    overall_done = 0
    root = bootstrap_root()
    if not dry_run:
        root.mkdir(parents=True, exist_ok=True)
    repository = os.environ.get("SHAFT_MCP_REPOSITORY_URL", DEFAULT_REPOSITORY).rstrip("/")
    java = None
    version = None
    args_file = None
    jar = None
    shaft_cli_jar = None
    if dry_run:
        plan = {
            "brand": BRAND_NAME,
            "dryRun": True,
            "installMcp": bool(args.install_mcp),
            "client": args.client,
            "installShaftCli": bool(args.install_shaft_cli),
            "installShaftSkills": bool(getattr(args, "install_shaft_skills", False)),
            "version": args.version,
            "userGuide": GUIDE_URL,
        }
        if args.json:
            print(json.dumps(plan, separators=(",", ":")))
        else:
            print(f"{BRAND_NAME} dry-run plan:")
            for key, value in plan.items():
                print(f"  {key}: {value}")
        return
    if args.install_mcp or args.install_shaft_cli:
        java = get_java25(root)
        java_home = java_home_for(java)
        os.environ["JAVA_HOME"] = str(java_home)
        os.environ["PATH"] = f"{java.parent}{os.pathsep}{os.environ.get('PATH', '')}"

        if args.install_mcp and args.client != "intellij-plugin":
            detect_project_override(args.client)
        version = resolve_shaft_mcp_version(args.version, repository, root)
        log(f"Installing io.github.shafthq:shaft-mcp:{version}")
        overall_progress("MCP", overall_done + 1, overall_phases)
        jar = install_shaft_mcp_jar(version, repository, root)
        overall_done += 1
        overall_progress("Runtime dependencies", overall_done + 1, overall_phases)
        dependencies = install_runtime_dependencies(jar, repository)
        overall_done += 1
        args_file = write_launcher_args(jar, dependencies)

        log(f"Verifying shaft-mcp {version} over stdio...")
        probe_stdio(java, args_file)

    shaft_cli_launcher = None
    if args.install_shaft_cli:
        overall_progress("CLI", overall_done + 1, overall_phases)
        log(f"Installing io.github.shafthq:shaft-cli:{version}")
        shaft_cli_jar = install_shaft_cli_jar(version, repository, root)
        overall_done += 1
        shaft_cli_launcher = write_shaft_cli_launcher(java, shaft_cli_jar)

    host_config = None
    if args.install_mcp and args.client != "intellij-plugin":
        log(f"Configuring shaft-mcp for {args.client}...")
        host_config = host_config_path(args.client)
        configure_client(args.client, java, args_file)
    current_directory = Path.cwd().resolve()
    skills_paths = shaft_skills_targets(current_directory, args.client)
    skills_path = skills_paths[0]
    skills_installed = False
    validation_script_dir = None
    if should_install_shaft_skills(args, current_directory):
        overall_progress("Skills", overall_done + 1, overall_phases, final=True)
        log("Installing SHAFT skills to " + ", ".join(str(path) for path in skills_paths) + "...")
        skills_paths = install_shaft_skills(current_directory, root, args.client)
        skills_path = skills_paths[0]
        skills_installed = True
        if has_agent_guidance_scaffold(current_directory):
            log("Fetching agent validation script files...")
            validation_script_dir = download_agent_validation_script_files(current_directory)
        else:
            log(f"Skipped agent validation scripts: no {AGENT_GUIDANCE_SCAFFOLD_MARKER} guidance "
                f"scaffold found in {current_directory}.")
    else:
        log(f"Skipped SHAFT skills installation for {skills_path}.")
    if args.install_mcp:
        result = {
            "client": args.client,
            "server": SERVER_NAME,
            "version": version,
            "command": str(java),
            "args": [f"@{args_file}"],
            "mavenLocalRepository": str(maven_local_repository()),
            "userGuide": USER_GUIDE_URL,
            "shaftSkills": {
                "installed": skills_installed,
                "path": str(skills_path),
                "paths": [str(path) for path in skills_paths],
            },
        }
        if validation_script_dir:
            result["agentValidationScript"] = {
                "installed": True,
                "path": str(validation_script_dir),
            }
        if shaft_cli_launcher:
            result["shaftCli"] = {
                "installed": True,
                "launcher": str(shaft_cli_launcher),
            }
    else:
        components = {}
        if args.install_shaft_skills:
            components["shaftSkills"] = {
                "installed": skills_installed,
                "path": str(skills_path),
                "paths": [str(path) for path in skills_paths],
            }
        if validation_script_dir:
            components["agentValidationScript"] = {
                "installed": True,
                "path": str(validation_script_dir),
            }
        if shaft_cli_launcher:
            components["shaftCli"] = {"installed": True, "launcher": str(shaft_cli_launcher)}
        result = {"components": components}
    receipt = build_install_receipt(
        version=version,
        client=args.client,
        java=java,
        mcp_jar=jar,
        cli_jar=shaft_cli_jar,
        cli_launcher=shaft_cli_launcher,
        skills_paths=list(skills_paths) if skills_installed else [],
        args_file=args_file,
        host_config=host_config,
    )
    receipt_file = write_receipt(merge_with_previous_receipt(receipt), dry_run=False)
    clear_heal_handoff()
    result["brand"] = BRAND_NAME
    result["receipt"] = str(receipt_file)
    if args.json:
        print(json.dumps(result, separators=(",", ":")))
    else:
        if args.install_mcp:
            action = "installed and ready for" if args.client == "intellij-plugin" else "installed and configured for"
            print(f"shaft-mcp {version} is {action} {args.client}.")
        if skills_installed:
            print(f"SHAFT skills installed at {skills_path}.")
            if validation_script_dir:
                print(f"Agent validation script files installed at {validation_script_dir}.")
        else:
            print(f"SHAFT skills installation skipped for {skills_path}.")
        if shaft_cli_launcher:
            print(f"shaft-cli {version} installed; run it at {shaft_cli_launcher}")
        if args.install_mcp:
            print(activation_hint(args.client))
            print(f"User guide: {USER_GUIDE_URL}")
        print_component_table(receipt.get("components") or {})
        print(f"Receipt    {receipt_file}")
        print(f"Guide      {GUIDE_URL}")


def main(argv: list[str]) -> int:
    try:
        args = parse_args(argv)
        command = getattr(args, "command", "install")
        if command == "install":
            install(args)
            return 0
        if command == "status":
            return cmd_status(args)
        if command == "doctor":
            return cmd_doctor(args)
        if command == "repair":
            return cmd_repair(args)
        if command == "rollback":
            return cmd_rollback(args)
        if command == "uninstall":
            return cmd_uninstall(args)
        fail(f"Unknown command: {command}", 2)
        return 2
    except InstallError as exc:
        print(f"install-shaft-agentic-tools: {exc}", file=sys.stderr)
        command = argv[0] if argv and argv[0] in {"install", "repair", "rollback", "uninstall"} else "install"
        if exc.code not in {2} and "--dry-run" not in argv and command != "uninstall":
            handoff = write_heal_handoff(command, argv, exc)
            if handoff is not None:
                print(f"install-shaft-agentic-tools: heal handoff written to {handoff}", file=sys.stderr)
        return exc.code
    except KeyboardInterrupt:
        print("install-shaft-agentic-tools: interrupted", file=sys.stderr)
        return 130
    except Exception as exc:
        print(f"install-shaft-agentic-tools: {exc}", file=sys.stderr)
        if os.environ.get("SHAFT_MCP_DEBUG") == "1":
            raise
        return 1


if __name__ == "__main__":
    raise SystemExit(main(sys.argv[1:]))
