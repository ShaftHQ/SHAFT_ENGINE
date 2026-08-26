#!/usr/bin/env python3
"""Plan and verify ChaosEngine-owned runtime dependencies."""

from __future__ import annotations

import argparse
from contextlib import ExitStack, contextmanager, nullcontext
import ctypes.wintypes
import errno
import hashlib
import json
import os
import platform
import re
import secrets
import shutil
import stat
import subprocess  # nosec B404 - fixed list-form dependency commands from tracked spec.
import sys
import tarfile
import time
import urllib.request
import zipfile
from datetime import datetime, timedelta, timezone
from pathlib import Path, PurePosixPath


RECEIPT_SCHEMA = 1
RUNTIME_CONTRACT_VERSION = 2
STALE_AFTER = timedelta(hours=24)
RECEIPT_NAME = "receipt.json"
LOCK_MAGIC = b"chaos-engine-dependencies-lock-v1\n"
BUILD_MARKER_MAGIC = "chaos-engine-dependencies-build-v1\n"
BUILD_MARKER_OWNED_SUFFIX = ".owned"
POINTER_NAME = ".chaos-engine-runtime-current.json"
POINTER_REMOVING_NAME = f"{POINTER_NAME}.removing"
POINTER_SCHEMA = 1
GENERATIONS_NAME = ".chaos-engine-runtime-generations"
TRANSACTIONS_NAME = ".chaos-engine-runtime-transactions"
MAX_CONTROL_BYTES = 4 * 1024 * 1024
# Pinned uv 0.11.29 + Graphify/MemPalace ownership is ~6.74 MiB compact.
MAX_RECEIPT_BYTES = 8 * 1024 * 1024
MAX_EXECUTABLE_BYTES = 256 * 1024 * 1024
MAX_RUNTIME_ARCHIVE_BYTES = 256 * 1024 * 1024
MAX_RUNTIME_EXPANDED_BYTES = 1024 * 1024 * 1024
HEX_ID = re.compile(r"[0-9a-f]{32}")
HEX_DIGEST = re.compile(r"[0-9a-f]{64}")
REQUIRED_DISPATCHES = {
    "uv",
    "mempalace",
    "mempalace-mcp",
    "graphify",
    "memory",
    "memory-mcp",
}
WINDOWS_UV_JUNCTION_TAG = 0xA0000003
WINDOWS_UV_ALIAS = re.compile(
    r"uv-python/cpython-(?P<version>3\.\d+)-windows-(?P<arch>x86_64|aarch64)-none"
)
CANDIDATE_TRUST_BOUNDARY = (
    "A same-user trusted subprocess has ambient write authority and this stdlib "
    "controller cannot sandbox it. Held handles and no-follow operations contain "
    "concurrent path substitution by other actors; command output remains untrusted."
)
PYTHON_DISPATCH = (
    "import importlib.metadata as m,sys;"
    "e=next(e for e in m.distribution(sys.argv[1]).entry_points "
    "if e.group=='console_scripts' and e.name==sys.argv[2]);"
    "sys.argv=[sys.argv[2],*sys.argv[3:]];raise SystemExit(e.load()())"
)
SUPPORTED_PLATFORMS = (
    "windows-x64", "windows-arm64", "linux-x64", "linux-arm64",
    "macos-x64", "macos-arm64",
)
ACCOUNT_RECEIPT_SCHEMA = 2
ACCOUNT_RECEIPT_NAME = ".chaos-engine-dependencies.json"
DEPENDENCY_ACTIONS = frozenset({"reused", "installed", "upgraded", "repaired", "blocked"})
_UNSTABLE_VERSION = re.compile(
    r"(?:alpha|beta|rc|pre|preview|dev|snapshot|nightly)", re.IGNORECASE
)
_SECRET_KEY = re.compile(
    r"authorization|credential|password|private.?key|secret|token|api.?key",
    re.IGNORECASE,
)


def version_key(value: str) -> tuple[int, ...]:
    """Return a comparable stable numeric version without accepting prereleases."""
    normalized = value.strip().lstrip("v")
    if not normalized or _UNSTABLE_VERSION.search(normalized):
        raise ValueError(f"dependency version is not stable: {value}")
    release = normalized.split("+", 1)[0]
    parts = re.findall(r"\d+", release)
    if not parts:
        raise ValueError(f"dependency version is invalid: {value}")
    return tuple(int(part) for part in parts)


def latest_compatible_stable(
    candidates: list[dict[str, object]], *, minimum: str
) -> str:
    """Select newest non-yanked stable candidate satisfying the minimum version."""
    minimum_key = version_key(minimum)
    accepted: list[tuple[tuple[int, ...], str]] = []
    for candidate in candidates:
        value = candidate.get("version")
        if not isinstance(value, str) or candidate.get("yanked") is True:
            continue
        try:
            key = version_key(value)
        except ValueError:
            continue
        if key >= minimum_key:
            accepted.append((key, value.lstrip("v")))
    if not accepted:
        raise ValueError("no compatible stable dependency version is available")
    return max(accepted)[1]


def dependency_action(
    *,
    installed_version: str | None,
    resolved_version: str | None,
    healthy: bool,
    latest_version_verified: bool,
) -> str:
    """Classify one dependency without guessing when the stable channel is unavailable."""
    if installed_version is None:
        return "installed" if latest_version_verified and resolved_version else "blocked"
    if not healthy:
        return "repaired" if latest_version_verified and resolved_version else "blocked"
    if not latest_version_verified:
        return "blocked"
    if resolved_version is None:
        return "blocked"
    return (
        "reused"
        if installed_version.lstrip("v") == resolved_version.lstrip("v")
        else "upgraded"
    )


def discover_executables(names: list[str], *, which=shutil.which) -> dict[str, dict[str, str]]:
    """Resolve every required sibling independently from the invoking user's PATH."""
    result: dict[str, dict[str, str]] = {}
    for name in names:
        selected = which(name)
        if not selected:
            result[name] = {"status": "missing"}
            continue
        path = Path(selected).expanduser()
        try:
            resolved = path.resolve(strict=True)
        except OSError:
            result[name] = {"status": "invalid"}
            continue
        if any(part.startswith(".chaos-engine-runtime") for part in resolved.parts):
            result[name] = {"status": "invalid"}
            continue
        executable_ok = resolved.is_file() and (
            os.name == "nt" or os.access(resolved, os.X_OK)
        )
        result[name] = (
            {"status": "healthy", "executable": str(resolved)}
            if executable_ok
            else {"status": "invalid"}
        )
    return result


def sanitize_receipt(value: object, *, home: Path | None = None) -> object:
    """Remove credential-shaped fields and replace the account home prefix."""
    account_home = (home or Path.home()).resolve()
    if isinstance(value, dict):
        return {
            str(key): sanitize_receipt(item, home=account_home)
            for key, item in value.items()
            if not _SECRET_KEY.search(str(key))
        }
    if isinstance(value, list):
        return [sanitize_receipt(item, home=account_home) for item in value]
    if isinstance(value, tuple):
        return [sanitize_receipt(item, home=account_home) for item in value]
    if isinstance(value, str):
        rendered = value.replace(str(account_home), "<home>")
        return rendered.replace(str(account_home).replace("\\", "/"), "<home>")
    if value is None or type(value) in {bool, int, float}:
        return value
    return type(value).__name__


def account_tool_plan(
    project: Path,
    specification: dict[str, object],
    *,
    actions: dict[str, str],
    executables: dict[str, str],
) -> dict[str, list[list[str]]]:
    """Render user-scope tool commands; project initialization is a separate phase."""
    del project
    if specification.get("schemaVersion") != 3:
        raise ValueError("dependency specification schema is unsupported")
    uv = executables["uv"]
    npm = executables["npm"]

    def uv_commands(name: str, package: str) -> list[list[str]]:
        action = actions.get(name)
        if action in {"installed", "repaired"}:
            return [[uv, "tool", "install", package]]
        if action == "upgraded":
            return [[uv, "tool", "upgrade", package]]
        return []

    def npm_commands(name: str, package: str) -> list[list[str]]:
        return (
            [[npm, "install", "-g", package]]
            if actions.get(name) in {"installed", "upgraded", "repaired"}
            else []
        )

    return {
        "mempalace": uv_commands("mempalace", "mempalace"),
        "graphify": uv_commands("graphify", "graphifyy"),
        "memory": npm_commands("memory", "@aictx/memory@latest"),
        "context7": npm_commands("context7", "ctx7@latest"),
    }


def prerequisite_command_plan(
    system: str, provider: str, actions: dict[str, str], *, node_major: int = 22,
    node_version: str | None = None, python_version: str = "3.14.0",
    uv_version: str = "0.12.0",
) -> dict[str, list[list[str]]]:
    """Render dry platform prerequisite commands with tightly scoped elevation."""
    wanted = lambda name: actions.get(name) in {"installed", "upgraded", "repaired"}
    plan: dict[str, list[list[str]]] = {"uv": [], "python": [], "node": [], "java": []}
    if wanted("uv"):
        plan["uv"] = (
            [["pwsh", "-NoProfile", "-ExecutionPolicy", "ByPass", "-c", f"$ErrorActionPreference='Stop'; $env:UV_INSTALL_DIR=Join-Path $HOME '.local/bin'; $env:UV_NO_MODIFY_PATH='1'; irm https://github.com/astral-sh/uv/releases/download/{uv_version}/uv-installer.ps1 | iex"]]
            if system == "windows"
            else [["bash", "-o", "pipefail", "-c", f"curl -fsSL https://github.com/astral-sh/uv/releases/download/{uv_version}/uv-installer.sh | env UV_INSTALL_DIR=\"$HOME/.local/bin\" UV_NO_MODIFY_PATH=1 sh"]]
        )
    if wanted("python"):
        plan["python"] = [["uv", "python", "install", python_version, "--no-progress"]]
    return plan


def _read_json_url(url: str, *, opener=urllib.request.urlopen) -> object:
    headers = {"Accept": "application/json", "User-Agent": "ChaosEngine-installer"}
    github_token = os.environ.get("GITHUB_TOKEN")
    if github_token and url.startswith("https://api.github.com/"):
        headers["Authorization"] = f"Bearer {github_token}"
    request = urllib.request.Request(
        url,
        headers=headers,
    )
    with opener(request, timeout=30) as response:
        payload = response.read(MAX_CONTROL_BYTES + 1)
    if len(payload) > MAX_CONTROL_BYTES:
        raise ValueError("stable-channel response exceeds the size limit")
    return json.loads(payload)


def resolve_stable_version(
    name: str, contract: dict[str, object], *, opener=urllib.request.urlopen
) -> str:
    """Resolve one stable version from its official channel."""
    url = contract.get("stableChannel")
    minimum = contract.get("minimumVersion")
    if not isinstance(url, str) or not url.startswith("https://") or not isinstance(minimum, str):
        raise ValueError(f"dependency stable-channel contract is invalid: {name}")
    payload = _read_json_url(url, opener=opener)
    candidates: list[dict[str, object]] = []
    if name == "node":
        if not isinstance(payload, list):
            raise ValueError("Node stable-channel response is invalid")
        candidates = [
            {"version": item.get("version"), "yanked": False}
            for item in payload
            if isinstance(item, dict) and item.get("lts") not in (False, None, "")
        ]
    elif name == "python":
        if not isinstance(payload, list):
            raise ValueError("Python stable-channel response is invalid")
        for item in payload:
            release_name = item.get("name") if isinstance(item, dict) else None
            match = re.fullmatch(r"Python (3\.\d+\.\d+)", release_name or "")
            if match:
                candidates.append({
                    "version": match.group(1),
                    "yanked": bool(item.get("pre_release"))
                    or item.get("is_published") is not True,
                })
    elif name in {"mempalace", "graphify"}:
        releases = payload.get("releases") if isinstance(payload, dict) else None
        if not isinstance(releases, dict):
            raise ValueError(f"{name} stable-channel response is invalid")
        for version, files in releases.items():
            if not isinstance(version, str) or not isinstance(files, list) or not files:
                continue
            candidates.append({
                "version": version,
                "yanked": all(
                    isinstance(item, dict) and item.get("yanked") is True
                    for item in files
                ),
            })
    elif name in {"memory", "context7"}:
        version = payload.get("version") if isinstance(payload, dict) else None
        candidates = [{"version": version, "yanked": False}]
    elif name == "java":
        releases = payload.get("versions") if isinstance(payload, dict) else None
        if not isinstance(releases, list):
            raise ValueError("Java stable-channel response is invalid")
        candidates = [
            {"version": match.group(0), "yanked": False}
            for item in releases
            if isinstance(item, dict)
            and item.get("major") == 25
            and (match := re.match(r"\d+\.\d+\.\d+\+\d+", str(item.get("semver"))))
        ]
    else:
        tag = payload.get("tag_name") if isinstance(payload, dict) else None
        prerelease = payload.get("prerelease") if isinstance(payload, dict) else True
        draft = payload.get("draft") if isinstance(payload, dict) else True
        candidates = [{"version": tag, "yanked": bool(prerelease or draft)}]
    return latest_compatible_stable(candidates, minimum=minimum)


def _version_from_output(output: str) -> str | None:
    if _UNSTABLE_VERSION.search(output):
        return None
    match = re.search(r"(?<!\d)(\d+(?:\.\d+){1,3}(?:[+._-]\d+)?)", output)
    return match.group(1).replace("_", ".") if match else None


def probe_account_dependency(
    name: str,
    executable_path: str,
    contract: dict[str, object],
    *,
    runner=subprocess.run,
) -> dict[str, object]:
    """Run bounded version and functional probes for one account executable."""
    probe = contract.get("probe")
    if not isinstance(probe, list) or not all(isinstance(item, str) for item in probe):
        raise ValueError(f"dependency probe contract is invalid: {name}")
    command = [executable_path, *probe[1:]]
    environment = {
        key: value
        for key, value in os.environ.items()
        if key.upper() not in {"AUTHORIZATION", "TOKEN", "API_KEY"}
        and not _SECRET_KEY.search(key)
    }
    environment["PATH"] = _account_search_path()
    try:
        result = runner(
            command,
            capture_output=True,
            text=True,
            check=False,
            timeout=30,
            env=environment,
        )
    except (OSError, subprocess.SubprocessError) as error:
        return {"healthy": False, "version": None, "detail": type(error).__name__}
    output = f"{result.stdout or ''}\n{result.stderr or ''}".strip()
    version = _version_from_output(output)
    if name == "java":
        build = re.search(r"(?<!\d)(\d+\.\d+\.\d+\+\d+)", output)
        if build:
            version = build.group(1)
    return {
        "healthy": result.returncode == 0,
        "version": version,
        "detail": "passed" if result.returncode == 0 else f"exit-{result.returncode}",
    }


def _account_search_path() -> str:
    managed_node = Path.home() / ".local/share/chaos-engine/node"
    node_bins = sorted(
        (
            root if os.name == "nt" else root / "bin"
            for root in managed_node.glob("*")
            if root.is_dir()
        ),
        key=lambda path: tuple(
            int(part)
            for part in re.findall(
                r"\d+", path.name if os.name == "nt" else path.parent.name
            )
        ),
        reverse=True,
    )
    candidates = [
        *node_bins,
        Path.home() / ".local/bin",
        Path.home() / ".cargo/bin",
    ]
    current = os.environ.get("PATH", "")
    return os.pathsep.join([*(str(path) for path in candidates), current])


def discover_account_commands(
    specification: dict[str, object], *, preferred_commands: dict[str, str] | None = None,
    which=shutil.which, runner=subprocess.run
) -> tuple[dict[str, dict[str, object]], dict[str, str]]:
    """Discover and probe every dependency, including required sibling commands."""
    contracts = specification.get("dependencies")
    if specification.get("schemaVersion") != 3 or not isinstance(contracts, dict):
        raise ValueError("dependency specification schema is unsupported")
    commands: dict[str, str] = {}
    components: dict[str, dict[str, object]] = {}
    search_path = _account_search_path()
    for name, value in contracts.items():
        if not isinstance(value, dict):
            raise ValueError(f"dependency contract is invalid: {name}")
        names = value.get("executables")
        if not isinstance(names, list) or not all(isinstance(item, str) for item in names):
            raise ValueError(f"dependency executable contract is invalid: {name}")
        sibling_paths: dict[str, str] = {}
        for command in names:
            selected = (preferred_commands or {}).get(command)
            if not selected or not Path(selected).is_file():
                selected = which(command, path=search_path)
            if selected:
                resolved = Path(selected).resolve()
                if any(part.startswith(".chaos-engine-runtime") for part in resolved.parts):
                    continue
                sibling_paths[command] = str(resolved)
                commands[command] = sibling_paths[command]
        primary = names[0] if names else None
        if primary is None:
            components[name] = {"status": "not-applicable", "siblings": sibling_paths}
            continue
        if len(sibling_paths) != len(names):
            components[name] = {
                "status": "missing",
                "healthy": False,
                "version": None,
                "siblings": sibling_paths,
            }
            continue
        probed = probe_account_dependency(
            name, sibling_paths[primary], value, runner=runner
        )
        components[name] = {
            "status": "healthy" if probed["healthy"] else "broken",
            **probed,
            "executable": sibling_paths[primary],
            "siblings": sibling_paths,
        }
    return components, commands


def resolve_account_actions(
    specification: dict[str, object],
    local: dict[str, dict[str, object]],
    *,
    opener=urllib.request.urlopen,
) -> dict[str, dict[str, object]]:
    """Combine local health with independently attempted stable-channel resolution."""
    contracts = specification["dependencies"]
    resolved: dict[str, dict[str, object]] = {}
    for name, contract in contracts.items():  # type: ignore[union-attr]
        if name == "maven-tools-mcp":
            continue
        record = local.get(name, {})
        latest = None
        verified = False
        lookup_error = None
        try:
            latest = resolve_stable_version(name, contract, opener=opener)
            verified = True
        except (OSError, ValueError, json.JSONDecodeError) as error:
            lookup_error = type(error).__name__
        installed = record.get("version") if isinstance(record.get("version"), str) else None
        healthy = record.get("healthy") is True
        action = dependency_action(
            installed_version=installed,
            resolved_version=latest,
            healthy=healthy,
            latest_version_verified=verified,
        )
        resolved[name] = {
            **record,
            "provider": contract.get("provider"),
            "source": contract.get("stableChannel"),
            "installedVersion": installed,
            "resolvedVersion": latest,
            "latestVersionVerified": verified,
            "action": action,
            "probe": record.get("detail", "not-run"),
            **({"lookupError": lookup_error} if lookup_error else {}),
        }
    return resolved


def read_account_receipt(project: Path) -> dict[str, object]:
    path = project.resolve() / ACCOUNT_RECEIPT_NAME
    try:
        receipt = json.loads(path.read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError) as error:
        raise ValueError("account dependency receipt is missing or invalid") from error
    if not isinstance(receipt, dict):
        raise ValueError("account dependency receipt schema is unsupported")
    if receipt.get("schemaVersion") == 1:
        receipt = {
            **receipt,
            "schemaVersion": ACCOUNT_RECEIPT_SCHEMA,
            "scope": "user",
            "migration": "migrated-v1",
        }
    if (
        receipt.get("schemaVersion") != ACCOUNT_RECEIPT_SCHEMA
        or not isinstance(receipt.get("components"), dict)
        or not isinstance(receipt.get("commands"), dict)
    ):
        raise ValueError("account dependency receipt schema is unsupported")
    return receipt


def write_account_receipt(
    project: Path,
    components: dict[str, dict[str, object]],
    commands: dict[str, str],
    *,
    now: datetime | None = None,
) -> dict[str, object]:
    """Publish a secret-free account receipt without claiming global package ownership."""
    receipt = sanitize_receipt({
        "schemaVersion": ACCOUNT_RECEIPT_SCHEMA,
        "checkedAt": (now or datetime.now(timezone.utc)).isoformat(),
        "scope": "user",
        "components": components,
        "commands": commands,
    })
    if not isinstance(receipt, dict):
        raise ValueError("account dependency receipt is invalid")
    receipt["commands"] = {
        name: str(Path(command).resolve()) for name, command in commands.items()
    }
    path = project.resolve() / ACCOUNT_RECEIPT_NAME
    scratch = path.with_name(f".{path.name}.{secrets.token_hex(8)}.tmp")
    scratch.write_text(json.dumps(receipt, indent=2, sort_keys=True) + "\n", encoding="utf-8")
    scratch.replace(path)
    return receipt


def ensure_mempalace_config(project: Path) -> None:
    """Create the minimal validated config without invoking interactive setup."""
    path = project / "mempalace.yaml"
    if path.is_file():
        return
    wing = re.sub(r"[^a-z0-9]+", "_", project.name.casefold()).strip("_") or "project"
    path.write_text(
        f"wing: {wing}_main\n"
        "rooms:\n  - name: general\n"
        "    description: Project source and documentation\n"
        "    keywords: [project, source, documentation]\n"
        "exclude_patterns:\n  - mempalace.yaml\n  - .memory/**\n"
        "  - graphify-out/**\n  - .chaos-engine-runtime/**\n"
        "  - .chaos-engine-state/**\n",
        encoding="utf-8",
    )


def shared_state_project(project: Path) -> Path:
    """Resolve the primary checkout that owns persistent worktree state."""
    if not (project / ".git").exists():
        return project.resolve()
    git = shutil.which("git")
    if git is None:
        return project.resolve()
    completed = subprocess.run(  # nosec B603 - fixed Git introspection.
        [git, "rev-parse", "--git-common-dir"],
        cwd=project,
        capture_output=True,
        text=True,
        check=False,
    )
    if completed.returncode != 0 or not completed.stdout.strip():
        return project.resolve()
    common = Path(completed.stdout.strip())
    if not common.is_absolute():
        common = (project / common).resolve()
    return common.parent if common.name == ".git" else common / "chaos-engine-worktree-state"


def project_setup_plan(project: Path, commands: dict[str, str]) -> list[list[str]]:
    """Plan current-folder initialization without resetting existing project data."""
    project = project.resolve()
    shared = shared_state_project(project)
    planned: list[list[str]] = []
    mempalace = commands.get("mempalace")
    if mempalace:
        mempalace_command = [
            mempalace,
            "--palace",
            str(shared / ".chaos-engine-state/mempalace"),
            "--backend",
            "sqlite_exact",
        ]
        if not (shared / ".chaos-engine-state/mempalace/.mined").is_file():
            planned.append([*mempalace_command, "mine", "."])
    graphify = commands.get("graphify")
    if graphify:
        if not (project / ".agents/skills/graphify/SKILL.md").is_file():
            planned.append(
                [graphify, "install", "--platform", "agents", "--project"]
            )
        if not (shared / "graphify-out/graph.json").is_file():
            planned.append([graphify, "extract", ".", "--code-only", "--out", str(shared)])
    memory = commands.get("memory")
    if memory and not (project / ".memory/config.json").is_file():
        planned.append([memory, "init", "--no-view"])
    return planned


def detected_package_provider(system: str | None = None, *, which=shutil.which) -> str:
    selected = system or ("windows" if os.name == "nt" else "macos" if sys.platform == "darwin" else "linux")
    candidates = {
        "windows": ("winget",),
        "macos": ("brew",),
        "linux": ("apt-get", "dnf"),
    }.get(selected, ())
    for command in candidates:
        if which(command):
            return "apt" if command == "apt-get" else command
    raise RuntimeError(f"no supported {selected} package provider was detected")


def require_user_writable_npm_prefix(
    npm: str, project: Path, *, runner=subprocess.run
) -> Path:
    """Use npm's current writable prefix or configure the standard account prefix."""
    result = _run_account_command([npm, "config", "get", "prefix"], project, runner=runner)
    prefix = Path((result.stdout or "").strip()).expanduser()
    if prefix.is_dir() and os.access(prefix, os.W_OK):
        return prefix.resolve()
    prefix = Path.home() / ".local"
    prefix.mkdir(parents=True, exist_ok=True)
    _run_account_command(
        [npm, "config", "set", "prefix", str(prefix)], project, runner=runner
    )
    return prefix.resolve()


def _run_account_command(
    command: list[str], project: Path, *, runner=subprocess.run
) -> subprocess.CompletedProcess[str]:
    environment = os.environ.copy()
    environment["PATH"] = _account_search_path()
    environment["PYTHONDONTWRITEBYTECODE"] = "1"
    result = runner(
        command,
        cwd=project,
        env=environment,
        capture_output=True,
        text=True,
        check=False,
        timeout=900,
    )
    if result.returncode != 0:
        detail = (result.stderr or result.stdout or "no process output").strip()
        raise RuntimeError(
            f"dependency command failed: {Path(command[0]).name}: {detail[:500]}"
        )
    return result


def install_account_dependencies(  # noqa: MC0001 - preflight then ordered account mutation.
    project: Path,
    specification: dict[str, object],
    *,
    runner=subprocess.run,
    opener=urllib.request.urlopen,
    which=shutil.which,
    system: str | None = None,
    provider: str | None = None,
    allow_root: bool = False,
    now: datetime | None = None,
) -> dict[str, object]:
    """Install/upgrade required tools for the invoking account, then initialize cwd."""
    project = project.resolve()
    validate_runtime_specification(specification)
    if os.name != "nt" and hasattr(os, "geteuid") and os.geteuid() == 0 and not allow_root:
        raise RuntimeError("ChaosEngine installer must not run as root")
    prior_commands: dict[str, str] = {}
    try:
        prior_receipt = read_account_receipt(project)
        prior = prior_receipt.get("commands")
        if isinstance(prior, dict):
            prior_commands = {
                name: command for name, command in prior.items()
                if isinstance(name, str) and isinstance(command, str)
            }
    except ValueError:
        pass
    local, commands = discover_account_commands(
        specification, preferred_commands=prior_commands, which=which, runner=runner
    )
    actions = resolve_account_actions(
        specification, local, opener=opener
    )
    blocked = sorted(
        name for name, record in actions.items() if record.get("action") == "blocked"
    )
    if blocked:
        raise RuntimeError("dependency setup blocked: " + ", ".join(blocked))

    selected_system = system or (
        "windows" if os.name == "nt" else "macos" if sys.platform == "darwin" else "linux"
    )
    prerequisite_actions = {
        name: str(actions[name]["action"])
        for name in ("uv", "python", "node", "java")
    }
    if any(value != "reused" for value in prerequisite_actions.values()):
        selected_provider = provider or ""
        prerequisite_commands = prerequisite_command_plan(
            selected_system,
            selected_provider,
            prerequisite_actions,
            node_major=int(str(actions["node"].get("resolvedVersion") or "22").split(".", 1)[0]),
            node_version=str(actions["node"].get("resolvedVersion") or ""),
            python_version=str(actions["python"].get("resolvedVersion") or ""),
            uv_version=str(actions["uv"].get("resolvedVersion") or ""),
        )
        for name in ("uv", "python", "node", "java"):
            for command in prerequisite_commands[name]:
                if command[0] == "uv" and commands.get("uv"):
                    command = [commands["uv"], *command[1:]]
                _run_account_command(command, project, runner=runner)
            if name == "uv" and prerequisite_actions["uv"] != "reused":
                selected_uv = which("uv", path=_account_search_path())
                if not selected_uv:
                    raise RuntimeError("latest stable uv executable was not found after installation")
                commands["uv"] = str(Path(selected_uv).resolve())
        managed_python = None
        if prerequisite_actions["python"] != "reused":
            uv_command = commands.get("uv", "uv")
            found = _run_account_command(
                [uv_command, "python", "find", str(actions["python"]["resolvedVersion"])],
                project,
                runner=runner,
            )
            candidate = Path((found.stdout or "").strip()).resolve()
            if not candidate.is_file():
                raise RuntimeError("latest stable Python executable was not found after installation")
            managed_python = str(candidate)
        managed_java = None
        if prerequisite_actions["java"] != "reused":
            managed_java = install_exact_java(
                str(actions["java"].get("resolvedVersion") or ""),
                system=selected_system,
                opener=opener,
            )
        managed_node = None
        if prerequisite_actions["node"] != "reused":
            managed_node = install_exact_node(
                str(actions["node"].get("resolvedVersion") or ""),
                system=selected_system,
                opener=opener,
            )
            commands.pop("node", None)
        local, commands = discover_account_commands(
            specification, preferred_commands=commands, which=which, runner=runner
        )
        if managed_python is not None:
            python_contract = specification["dependencies"]["python"]  # type: ignore[index]
            probed = probe_account_dependency(
                "python", managed_python, python_contract, runner=runner  # type: ignore[arg-type]
            )
            local["python"] = {
                "status": "healthy" if probed["healthy"] else "broken",
                **probed,
                "executable": managed_python,
                "siblings": {"python3": managed_python},
            }
            commands["python3"] = managed_python
        if managed_java is not None:
            java_contract = specification["dependencies"]["java"]  # type: ignore[index]
            probed = probe_account_dependency(
                "java", managed_java, java_contract, runner=runner  # type: ignore[arg-type]
            )
            local["java"] = {
                "status": "healthy" if probed["healthy"] else "broken",
                **probed,
                "executable": managed_java,
                "siblings": {"java": managed_java},
            }
            commands["java"] = managed_java
        if managed_node is not None:
            node_root = Path(managed_node).parent if selected_system == "windows" else Path(managed_node).parent.parent
            managed_npm = node_root / ("npm.cmd" if selected_system == "windows" else "bin/npm")
            managed_npx = node_root / ("npx.cmd" if selected_system == "windows" else "bin/npx")
            if not managed_npm.is_file() or not managed_npx.is_file():
                raise RuntimeError("latest stable Node sibling executables are incomplete")
            node_contract = specification["dependencies"]["node"]  # type: ignore[index]
            probed = probe_account_dependency(
                "node", managed_node, node_contract, runner=runner  # type: ignore[arg-type]
            )
            local["node"] = {
                "status": "healthy" if probed["healthy"] else "broken",
                **probed,
                "executable": managed_node,
                "siblings": {
                    "node": managed_node,
                    "npm": str(managed_npm.resolve()),
                    "npx": str(managed_npx.resolve()),
                },
            }
            commands.update(
                node=managed_node,
                npm=str(managed_npm.resolve()),
                npx=str(managed_npx.resolve()),
            )
        missing = [
            name for name in ("uv", "python", "node", "java")
            if local.get(name, {}).get("healthy") is not True
        ]
        if missing:
            raise RuntimeError(
                "dependency prerequisite verification failed: " + ", ".join(missing)
            )

    tool_actions = {
        name: str(actions[name]["action"])
        for name in ("mempalace", "graphify", "memory", "context7")
    }
    if "uv" not in commands or "npm" not in commands:
        raise RuntimeError("dependency prerequisite siblings are incomplete")
    if any(value in {"installed", "upgraded", "repaired"} for value in (
        tool_actions["memory"], tool_actions["context7"]
    )):
        require_user_writable_npm_prefix(commands["npm"], project, runner=runner)
    for name, planned in account_tool_plan(
        project,
        specification,
        actions=tool_actions,
        executables={"uv": commands["uv"], "npm": commands["npm"]},
    ).items():
        for command in planned:
            _run_account_command(command, project, runner=runner)

    local, commands = discover_account_commands(
        specification, preferred_commands=commands, which=which, runner=runner
    )
    unhealthy = [
        name
        for name in ("uv", "python", "node", "java", "mempalace", "graphify", "memory", "context7")
        if local.get(name, {}).get("healthy") is not True
    ]
    if unhealthy:
        raise RuntimeError("dependency verification failed: " + ", ".join(unhealthy))
    if commands.get("mempalace"):
        ensure_mempalace_config(project)
    for command in project_setup_plan(project, commands):
        _run_account_command(command, project, runner=runner)
        if command[-2:] == ["mine", "."]:
            marker = shared_state_project(project) / ".chaos-engine-state/mempalace/.mined"
            marker.parent.mkdir(parents=True, exist_ok=True)
            marker.write_bytes(b"current\n")

    final_components: dict[str, dict[str, object]] = {}
    for name, record in actions.items():
        observed = local.get(name, {})
        final_components[name] = {
            **record,
            **observed,
            "installedVersion": observed.get("version"),
            "scope": "user",
            "action": record["action"],
            "probe": observed.get("detail", "passed"),
        }
    return write_account_receipt(
        project, final_components, commands, now=now
    )


def platform_key(*, system: str | None = None, machine: str | None = None) -> str:
    systems = {"windows": "windows", "linux": "linux", "darwin": "macos", "macos": "macos"}
    machines = {"x86_64": "x64", "amd64": "x64", "x64": "x64", "aarch64": "arm64", "arm64": "arm64"}
    system_name = systems.get((system or platform.system()).casefold())
    machine_name = machines.get((machine or platform.machine()).casefold())
    key = f"{system_name}-{machine_name}"
    if system_name is None or machine_name is None or key not in SUPPORTED_PLATFORMS:
        raise ValueError(f"unsupported platform: {system or platform.system()}/{machine or platform.machine()}")
    return key


def select_runtime_artifact(
    specification: dict[str, object], runtime: str, *, system: str | None = None,
    machine: str | None = None,
) -> dict[str, object]:
    runtimes = specification.get("runtimes")
    selected = runtimes.get(runtime) if isinstance(runtimes, dict) else None
    artifacts = selected.get("artifacts") if isinstance(selected, dict) else None
    artifact = artifacts.get(platform_key(system=system, machine=machine)) if isinstance(artifacts, dict) else None
    if not isinstance(artifact, dict):
        raise ValueError(f"runtime artifact is missing: {runtime}")
    url, digest = artifact.get("url"), artifact.get("sha256")
    if not isinstance(url, str) or not url.startswith("https://") or not isinstance(digest, str) or HEX_DIGEST.fullmatch(digest) is None:
        raise ValueError(f"runtime artifact is invalid: {runtime}")
    return dict(artifact)


def validate_runtime_specification(specification: dict[str, object]) -> None:
    if specification.get("schemaVersion") not in {2, 3}:
        raise ValueError("dependency specification schema is unsupported")
    if specification.get("schemaVersion") == 3:
        dependencies = specification.get("dependencies")
        if not isinstance(dependencies, dict) or not {
            "uv", "python", "node", "java", "mempalace", "graphify", "memory", "context7",
            "maven-tools-mcp",
        } <= set(dependencies):
            raise ValueError("account dependency specification is invalid")
        for name, contract in dependencies.items():
            if (
                not isinstance(contract, dict)
                or not isinstance(contract.get("minimumVersion"), str)
                or not isinstance(contract.get("provider"), str)
                or not isinstance(contract.get("stableChannel"), str)
                or not isinstance(contract.get("executables"), list)
                or not isinstance(contract.get("probe"), list)
            ):
                raise ValueError(f"account dependency specification is invalid: {name}")
        return
    runtimes = specification.get("runtimes")
    if not isinstance(runtimes, dict):
        raise ValueError("dependency runtime specification is invalid")
    for name in ("uv", "node", "temurin"):
        runtime = runtimes.get(name)
        artifacts = runtime.get("artifacts") if isinstance(runtime, dict) else None
        if not isinstance(artifacts, dict) or set(artifacts) != set(SUPPORTED_PLATFORMS):
            raise ValueError(f"dependency runtime artifact matrix is invalid: {name}")
        for key in SUPPORTED_PLATFORMS:
            select_runtime_artifact(
                specification, name, system=key.split("-")[0], machine=key.split("-")[1]
            )
    platform_key()


def owned_node(root: Path) -> Path:
    return root / ("node/node.exe" if os.name == "nt" else "node/bin/node")


def memory_javascript(root: Path, name: str) -> Path:
    suffix = "dist/cli/main.js" if name == "memory" else "dist/mcp/server.js"
    return root / f"npm/node_modules/@aictx/memory/{suffix}"


def require_javascript_entrypoint(path: Path, relative: str) -> None:
    posix = relative.replace("\\", "/")
    name = Path(posix).name.casefold()
    if "node_modules/.bin/" in posix or name.endswith((".cmd", ".ps1")):
        raise ValueError(f"dependency entrypoint is an npm launcher shim: {relative}")
    if not posix.endswith(".js"):
        raise ValueError(f"dependency entrypoint is not JavaScript: {relative}")
    try:
        head = path.read_bytes()[:64]
    except OSError as error:
        raise ValueError(f"dependency entrypoint is unreadable: {relative}") from error
    shebang = head.split(b"\n", 1)[0]
    if head.startswith(b"#!") and b"node" not in shebang.lower():
        raise ValueError(f"dependency entrypoint is a POSIX shim: {relative}")


def node_dispatch(generation: Path, script: Path) -> dict[str, object]:
    node = owned_node(generation)
    relative = script.relative_to(generation).as_posix()
    require_javascript_entrypoint(script, relative)
    return {
        "kind": "node",
        "executable": node.relative_to(generation).as_posix(),
        "executableSha256": sha256(node),
        "executableSize": node.stat().st_size,
        "script": relative,
        "scriptSha256": sha256(script),
        "scriptSize": script.stat().st_size,
    }


def _download_artifact(
    url: str, destination: Path, expected: str, opener=urllib.request.urlopen, *, reporter=None
) -> None:
    if reporter is not None:
        reporter.start("Provision dependencies", detail=url)
    digest = hashlib.sha256()
    total = 0
    try:
        with opener(url, timeout=60) as response, destination.open("xb") as stream:
            length = None
            headers = getattr(response, "headers", None)
            if headers is not None:
                try:
                    candidate = int(headers.get("Content-Length", ""))
                    length = (
                        candidate
                        if 0 < candidate <= MAX_RUNTIME_ARCHIVE_BYTES
                        else None
                    )
                except (TypeError, ValueError):
                    length = None
            if reporter is not None:
                reporter.begin_download(length, detail=url)
            while chunk := response.read(1024 * 1024):
                total += len(chunk)
                if total > MAX_RUNTIME_ARCHIVE_BYTES:
                    raise ValueError("runtime artifact exceeds the download limit")
                digest.update(chunk)
                stream.write(chunk)
                if reporter is not None:
                    reporter.downloaded(len(chunk))
        if digest.hexdigest() != expected:
            raise ValueError("runtime artifact checksum verification failed")
    except BaseException:
        destination.unlink(missing_ok=True)
        raise


def _safe_archive_members(archive: Path) -> list[tuple[str, bytes, int]]:
    result: list[tuple[str, bytes, int]] = []
    total = 0
    if archive.suffix == ".zip":
        with zipfile.ZipFile(archive) as bundle:
            for member in bundle.infolist():
                mode = member.external_attr >> 16
                if stat.S_ISLNK(mode):
                    raise ValueError("runtime archive contains a link")
                if member.is_dir():
                    continue
                total += member.file_size
                if total > MAX_RUNTIME_EXPANDED_BYTES:
                    raise ValueError("runtime archive expands beyond the size limit")
                result.append((member.filename, bundle.read(member), mode))
    else:
        with tarfile.open(archive, "r:*") as bundle:
            for member in bundle.getmembers():
                if member.issym() or member.islnk():
                    base = PurePosixPath(member.name).parent if member.issym() else PurePosixPath()
                    target = PurePosixPath(os.path.normpath(str(base / member.linkname)).replace("\\", "/"))
                    if target.is_absolute() or not target.parts or ".." in target.parts:
                        raise ValueError("runtime archive contains an unsafe link")
                    continue
                if not (member.isfile() or member.isdir()):
                    raise ValueError("runtime archive contains an unsafe entry")
                if member.isdir():
                    continue
                total += member.size
                if total > MAX_RUNTIME_EXPANDED_BYTES:
                    raise ValueError("runtime archive expands beyond the size limit")
                stream = bundle.extractfile(member)
                if stream is None:
                    raise ValueError("runtime archive entry is unreadable")
                result.append((member.name, stream.read(), member.mode))
    return result


def _extract_runtime_archive(archive: Path, destination: Path) -> None:
    members = _safe_archive_members(archive)
    if not members:
        raise ValueError("runtime archive is empty")
    split = [Path(name.replace("\\", "/")).parts for name, _, _ in members]
    if any(not parts or Path(*parts).is_absolute() or ".." in parts for parts in split):
        raise ValueError("runtime archive contains an unsafe path")
    strip = 1 if len({parts[0] for parts in split}) == 1 and all(len(parts) > 1 for parts in split) else 0
    destination.mkdir(parents=True)
    for (name, payload, mode), parts in zip(members, split):
        relative = Path(*parts[strip:])
        target = destination / relative
        target.parent.mkdir(parents=True, exist_ok=True)
        target.write_bytes(payload)
        if os.name != "nt" and mode & stat.S_IXUSR:
            target.chmod(target.stat().st_mode | stat.S_IXUSR)


def install_exact_node(
    version: str, *, system: str, opener=urllib.request.urlopen
) -> str:
    """Install one checksum-verified official Node release for this account."""
    machine = platform.machine().casefold()
    architecture = "arm64" if machine in {"arm64", "aarch64"} else "x64"
    platform_name = {"windows": "win", "macos": "darwin", "linux": "linux"}[system]
    suffix = ".zip" if system == "windows" else ".tar.gz" if system == "macos" else ".tar.xz"
    filename = f"node-v{version}-{platform_name}-{architecture}{suffix}"
    base = f"https://nodejs.org/download/release/v{version}"
    with opener(f"{base}/SHASUMS256.txt", timeout=30) as response:
        checksums = response.read(MAX_CONTROL_BYTES + 1)
    if len(checksums) > MAX_CONTROL_BYTES:
        raise ValueError("Node checksum manifest exceeds the size limit")
    expected = next(
        (
            line.split()[0]
            for line in checksums.decode("utf-8").splitlines()
            if line.split()[1:] == [filename]
        ),
        None,
    )
    if expected is None or re.fullmatch(r"[0-9a-f]{64}", expected) is None:
        raise ValueError("Node checksum manifest is missing the selected release")
    root = Path.home() / ".local/share/chaos-engine/node" / version
    executable = root / ("node.exe" if system == "windows" else "bin/node")
    if executable.is_file():
        _ensure_node_siblings(root, system)
        return str(executable.resolve())
    if root.exists() or is_link_or_reparse(root):
        raise ValueError("existing Node account runtime is invalid")
    parent = root.parent
    parent.mkdir(parents=True, exist_ok=True)
    transaction = parent / f".{version}.{secrets.token_hex(8)}.building"
    archive = transaction.with_suffix(suffix)
    try:
        _download_artifact(f"{base}/{filename}", archive, expected, opener)
        _extract_runtime_archive(archive, transaction)
        transaction.rename(root)
    except BaseException:
        archive.unlink(missing_ok=True)
        if transaction.exists() and not is_link_or_reparse(transaction):
            shutil.rmtree(transaction)
        raise
    finally:
        archive.unlink(missing_ok=True)
    if not executable.is_file():
        raise ValueError("installed Node account runtime is incomplete")
    _ensure_node_siblings(root, system)
    return str(executable.resolve())


def _ensure_node_siblings(root: Path, system: str) -> None:
    """Restore safe npm launchers omitted when archive links are rejected."""
    for name in ("npm", "npx"):
        launcher = root / (f"{name}.cmd" if system == "windows" else f"bin/{name}")
        if system == "windows":
            if not launcher.is_file():
                raise ValueError("installed Node account runtime is incomplete")
            continue
        script = root / "lib/node_modules/npm/bin" / f"{name}-cli.js"
        if not script.is_file():
            raise ValueError("installed Node account runtime is incomplete")
        launcher.write_text(
            '#!/bin/sh\nexec "$(dirname "$0")/node" "$(dirname "$0")/../lib/node_modules/npm/bin/'
            f'{name}-cli.js" "$@"\n',
            encoding="utf-8",
        )
        launcher.chmod(launcher.stat().st_mode | stat.S_IXUSR)


def install_exact_java(
    version: str, *, system: str, opener=urllib.request.urlopen
) -> str:
    """Install one checksum-verified official Temurin 25 release for this account."""
    match = re.fullmatch(r"(25\.\d+\.\d+)\+(\d+)", version)
    if match is None:
        raise ValueError("Temurin release version is invalid")
    feature, build = match.groups()
    machine = platform.machine().casefold()
    architecture = (
        "aarch64"
        if system != "windows" and machine in {"arm64", "aarch64"}
        else "x64"
    )
    platform_name = {"windows": "windows", "macos": "mac", "linux": "linux"}[system]
    suffix = ".zip" if system == "windows" else ".tar.gz"
    filename = (
        f"OpenJDK25U-jdk_{architecture}_{platform_name}_hotspot_"
        f"{feature}_{build}{suffix}"
    )
    tag = f"jdk-{feature}%2B{build}"
    url = (
        "https://github.com/adoptium/temurin25-binaries/releases/download/"
        f"{tag}/{filename}"
    )
    with opener(f"{url}.sha256.txt", timeout=30) as response:
        checksum = response.read(MAX_CONTROL_BYTES + 1)
    if len(checksum) > MAX_CONTROL_BYTES:
        raise ValueError("Temurin checksum manifest exceeds the size limit")
    expected = checksum.decode("utf-8").split()[0] if checksum.strip() else ""
    if re.fullmatch(r"[0-9a-f]{64}", expected) is None:
        raise ValueError("Temurin checksum manifest is invalid")
    root = Path.home() / ".local/share/chaos-engine/java" / version
    executable = root / (
        "bin/java.exe"
        if system == "windows"
        else "Contents/Home/bin/java"
        if system == "macos"
        else "bin/java"
    )
    if executable.is_file():
        return str(executable.resolve())
    if root.exists() or is_link_or_reparse(root):
        raise ValueError("existing Java account runtime is invalid")
    parent = root.parent
    parent.mkdir(parents=True, exist_ok=True)
    transaction = parent / f".{feature}-{build}.{secrets.token_hex(8)}.building"
    archive = transaction.with_suffix(suffix)
    try:
        _download_artifact(url, archive, expected, opener)
        _extract_runtime_archive(archive, transaction)
        transaction.rename(root)
    except BaseException:
        archive.unlink(missing_ok=True)
        if transaction.exists() and not is_link_or_reparse(transaction):
            shutil.rmtree(transaction)
        raise
    finally:
        archive.unlink(missing_ok=True)
    if not executable.is_file():
        raise ValueError("installed Java account runtime is incomplete")
    return str(executable.resolve())


def provision_generation_runtimes(
    generation: Path, transaction: Path, specification: dict[str, object], *,
    opener=urllib.request.urlopen, reporter=None, confirmer=None,
) -> None:
    platform_key()
    for name in ("uv", "node"):
        artifact = select_runtime_artifact(specification, name)
        suffix = ".zip" if str(artifact["url"]).endswith(".zip") else (
            ".tar.xz" if str(artifact["url"]).endswith(".tar.xz") else ".tar.gz"
        )
        archive = transaction / f"{name}{suffix}"
        if confirmer is not None:
            confirmer(f"Download {name} runtime from {artifact['url']}")
        _download_artifact(
            str(artifact["url"]), archive, str(artifact["sha256"]), opener,
            reporter=reporter,
        )
        destination = (
            generation / "bootstrap" / ("Scripts" if os.name == "nt" else "bin")
            if name == "uv" else generation / "node"
        )
        _extract_runtime_archive(archive, destination)


class DanglingRuntimeLink(ValueError):
    pass


def is_link_or_reparse(path: Path) -> bool:
    if path.is_symlink():
        return True
    try:
        attributes = getattr(os.lstat(path), "st_file_attributes", 0)
    except FileNotFoundError:
        return False
    return bool(attributes & 0x400)


def runtime_entries(runtime: Path) -> list[Path]:
    """List a runtime tree without traversing links or reparse points."""
    entries: list[Path] = []
    pending = [runtime]
    while pending:
        directory = pending.pop()
        with os.scandir(directory) as children:
            paths = sorted((Path(child.path) for child in children), reverse=True)
        for path in paths:
            entries.append(path)
            if not is_link_or_reparse(path) and path.is_dir():
                pending.append(path)
    return sorted(entries)


def executable(directory: Path, name: str) -> str:
    suffix = ".exe" if os.name == "nt" else ""
    return str(directory / f"{name}{suffix}")


def npm_executable(directory: Path, name: str) -> str:
    suffix = ".cmd" if os.name == "nt" else ""
    return str(directory / f"{name}{suffix}")


@contextmanager
def runtime_lock(runtime: Path):
    lock = runtime.with_name(f"{runtime.name}.lock")
    flags = os.O_RDWR | getattr(os, "O_BINARY", 0)
    created = False
    try:
        descriptor = os.open(lock, flags | os.O_CREAT | os.O_EXCL, 0o600)
        created = True
    except FileExistsError:
        if is_link_or_reparse(lock):
            raise ValueError(f"dependency lock is a link or reparse point: {lock}")
        descriptor = os.open(lock, flags)
    try:
        stream = os.fdopen(descriptor, "r+b", closefd=True)
    except (Exception, KeyboardInterrupt, SystemExit):
        os.close(descriptor)
        raise
    try:
        opened = os.fstat(stream.fileno())
        named = os.stat(lock, follow_symlinks=False)
        if (opened.st_dev, opened.st_ino) != (named.st_dev, named.st_ino):
            raise ValueError(f"dependency lock collision: {lock}")
        if os.name == "nt":
            import msvcrt

            msvcrt.locking(stream.fileno(), msvcrt.LK_NBLCK, 1)
        else:
            import fcntl

            fcntl.flock(stream.fileno(), fcntl.LOCK_EX | fcntl.LOCK_NB)
    except (OSError, BlockingIOError) as error:
        stream.close()
        raise RuntimeError(f"dependency runtime is locked: {runtime}") from error
    except (Exception, KeyboardInterrupt, SystemExit):
        stream.close()
        raise
    try:
        stream.seek(0)
        existing = stream.read()
        if created:
            stream.seek(0)
            stream.write(LOCK_MAGIC)
            stream.truncate()
            stream.flush()
        elif existing != LOCK_MAGIC:
            raise ValueError(f"dependency lock is not ChaosEngine-owned: {lock}")
        yield
    finally:
        try:
            if os.name == "nt":
                stream.seek(0)
                msvcrt.locking(stream.fileno(), msvcrt.LK_UNLCK, 1)
            else:
                fcntl.flock(stream.fileno(), fcntl.LOCK_UN)
        finally:
            stream.close()


def sha256(path: Path) -> str:
    return hashlib.sha256(path.read_bytes()).hexdigest()


def json_integrity(value: dict[str, object]) -> str:
    body = {
        key: item
        for key, item in value.items()
        if key not in {"integritySha256", "receiptIntegritySha256"}
    }
    encoded = json.dumps(body, sort_keys=True, separators=(",", ":")).encode()
    return hashlib.sha256(encoded).hexdigest()


def _validate_generation_record(value: object) -> dict[str, str]:
    fields = {
        "generationId",
        "specificationSha256",
        "coreSha256",
        "receiptSha256",
    }
    if not isinstance(value, dict) or set(value) != fields:
        raise ValueError("dependency generation record is invalid")
    record = {key: item for key, item in value.items() if isinstance(item, str)}
    if set(record) != fields or HEX_ID.fullmatch(record["generationId"]) is None:
        raise ValueError("dependency generation identifier is invalid")
    if any(
        HEX_DIGEST.fullmatch(record[name]) is None
        for name in fields - {"generationId"}
    ):
        raise ValueError("dependency generation digest is invalid")
    return record


def _select_previous(project: Path, active: dict[str, str]) -> dict[str, str] | None:
    pointer_path = project / POINTER_NAME
    if not (pointer_path.exists() or is_link_or_reparse(pointer_path)):
        return None
    current = _read_pointer(project)
    current_active = _validate_generation_record(current["active"])
    try:
        _validate_selected_generation(
            project,
            current_active,
            current_active["specificationSha256"],
            current_active["coreSha256"],
            verify_installed_core=False,
        )
    except (OSError, ValueError):
        current_active = active
    if current_active != active:
        return current_active
    if current.get("previous") is None:
        return None
    previous = _validate_generation_record(current["previous"])
    try:
        _validate_selected_generation(
            project,
            previous,
            previous["specificationSha256"],
            previous["coreSha256"],
            verify_installed_core=False,
        )
    except (OSError, ValueError):
        return None
    return previous


def _persist_pointer(
    project: Path, pointer: dict[str, object], transaction: str
) -> dict[str, object]:
    path = project / POINTER_NAME
    temporary = project / f"{POINTER_NAME}.tmp.{transaction}.{secrets.token_hex(8)}"
    if is_link_or_reparse(path):
        raise ValueError("dependency pointer is a link or reparse point")
    directory = None
    if os.name != "nt":
        directory = os.open(project, os.O_RDONLY | getattr(os, "O_DIRECTORY", 0))
    descriptor = os.open(
        temporary,
        os.O_WRONLY | os.O_CREAT | os.O_EXCL | getattr(os, "O_BINARY", 0),
        0o600,
    )
    try:
        with os.fdopen(descriptor, "w", encoding="utf-8") as stream:
            json.dump(pointer, stream, indent=2, sort_keys=True)
            stream.write("\n")
            stream.flush()
            os.fsync(stream.fileno())
        temporary.replace(path)
        if directory is not None:
            try:
                os.fsync(directory)
            except OSError:
                # Replacement is commit point; a durability warning cannot be rolled back.
                persisted, _ = _bounded_json(project, POINTER_NAME, "pointer")
                if persisted != pointer:
                    raise
                result = dict(pointer)
                result["publicationStatus"] = "committed-not-durable"
                return result
    finally:
        if temporary.exists() and not is_link_or_reparse(temporary):
            temporary.unlink()
        if directory is not None:
            os.close(directory)
    result = dict(pointer)
    result["publicationStatus"] = "durable"
    return result


def publish_pointer(
    project: Path,
    active: dict[str, str],
    *,
    transaction_id: str | None = None,
    expected_specification_sha256: str,
    expected_core_sha256: str,
) -> dict[str, object]:
    """Atomically select immutable generations using identifiers, never paths."""
    project = project.absolute()
    active = _validate_generation_record(active)
    if (
        active["specificationSha256"] != expected_specification_sha256
        or active["coreSha256"] != expected_core_sha256
    ):
        raise ValueError("dependency candidate does not match requested specification or core")
    _validate_selected_generation(
        project,
        active,
        expected_specification_sha256,
        expected_core_sha256,
        verify_installed_core=True,
    )
    previous = _select_previous(project, active)
    transaction = transaction_id or secrets.token_hex(16)
    if HEX_ID.fullmatch(transaction) is None:
        raise ValueError("dependency transaction identifier is invalid")
    persisted_pointer: dict[str, object] = {
        "schemaVersion": POINTER_SCHEMA,
        "transactionId": transaction,
        "active": active,
        "previous": previous,
    }
    persisted_pointer["integritySha256"] = json_integrity(persisted_pointer)
    return _persist_pointer(project, persisted_pointer, transaction)


def _relative_parts(relative: str, label: str) -> tuple[str, ...]:
    path = Path(relative)
    if (
        path.is_absolute()
        or path.anchor
        or path.drive
        or path.root
        or not path.parts
        or ".." in path.parts
    ):
        raise ValueError(f"dependency {label} path is unsafe")
    return path.parts


def _trusted_root(root: Path, label: str) -> Path:
    lexical = _lexical_path(root)
    try:
        resolved = root.resolve(strict=True)
    except OSError as error:
        raise ValueError(f"dependency {label} root is missing or unsafe") from error
    if is_link_or_reparse(lexical) or os.path.normcase(str(lexical)) != os.path.normcase(
        str(resolved)
    ):
        raise ValueError(f"dependency {label} root has an unsafe ancestor or link")
    return lexical


@contextmanager
def _hold_directory(path: Path, label: str):
    """Hold directory identity; Windows handle denies rename/delete while in use."""
    path = _trusted_root(path, label)
    if os.name == "nt":
        class FILE_ATTRIBUTE_TAG_INFO(ctypes.Structure):
            _fields_ = [
                ("FileAttributes", ctypes.wintypes.DWORD),
                ("ReparseTag", ctypes.wintypes.DWORD),
            ]

        kernel32 = ctypes.WinDLL("kernel32", use_last_error=True)
        create_file = kernel32.CreateFileW
        create_file.argtypes = [
            ctypes.wintypes.LPCWSTR,
            ctypes.wintypes.DWORD,
            ctypes.wintypes.DWORD,
            ctypes.wintypes.LPVOID,
            ctypes.wintypes.DWORD,
            ctypes.wintypes.DWORD,
            ctypes.wintypes.HANDLE,
        ]
        create_file.restype = ctypes.wintypes.HANDLE
        handle = create_file(
            str(path),
            0x80000000,
            0x00000001 | 0x00000002,
            None,
            3,
            0x00200000 | 0x02000000,
            None,
        )
        if handle == ctypes.c_void_p(-1).value:
            raise ValueError(f"dependency {label} directory cannot be held safely")
        try:
            attributes = FILE_ATTRIBUTE_TAG_INFO()
            if not kernel32.GetFileInformationByHandleEx(
                handle, 9, ctypes.byref(attributes), ctypes.sizeof(attributes)
            ) or attributes.FileAttributes & 0x400:
                raise ValueError(f"dependency {label} directory is a reparse point")
            value = os.stat(path, follow_symlinks=False)
            yield (None, (value.st_dev, value.st_ino))
        finally:
            kernel32.CloseHandle(handle)
        return
    descriptor = os.open(
        path,
        os.O_RDONLY | getattr(os, "O_DIRECTORY", 0) | getattr(os, "O_NOFOLLOW", 0),
    )
    try:
        value = os.fstat(descriptor)
        yield (descriptor, (value.st_dev, value.st_ino))
    finally:
        os.close(descriptor)


def _assert_held_directory(path: Path, held: tuple[object, tuple[int, int]], label: str) -> None:
    if is_link_or_reparse(path):
        raise ValueError(f"dependency {label} directory became unsafe")
    try:
        value = os.stat(path, follow_symlinks=False)
    except OSError as error:
        raise ValueError(f"dependency {label} directory identity changed") from error
    if (value.st_dev, value.st_ino) != held[1]:
        raise ValueError(f"dependency {label} directory identity changed")


def _delete_fd_contents(directory: int) -> None:
    for name in os.listdir(directory):
        value = os.stat(name, dir_fd=directory, follow_symlinks=False)
        if stat.S_ISDIR(value.st_mode):
            child = os.open(
                name,
                os.O_RDONLY
                | getattr(os, "O_DIRECTORY", 0)
                | getattr(os, "O_NOFOLLOW", 0),
                dir_fd=directory,
            )
            try:
                opened = os.fstat(child)
                if (opened.st_dev, opened.st_ino) != (value.st_dev, value.st_ino):
                    raise ValueError("dependency cleanup directory identity changed")
                _delete_fd_contents(child)
                after = os.fstat(child)
                if (after.st_dev, after.st_ino) != (opened.st_dev, opened.st_ino):
                    raise ValueError("dependency cleanup directory identity changed")
            finally:
                os.close(child)
            os.rmdir(name, dir_fd=directory)
        else:
            os.unlink(name, dir_fd=directory)


def _delete_held_child(
    parent: tuple[object, tuple[int, int]],
    name: str,
    child: tuple[object, tuple[int, int]],
    expected: tuple[int, int],
) -> None:
    parent_fd, child_identity = parent[0], child[1]
    child_fd = child[0]
    if not isinstance(parent_fd, int) or not isinstance(child_fd, int):
        raise ValueError("descriptor-relative cleanup is unavailable")
    named = os.stat(name, dir_fd=parent_fd, follow_symlinks=False)
    opened = os.fstat(child_fd)
    identities = {
        (named.st_dev, named.st_ino),
        (opened.st_dev, opened.st_ino),
        child_identity,
    }
    if identities != {expected}:
        raise ValueError("dependency cleanup identity changed; quarantine retained")
    _delete_fd_contents(child_fd)
    os.rmdir(name, dir_fd=parent_fd)


def _delete_windows_tree(
    path: Path, expected: tuple[int, int], runtime: Path | None = None
) -> None:
    runtime = runtime or path
    if is_link_or_reparse(path):
        raise ValueError("dependency cleanup path is unsafe; quarantine retained")
    value = os.stat(path, follow_symlinks=False)
    if (value.st_dev, value.st_ino) != expected:
        raise ValueError("dependency cleanup identity changed; quarantine retained")
    children = sorted(path.iterdir(), reverse=True)
    for child in children:
        if is_link_or_reparse(child):
            if child.is_symlink():
                child.unlink()
                continue
            _windows_uv_junction_record(runtime, child)
            child.rmdir()
    for child in children:
        if not child.exists() and not is_link_or_reparse(child):
            continue
        if child.is_dir():
            state = os.stat(child, follow_symlinks=False)
            _delete_windows_tree(child, (state.st_dev, state.st_ino), runtime)
        else:
            child.unlink()
    path.rmdir()


def _open_regular_relative(root: Path, relative: str, label: str) -> int:
    """Open a regular descendant without following POSIX links in any component."""
    root = _trusted_root(root, label)
    parts = _relative_parts(relative, label)
    nofollow = getattr(os, "O_NOFOLLOW", 0)
    binary = getattr(os, "O_BINARY", 0)
    if os.name != "nt" and os.open in os.supports_dir_fd:
        directory = os.open(
            root,
            os.O_RDONLY | getattr(os, "O_DIRECTORY", 0) | nofollow,
        )
        try:
            for part in parts[:-1]:
                child = os.open(
                    part,
                    os.O_RDONLY | getattr(os, "O_DIRECTORY", 0) | nofollow,
                    dir_fd=directory,
                )
                previous = directory
                directory = child
                os.close(previous)
            descriptor = os.open(
                parts[-1], os.O_RDONLY | binary | nofollow, dir_fd=directory
            )
        except OSError as error:
            if error.errno == errno.ENOENT:
                raise ValueError(f"dependency {label} is missing") from error
            raise ValueError(f"dependency {label} has an unsafe ancestor or link") from error
        finally:
            os.close(directory)
    elif os.name == "nt":
        current = root
        for part in parts:
            current /= part
            if is_link_or_reparse(current):
                raise ValueError(f"dependency {label} has an unsafe ancestor or link")
        descriptor = _open_windows_regular(root, current, label)
    else:
        raise ValueError("safe no-follow file traversal is unavailable on this platform")
    try:
        opened = os.fstat(descriptor)
        if not stat.S_ISREG(opened.st_mode):
            raise ValueError(f"dependency {label} is not a regular file")
    except (Exception, KeyboardInterrupt, SystemExit):
        os.close(descriptor)
        raise
    return descriptor


def _open_windows_regular(root: Path, path: Path, label: str) -> int:
    """Open final Windows file without following it; validate kernel-resolved location."""
    import msvcrt

    class FILE_ATTRIBUTE_TAG_INFO(ctypes.Structure):
        _fields_ = [
            ("FileAttributes", ctypes.wintypes.DWORD),
            ("ReparseTag", ctypes.wintypes.DWORD),
        ]

    kernel32 = ctypes.WinDLL("kernel32", use_last_error=True)
    create_file = kernel32.CreateFileW
    create_file.argtypes = [
        ctypes.wintypes.LPCWSTR,
        ctypes.wintypes.DWORD,
        ctypes.wintypes.DWORD,
        ctypes.wintypes.LPVOID,
        ctypes.wintypes.DWORD,
        ctypes.wintypes.DWORD,
        ctypes.wintypes.HANDLE,
    ]
    create_file.restype = ctypes.wintypes.HANDLE
    invalid = ctypes.c_void_p(-1).value
    handle = invalid
    error_number = 0
    for delay in (0.0, 0.02, 0.05, 0.1, 0.2):
        if delay:
            time.sleep(delay)
        handle = create_file(
            str(path),
            0x80000000,
            0x00000001 | 0x00000002 | 0x00000004,
            None,
            3,
            0x00200000 | 0x08000000,
            None,
        )
        if handle != invalid:
            break
        error_number = ctypes.get_last_error()
        if error_number not in {5, 32, 33}:
            break
    if handle == invalid:
        raise ValueError(
            f"dependency {label} is missing or unsafe (Windows error {error_number})"
        ) from OSError(error_number, ctypes.FormatError(error_number))
    try:
        attributes = FILE_ATTRIBUTE_TAG_INFO()
        if not kernel32.GetFileInformationByHandleEx(
            handle, 9, ctypes.byref(attributes), ctypes.sizeof(attributes)
        ):
            raise ValueError(f"dependency {label} identity cannot be verified")
        if attributes.FileAttributes & 0x400:
            raise ValueError(f"dependency {label} is a link or reparse point")
        size = kernel32.GetFinalPathNameByHandleW(handle, None, 0, 0)
        if not size:
            raise ValueError(f"dependency {label} final path cannot be verified")
        buffer = ctypes.create_unicode_buffer(size + 1)
        if not kernel32.GetFinalPathNameByHandleW(handle, buffer, len(buffer), 0):
            raise ValueError(f"dependency {label} final path cannot be verified")
        final_path = _lexical_path(Path(buffer.value))
        if not final_path.is_relative_to(root):
            raise ValueError(f"dependency {label} escaped its trusted root")
        named = os.stat(path, follow_symlinks=False)
        descriptor = msvcrt.open_osfhandle(handle, os.O_RDONLY | getattr(os, "O_BINARY", 0))
        handle = None
        opened = os.fstat(descriptor)
        if (opened.st_dev, opened.st_ino) != (named.st_dev, named.st_ino):
            os.close(descriptor)
            raise ValueError(f"dependency {label} identity changed during open")
        return descriptor
    finally:
        if handle is not None:
            kernel32.CloseHandle(handle)


def _read_regular_relative(
    root: Path, relative: str, label: str, limit: int | None = None
) -> bytes:
    descriptor = _open_regular_relative(root, relative, label)
    try:
        opened = os.fstat(descriptor)
        if limit is not None and opened.st_size > limit:
            raise ValueError(f"dependency {label} is too large")
        chunks: list[bytes] = []
        remaining = None if limit is None else limit + 1
        while remaining is None or remaining > 0:
            chunk = os.read(descriptor, 1024 * 1024 if remaining is None else min(1024 * 1024, remaining))
            if not chunk:
                break
            chunks.append(chunk)
            if remaining is not None:
                remaining -= len(chunk)
        data = b"".join(chunks)
        if limit is not None and len(data) > limit:
            raise ValueError(f"dependency {label} is too large")
        return data
    finally:
        os.close(descriptor)


def _capture_regular_relative(
    root: Path,
    relative: str,
    label: str,
    expected_size: int | None = None,
) -> tuple[str, dict[str, int]]:
    if expected_size is not None and (
        not isinstance(expected_size, int) or not 0 <= expected_size <= MAX_EXECUTABLE_BYTES
    ):
        raise ValueError(f"dependency {label} size is invalid")
    descriptor = _open_regular_relative(root, relative, label)
    digest = hashlib.sha256()
    try:
        before = os.fstat(descriptor)
        if before.st_size > MAX_EXECUTABLE_BYTES:
            raise ValueError(f"dependency {label} is too large")
        if expected_size is not None and before.st_size != expected_size:
            raise ValueError(f"dependency {label} size drift detected")
        remaining = before.st_size
        while remaining > 0:
            chunk = os.read(descriptor, min(1024 * 1024, remaining))
            if not chunk:
                break
            digest.update(chunk)
            remaining -= len(chunk)
        if os.read(descriptor, 1):
            raise ValueError(f"dependency {label} changed while hashing")
        after = os.fstat(descriptor)
        identity = lambda item: (
            item.st_dev,
            item.st_ino,
            item.st_size,
            item.st_mtime_ns,
            item.st_ctime_ns,
            item.st_mode,
        )
        if identity(before) != identity(after):
            raise ValueError(f"dependency {label} changed while hashing")
    finally:
        os.close(descriptor)
    return digest.hexdigest(), _identity_from_stat(after)


def _digest_regular_relative(
    root: Path, relative: str, label: str, expected_size: int
) -> str:
    return _capture_regular_relative(root, relative, label, expected_size)[0]


def _bounded_json(
    root: Path,
    relative: str,
    label: str,
    limit: int = MAX_CONTROL_BYTES,
) -> tuple[dict[str, object], bytes]:
    try:
        data = _read_regular_relative(root, relative, label, limit)
        value = json.loads(data)
    except (OSError, UnicodeDecodeError, json.JSONDecodeError) as error:
        raise ValueError(f"dependency {label} is missing or invalid") from error
    if not isinstance(value, dict):
        raise ValueError(f"dependency {label} is invalid")
    return value, data


def _validate_generation_receipt(receipt: dict[str, object]) -> None:
    required = {
        "schemaVersion",
        "runtimeContractVersion",
        "checkedAt",
        "specificationSha256",
        "coreSha256",
        "environment",
        "installed",
        "tools",
        "ownership",
        "receiptIntegritySha256",
    }
    modern = receipt.get("schemaVersion") == 3 and receipt.get("runtimeContractVersion") == 4
    if modern:
        required.add("runtimes")
    if (
        not (modern or (receipt.get("schemaVersion") == 2 and receipt.get("runtimeContractVersion") == 3))
        or set(receipt) != required
        or HEX_DIGEST.fullmatch(str(receipt.get("specificationSha256", ""))) is None
        or HEX_DIGEST.fullmatch(str(receipt.get("coreSha256", ""))) is None
        or not isinstance(receipt.get("environment"), dict)
        or not isinstance(receipt.get("installed"), dict)
        or not isinstance(receipt.get("ownership"), dict)
    ):
        raise ValueError("dependency generation receipt schema is invalid")
    if modern and not isinstance(receipt.get("runtimes"), dict):
        raise ValueError("dependency generation runtime metadata is invalid")
    ownership = receipt["ownership"]
    if (
        set(ownership) != {"directories", "files", "links", "sha256", "identities"}
        or not isinstance(ownership.get("directories"), list)
        or not isinstance(ownership.get("files"), dict)
        or not isinstance(ownership.get("links"), list)
        or not isinstance(ownership.get("identities"), dict)
        or HEX_DIGEST.fullmatch(str(ownership.get("sha256", ""))) is None
    ):
        raise ValueError("dependency generation receipt ownership is invalid")
    try:
        checked = datetime.fromisoformat(str(receipt["checkedAt"]))
    except ValueError as error:
        raise ValueError("dependency generation receipt timestamp is invalid") from error
    if checked.tzinfo is None:
        raise ValueError("dependency generation receipt timestamp is invalid")
    tools = receipt.get("tools")
    if not isinstance(tools, dict) or set(tools) != REQUIRED_DISPATCHES:
        raise ValueError("dependency generation tool metadata is invalid")
    for name, record in tools.items():
        dispatch = record.get("dispatch") if isinstance(record, dict) else None
        _validate_dispatch_metadata(name, dispatch)


def _validate_dispatch_metadata(name: str, value: object) -> dict[str, object]:
    if not isinstance(value, dict):
        raise ValueError(f"dependency generation tool metadata is invalid: {name}")
    scripts = "Scripts" if os.name == "nt" else "bin"
    python_name = "python.exe" if os.name == "nt" else "python"
    uv_name = "uv.exe" if os.name == "nt" else "uv"
    if name == "uv":
        expected = {
            "kind": "executable",
            "path": f"bootstrap/{scripts}/{uv_name}",
        }
        fields = {**expected, "sha256": value.get("sha256"), "size": value.get("size")}
        if set(value) != set(fields) or any(value.get(key) != item for key, item in expected.items()):
            raise ValueError("dependency generation tool metadata is invalid: uv")
        digest, size = value.get("sha256"), value.get("size")
    elif name in {"mempalace", "mempalace-mcp", "graphify"}:
        environment = "graphifyy" if name == "graphify" else "mempalace"
        expected = {
            "kind": "python",
            "interpreter": f"uv-tools/{environment}/{scripts}/{python_name}",
            "distribution": environment,
            "entrypoint": name,
        }
        required = set(expected) | {"interpreterSha256", "interpreterSize"}
        linked = {"interpreterLinkTarget", "interpreterTarget"}
        if set(value) not in {frozenset(required), frozenset(required | linked)} or any(
            value.get(key) != item for key, item in expected.items()
        ):
            raise ValueError(f"dependency generation tool metadata is invalid: {name}")
        if linked <= set(value) and (
            not all(isinstance(value.get(key), str) and value.get(key) for key in linked)
            or Path(str(value["interpreterLinkTarget"])).is_absolute()
            or Path(str(value["interpreterTarget"])).is_absolute()
        ):
            raise ValueError(f"dependency generation tool metadata is invalid: {name}")
        digest, size = value.get("interpreterSha256"), value.get("interpreterSize")
    elif name in {"memory", "memory-mcp"}:
        suffix = "dist/cli/main.js" if name == "memory" else "dist/mcp/server.js"
        script = str(value.get("script", "")).replace("\\", "/")
        if "node_modules/.bin/" in script or not script.endswith(".js"):
            raise ValueError(f"dependency generation tool metadata is invalid: {name}")
        if value.get("kind") == "npm":
            expected = {
                "kind": "npm", "script": f"npm/node_modules/@aictx/memory/{suffix}",
                "entrypoint": name,
            }
            required = set(expected) | {"scriptSha256", "scriptSize"}
            if set(value) != required or any(value.get(key) != item for key, item in expected.items()):
                raise ValueError(f"dependency generation tool metadata is invalid: {name}")
            digest, size = value.get("scriptSha256"), value.get("scriptSize")
            if not isinstance(digest, str) or HEX_DIGEST.fullmatch(digest) is None or not isinstance(size, int) or not 0 < size <= MAX_EXECUTABLE_BYTES:
                raise ValueError(f"dependency generation tool metadata is invalid: {name}")
            return value
        expected = {
            "kind": "node",
            "executable": "node/node.exe" if os.name == "nt" else "node/bin/node",
            "script": f"npm/node_modules/@aictx/memory/{suffix}",
            "entrypoint": name,
        }
        required = set(expected) | {"executableSha256", "executableSize", "scriptSha256", "scriptSize"}
        if set(value) != required or any(value.get(key) != item for key, item in expected.items()):
            raise ValueError(f"dependency generation tool metadata is invalid: {name}")
        digest, size = value.get("executableSha256"), value.get("executableSize")
    else:
        raise ValueError(f"dependency generation tool metadata is invalid: {name}")
    if (
        not isinstance(digest, str)
        or HEX_DIGEST.fullmatch(digest) is None
        or not isinstance(size, int)
        or not 0 < size <= MAX_EXECUTABLE_BYTES
    ):
        raise ValueError(f"dependency generation tool metadata is invalid: {name}")
    return value


def _read_pointer_at(project: Path, relative: str) -> dict[str, object]:
    pointer, _ = _bounded_json(project, relative, "pointer")
    if (
        pointer.get("schemaVersion") != POINTER_SCHEMA
        or HEX_ID.fullmatch(str(pointer.get("transactionId", ""))) is None
        or pointer.get("integritySha256") != json_integrity(pointer)
        or set(pointer) != {
            "schemaVersion",
            "transactionId",
            "active",
            "previous",
            "integritySha256",
        }
    ):
        raise ValueError("dependency pointer schema or integrity is invalid")
    _validate_generation_record(pointer.get("active"))
    if pointer.get("previous") is not None:
        _validate_generation_record(pointer["previous"])
    return pointer


def _read_pointer(project: Path) -> dict[str, object]:
    return _read_pointer_at(project, POINTER_NAME)


def _authenticate_selected_generation(
    project: Path,
    active: dict[str, str],
    expected_specification_sha256: str,
    expected_core_sha256: str,
) -> tuple[Path, dict[str, object]]:
    if (
        HEX_DIGEST.fullmatch(expected_specification_sha256) is None
        or HEX_DIGEST.fullmatch(expected_core_sha256) is None
        or active["specificationSha256"] != expected_specification_sha256
        or active["coreSha256"] != expected_core_sha256
    ):
        raise ValueError("dependency generation does not match tracked specification or core")
    generation = project / GENERATIONS_NAME / active["generationId"]
    receipt, receipt_bytes = _bounded_json(
        project,
        f"{GENERATIONS_NAME}/{active['generationId']}/{RECEIPT_NAME}",
        "generation receipt",
        MAX_RECEIPT_BYTES,
    )
    if hashlib.sha256(receipt_bytes).hexdigest() != active["receiptSha256"]:
        raise ValueError("dependency generation receipt digest drift detected")
    _validate_generation_receipt(receipt)
    if receipt.get("receiptIntegritySha256") != json_integrity(receipt):
        raise ValueError("dependency generation receipt integrity drift detected")
    if receipt.get("specificationSha256") != expected_specification_sha256:
        raise ValueError("dependency generation specification digest drift detected")
    if receipt.get("coreSha256") != expected_core_sha256:
        raise ValueError("dependency generation core digest drift detected")
    return generation, receipt


def _validate_selected_generation(
    project: Path,
    active: dict[str, str],
    expected_specification_sha256: str,
    expected_core_sha256: str,
    *,
    verify_installed_core: bool,
) -> Path:
    generation, receipt = _authenticate_selected_generation(
        project,
        active,
        expected_specification_sha256,
        expected_core_sha256,
    )
    verify_sealed_ownership(generation, receipt["ownership"])
    if verify_installed_core:
        core = _read_regular_relative(
            project,
            ".chaos-engine/manifest.json",
            "installed core manifest",
            MAX_CONTROL_BYTES,
        )
        if hashlib.sha256(core).hexdigest() != expected_core_sha256:
            raise ValueError("dependency installed core digest drift detected")
    return generation


def active_generation(
    project: Path,
    *,
    expected_specification_sha256: str,
    expected_core_sha256: str,
) -> tuple[Path, dict[str, object]]:
    project = project.absolute()
    pointer = _read_pointer(project)
    active = _validate_generation_record(pointer.get("active"))
    generation = _validate_selected_generation(
        project,
        active,
        expected_specification_sha256,
        expected_core_sha256,
        verify_installed_core=True,
    )
    return generation, pointer


def pointer_records(project: Path) -> dict[str, object]:
    """Return authenticated active/previous records without accepting paths."""
    return _read_pointer(project.absolute())


def validated_previous(
    project: Path,
    expected_specification_sha256: str,
    expected_core_sha256: str,
    *,
    runner=None,
) -> dict[str, str]:
    """Validate and probe the compatible previous generation before rollback."""
    project = project.absolute()
    pointer = _read_pointer(project)
    previous = _validate_generation_record(pointer.get("previous"))
    generation = _validate_selected_generation(
        project,
        previous,
        expected_specification_sha256,
        expected_core_sha256,
        verify_installed_core=False,
    )
    receipt, _ = _bounded_json(
        project,
        f"{GENERATIONS_NAME}/{previous['generationId']}/{RECEIPT_NAME}",
        "generation receipt",
        MAX_RECEIPT_BYTES,
    )
    environment = os.environ.copy()
    environment["PYTHONDONTWRITEBYTECODE"] = "1"
    execute = runner or run_command
    probes = {
        "uv": ["--version"],
        "mempalace": ["--version"],
        "mempalace-mcp": ["--help"],
        "graphify": ["--version"],
        "memory": ["--help"],
        "memory-mcp": ["--help"],
    }
    for name, arguments in probes.items():
        execute(dispatch_command(generation, receipt, name, arguments), environment)
    return previous


def probe_active(
    project: Path,
    expected_specification_sha256: str,
    expected_core_sha256: str,
    *,
    runner=None,
) -> None:
    """Actively probe every exact dispatch from the selected generation."""
    project = project.absolute()
    generation, pointer = active_generation(
        project,
        expected_specification_sha256=expected_specification_sha256,
        expected_core_sha256=expected_core_sha256,
    )
    active = _validate_generation_record(pointer["active"])
    receipt, _ = _bounded_json(
        project,
        f"{GENERATIONS_NAME}/{active['generationId']}/{RECEIPT_NAME}",
        "generation receipt",
        MAX_RECEIPT_BYTES,
    )
    environment = os.environ.copy()
    environment["PYTHONDONTWRITEBYTECODE"] = "1"
    execute = runner or run_command
    for name, arguments in {
        "uv": ["--version"],
        "mempalace": ["--version"],
        "mempalace-mcp": ["--help"],
        "graphify": ["--version"],
        "memory": ["--help"],
        "memory-mcp": ["--help"],
    }.items():
        execute(dispatch_command(generation, receipt, name, arguments), environment)


def active_dispatch(project: Path, tool: str, arguments: list[str]) -> list[str]:
    """Resolve one account command, falling back to authenticated legacy generations."""
    project = project.absolute()
    account_path = project / ACCOUNT_RECEIPT_NAME
    if account_path.is_file() and not is_link_or_reparse(account_path):
        receipt = read_account_receipt(project)
        commands = receipt["commands"]
        command = commands.get(tool) if isinstance(commands, dict) else None
        if not isinstance(command, str) or not Path(command).is_absolute():
            raise ValueError(f"account dependency tool dispatch is missing: {tool}")
        resolved = Path(command).resolve(strict=True)
        if not resolved.is_file() or (os.name != "nt" and not os.access(resolved, os.X_OK)):
            raise ValueError(f"account dependency tool dispatch is unhealthy: {tool}")
        return [str(resolved), *arguments]
    pointer = _read_pointer(project)
    active = _validate_generation_record(pointer.get("active"))
    generation, receipt = _authenticate_selected_generation(
        project,
        active,
        active["specificationSha256"],
        active["coreSha256"],
    )
    return dispatch_command(generation, receipt, tool, arguments)


def dispatch_command(
    generation: Path,
    receipt: dict[str, object],
    tool: str,
    arguments: list[str],
) -> list[str]:
    tools = receipt.get("tools")
    record = tools.get(tool) if isinstance(tools, dict) else None
    dispatch = record.get("dispatch") if isinstance(record, dict) else None
    if not isinstance(dispatch, dict):
        raise ValueError(f"dependency tool dispatch is missing: {tool}")
    dispatch = _validate_dispatch_metadata(tool, dispatch)
    if dispatch.get("kind") == "python":
        relative = dispatch.get("interpreter")
        distribution = dispatch.get("distribution")
        entrypoint = dispatch.get("entrypoint")
        expected_digest = dispatch.get("interpreterSha256")
        expected_size = dispatch.get("interpreterSize")
        if not all(
            isinstance(item, str) and item
            for item in (relative, distribution, entrypoint, expected_digest)
        ) or HEX_DIGEST.fullmatch(str(expected_digest)) is None:
            raise ValueError(f"dependency Python dispatch is invalid: {tool}")
        interpreter = generation / str(relative)
        try:
            target = str(relative)
            if "interpreterLinkTarget" in dispatch:
                if not interpreter.is_symlink():
                    raise ValueError("recorded interpreter link is missing")
                link_target = os.readlink(interpreter)
                if link_target != dispatch["interpreterLinkTarget"]:
                    raise ValueError("recorded interpreter link target drift detected")
                resolved = _managed_link_target(generation, interpreter)
                target = resolved.relative_to(generation).as_posix()
                if target != dispatch["interpreterTarget"]:
                    raise ValueError("recorded interpreter terminal target drift detected")
            actual_digest = _digest_regular_relative(
                generation, target, f"Python interpreter for {tool}", expected_size  # type: ignore[arg-type]
            )
        except ValueError as error:
            raise ValueError(
                f"dependency Python interpreter is unsafe or a link: {tool}"
            ) from error
        if actual_digest != expected_digest:
            raise ValueError(f"dependency Python interpreter drift detected: {tool}")
        return [
            str(interpreter),
            "-c",
            PYTHON_DISPATCH,
            str(distribution),
            str(entrypoint),
            *arguments,
        ]
    if dispatch.get("kind") == "executable":
        relative = str(dispatch["path"])
        digest = _digest_regular_relative(
            generation,
            relative,
            f"executable for {tool}",
            dispatch["size"],  # type: ignore[arg-type]
        )
        if digest != dispatch["sha256"]:
            raise ValueError(f"dependency executable drift detected: {tool}")
        return [str(generation / relative), *arguments]
    if dispatch.get("kind") == "node":
        executable_relative = str(dispatch["executable"])
        executable_digest = _digest_regular_relative(
            generation, executable_relative, f"Node executable for {tool}", dispatch["executableSize"]  # type: ignore[arg-type]
        )
        if executable_digest != dispatch["executableSha256"]:
            raise ValueError(f"dependency Node executable drift detected: {tool}")
        relative = str(dispatch["script"])
        script = generation / relative
        require_javascript_entrypoint(script, relative)
        digest = _digest_regular_relative(
            generation,
            relative,
            f"npm script for {tool}",
            dispatch["scriptSize"],  # type: ignore[arg-type]
        )
        if digest != dispatch["scriptSha256"]:
            raise ValueError(f"dependency npm script drift detected: {tool}")
        return [str(generation / executable_relative), str(script), *arguments]
    if dispatch.get("kind") == "npm":
        relative = str(dispatch["script"])
        script = generation / relative
        require_javascript_entrypoint(script, relative)
        digest = _digest_regular_relative(
            generation, relative, f"npm script for {tool}", dispatch["scriptSize"]  # type: ignore[arg-type]
        )
        if digest != dispatch["scriptSha256"]:
            raise ValueError(f"dependency npm script drift detected: {tool}")
        return [shutil.which("node") or "node", str(script), *arguments]
    raise ValueError(f"dependency tool dispatch kind is unsupported: {tool}")


def generation_environment(generation: Path, transaction: Path) -> dict[str, str]:
    return {
        "UV_TOOL_DIR": str(generation / "uv-tools"),
        "UV_TOOL_BIN_DIR": str(transaction / "bin"),
        "UV_CACHE_DIR": str(transaction / "uv-cache"),
        "UV_PYTHON_INSTALL_DIR": str(generation / "uv-python"),
        "UV_PYTHON_BIN_DIR": str(transaction / "python-bin"),
        "UV_LINK_MODE": "copy",
        "NPM_CONFIG_PREFIX": str(generation / "npm"),
        "NPM_CONFIG_CACHE": str(transaction / "npm-cache"),
        "PIP_NO_CACHE_DIR": "1",
        "PYTHONDONTWRITEBYTECODE": "1",
    }


def generation_install_plan(
    generation: Path, specification: dict[str, object]
) -> dict[str, list[list[str]]]:
    tools = specification.get("tools")
    if specification.get("schemaVersion") not in {2, 3} or not isinstance(tools, dict):
        raise ValueError("dependency specification schema is unsupported")
    scripts = "Scripts" if os.name == "nt" else "bin"
    bootstrap = generation / "bootstrap"
    uv = executable(bootstrap / scripts, "uv")
    node = str(owned_node(generation))
    npm_cli = generation / (
        "node/node_modules/npm/bin/npm-cli.js" if os.name == "nt"
        else "node/lib/node_modules/npm/bin/npm-cli.js"
    )
    graphify = tools.get("graphify")
    python_runtime = specification.get("runtimes", {}).get("python", {})
    python_version = python_runtime.get("version") if isinstance(python_runtime, dict) else None
    if not isinstance(python_version, str) or re.fullmatch(r"3\.\d+", python_version) is None:
        raise ValueError("Python runtime specification is invalid")
    if not isinstance(graphify, dict):
        raise ValueError("graphify dependency specification is invalid")
    uv_commands = [[uv, "--version"]] if Path(uv).is_file() else [
        [sys.executable, "-m", "venv", "--copies", str(bootstrap)],
        [executable(bootstrap / scripts, "python"), "-m", "pip", "install", "--no-cache-dir", "--upgrade", str(tools["uv"]["package"])],  # type: ignore[index]
    ]
    return {
        "uv": uv_commands,
        "mempalace": [[
            uv,
            "tool",
            "install",
            "--no-cache",
            "--managed-python",
            "--python",
            python_version,
            "--link-mode",
            "copy",
            str(tools["mempalace"]["package"]),  # type: ignore[index]
        ]],
        "graphify": [[
            uv,
            "tool",
            "install",
            "--no-cache",
            "--managed-python",
            "--python",
            python_version,
            "--link-mode",
            "copy",
            "--with",
            str(graphify["with"][0]),  # type: ignore[index]
            str(graphify["package"]),
        ]],
        "memory": [[
            node,
            str(npm_cli),
            "install",
            "--ignore-scripts",
            "--prefix",
            str(generation / "npm"),
            str(tools["memory"]["package"]),  # type: ignore[index]
        ]],
    }


def _generation_dispatches(generation: Path) -> dict[str, dict[str, object]]:
    scripts = "Scripts" if os.name == "nt" else "bin"
    python_name = "python.exe" if os.name == "nt" else "python"
    uv_name = "uv.exe" if os.name == "nt" else "uv"

    def file_record(path: Path) -> tuple[str, int]:
        if is_link_or_reparse(path) or not path.is_file():
            raise ValueError(f"dependency entrypoint is missing or unsafe: {path}")
        return sha256(path), path.stat().st_size

    uv = generation / f"bootstrap/{scripts}/{uv_name}"
    uv_digest, uv_size = file_record(uv)
    records: dict[str, dict[str, object]] = {
        "uv": {"dispatch": {"kind": "executable", "path": uv.relative_to(generation).as_posix(), "sha256": uv_digest, "size": uv_size}}
    }
    for name, environment, distribution in (
        ("mempalace", "mempalace", "mempalace"),
        ("mempalace-mcp", "mempalace", "mempalace"),
        ("graphify", "graphifyy", "graphifyy"),
    ):
        interpreter = generation / f"uv-tools/{environment}/{scripts}/{python_name}"
        link_metadata: dict[str, object] = {}
        if interpreter.is_symlink():
            link_target = os.readlink(interpreter)
            if Path(link_target).is_absolute():
                raise ValueError(f"dependency entrypoint link is not canonical: {interpreter}")
            target = _managed_link_target(generation, interpreter)
            digest, size = file_record(target)
            link_metadata = {
                "interpreterLinkTarget": link_target,
                "interpreterTarget": target.relative_to(generation).as_posix(),
            }
        else:
            digest, size = file_record(interpreter)
        records[name] = {"dispatch": {
            "kind": "python",
            "interpreter": interpreter.relative_to(generation).as_posix(),
            "interpreterSha256": digest,
            "interpreterSize": size,
            "distribution": distribution,
            "entrypoint": name,
            **link_metadata,
        }}
    for name, suffix in (
        ("memory", "dist/cli/main.js"),
        ("memory-mcp", "dist/mcp/server.js"),
    ):
        script = memory_javascript(generation, name)
        require_javascript_entrypoint(script, script.relative_to(generation).as_posix())
        digest, size = file_record(script)
        node = owned_node(generation)
        records[name] = {"dispatch": (
            {**node_dispatch(generation, script), "entrypoint": name}
            if node.is_file() else {
                "kind": "npm", "script": script.relative_to(generation).as_posix(),
                "scriptSha256": digest, "scriptSize": size, "entrypoint": name,
            }
        )}
    return records


def _verify_dispatch_set(
    generation: Path, records: dict[str, dict[str, object]]
) -> None:
    for name in REQUIRED_DISPATCHES:
        dispatch = _validate_dispatch_metadata(name, records[name]["dispatch"])
        if dispatch["kind"] == "python":
            path, digest, size = (
                dispatch.get("interpreterTarget", dispatch["interpreter"]),
                dispatch["interpreterSha256"],
                dispatch["interpreterSize"],
            )
        elif dispatch["kind"] == "node":
            path, digest, size = (
                dispatch["executable"],
                dispatch["executableSha256"],
                dispatch["executableSize"],
            )
        elif dispatch["kind"] == "npm":
            path, digest, size = dispatch["script"], dispatch["scriptSha256"], dispatch["scriptSize"]
        else:
            path, digest, size = dispatch["path"], dispatch["sha256"], dispatch["size"]
        actual = _digest_regular_relative(
            generation,
            str(path),
            f"dispatch target for {name}",
            size,  # type: ignore[arg-type]
        )
        if actual != digest:
            raise ValueError(f"dependency dispatch digest drift detected: {name}")


def _crosscheck_dispatch_ownership(
    ownership: dict[str, object], records: dict[str, dict[str, object]]
) -> None:
    files = ownership.get("files")
    links = ownership.get("links")
    if not isinstance(files, dict) or not valid_link_records(links):
        raise ValueError("dependency sealed ownership files are invalid")
    for name, record in records.items():
        dispatch = record["dispatch"]
        kind = dispatch["kind"]
        if kind == "python":
            path = dispatch.get("interpreterTarget", dispatch["interpreter"])
            digest = dispatch["interpreterSha256"]
            if "interpreterTarget" in dispatch and not any(
                link.get("path") == dispatch["interpreter"]
                and link.get("target") == dispatch["interpreterLinkTarget"]
                for link in links
            ):
                raise ValueError(f"dependency dispatch ownership link drift detected: {name}")
        elif kind == "node":
            path, digest = dispatch["executable"], dispatch["executableSha256"]
        elif kind == "npm":
            path, digest = dispatch["script"], dispatch["scriptSha256"]
        else:
            path, digest = dispatch["path"], dispatch["sha256"]
        if files.get(path) != digest:
            raise ValueError(f"dependency dispatch ownership digest drift detected: {name}")


def _install_candidate_payload(
    project: Path,
    generation: Path,
    transaction: Path,
    specification: dict[str, object],
    core_sha256: str,
    generation_name: str,
    command_runner,
    validate_holds,
    checked_at: datetime,
    confirmer=None,
) -> dict[str, str]:
    environment = generation_environment(generation, transaction)
    completed: dict[str, list[str]] = {}
    for tool, commands in generation_install_plan(generation, specification).items():
        completed[tool] = []
        for command in commands:
            if confirmer is not None:
                package = next((item for item in reversed(command) if not item.startswith("-")), tool)
                confirmer(f"Install {tool} package {package}")
            validate_holds()
            try:
                result = command_runner(command, environment)
            except (OSError, subprocess.SubprocessError) as error:
                raise RuntimeError(f"{tool} install command failed: {command[0]}") from error
            validate_holds()
            completed[tool].append((result.stdout or result.stderr).strip())
    canonicalize_runtime_links(generation)
    records = _generation_dispatches(generation)
    for name, arguments in {
        "uv": ["--version"],
        "mempalace": ["--version"],
        "mempalace-mcp": ["--help"],
        "graphify": ["--version"],
        "memory": ["--help"],
        "memory-mcp": ["--help"],
    }.items():
        validate_holds()
        try:
            result = command_runner(
                dispatch_command(generation, {"tools": records}, name, arguments),
                environment,
            )
        except (OSError, subprocess.SubprocessError) as error:
            raise RuntimeError(f"{name} entrypoint probe failed") from error
        validate_holds()
        _verify_dispatch_set(generation, records)
        records[name]["resolved"] = (result.stdout or result.stderr).strip()
    ownership = sealed_ownership_record(generation)
    _crosscheck_dispatch_ownership(ownership, records)
    receipt: dict[str, object] = {
        "schemaVersion": 3,
        "runtimeContractVersion": 4,
        "checkedAt": checked_at.isoformat(),
        "specificationSha256": specification_digest(specification),
        "coreSha256": core_sha256,
        "environment": {
            key: (
                value
                if key in {"UV_LINK_MODE", "PIP_NO_CACHE_DIR", "PYTHONDONTWRITEBYTECODE"}
                else Path(value).relative_to(generation).as_posix()
            )
            for key, value in environment.items()
            if key
            not in {"UV_CACHE_DIR", "UV_TOOL_BIN_DIR", "UV_PYTHON_BIN_DIR", "NPM_CONFIG_CACHE"}
        },
        "installed": completed,
        "runtimes": {
            name: {
                "version": specification["runtimes"][name]["version"],  # type: ignore[index]
                "platform": platform_key(),
                **select_runtime_artifact(specification, name),
            }
            for name in ("uv", "node")
        },
        "tools": records,
        "ownership": ownership,
    }
    receipt["receiptIntegritySha256"] = json_integrity(receipt)
    receipt_path = generation / RECEIPT_NAME
    receipt_path.write_text(
        json.dumps(receipt, sort_keys=True, separators=(",", ":")) + "\n",
        encoding="utf-8",
    )
    record = {
        "generationId": generation_name,
        "specificationSha256": specification_digest(specification),
        "coreSha256": core_sha256,
        "receiptSha256": sha256(receipt_path),
    }
    _validate_selected_generation(
        project,
        record,
        record["specificationSha256"],
        core_sha256,
        verify_installed_core=True,
    )
    return record


def _cleanup_candidate_paths(removals, holds: ExitStack, failed: bool) -> None:
    try:
        if os.name != "nt":
            try:
                for _, name, parent_hold, child_hold, identity in removals:
                    if identity is None or parent_hold is None:
                        continue
                    opened_here = None
                    if child_hold is None:
                        descriptor = os.open(
                            name,
                            os.O_RDONLY
                            | getattr(os, "O_DIRECTORY", 0)
                            | getattr(os, "O_NOFOLLOW", 0),
                            dir_fd=parent_hold[0],
                        )
                        state = os.fstat(descriptor)
                        child_hold = (descriptor, (state.st_dev, state.st_ino))
                        opened_here = descriptor
                    try:
                        _delete_held_child(parent_hold, name, child_hold, identity)
                    finally:
                        if opened_here is not None:
                            os.close(opened_here)
            finally:
                holds.close()
        else:
            holds.close()
            for path, _, _, _, identity in removals:
                if identity is not None:
                    _delete_windows_tree(path, identity)
    except Exception:
        if not failed:
            raise


def prepare_candidate(
    project: Path,
    specification: dict[str, object],
    core_sha256: str,
    *,
    runner=None,
    now: datetime | None = None,
    generation_id: str | None = None,
    transaction_id: str | None = None,
    reporter=None,
    confirmer=None,
) -> dict[str, str]:
    """Build once at the final path with held no-follow identities."""
    project = _trusted_root(project.absolute(), "project")
    command_runner = runner or run_command
    generation_name = generation_id or secrets.token_hex(16)
    transaction_name = transaction_id or secrets.token_hex(16)
    if HEX_ID.fullmatch(generation_name) is None or HEX_ID.fullmatch(transaction_name) is None:
        raise ValueError("dependency generation or transaction identifier is invalid")
    if HEX_DIGEST.fullmatch(core_sha256) is None:
        raise ValueError("dependency core digest is invalid")
    core = _read_regular_relative(
        project, ".chaos-engine/manifest.json", "installed core manifest", MAX_CONTROL_BYTES
    )
    if hashlib.sha256(core).hexdigest() != core_sha256:
        raise ValueError("dependency installed core digest drift detected")
    generations = project / GENERATIONS_NAME
    transactions = project / ".chaos-engine-runtime-transactions"
    generation = generations / generation_name
    transaction = transactions / transaction_name
    holds = ExitStack()
    created_generation: tuple[int, int] | None = None
    created_transaction: tuple[int, int] | None = None
    generation_hold = None
    transaction_hold = None
    candidate_hold = None
    candidate_transaction_hold = None
    succeeded = False
    try:
        project_hold = holds.enter_context(_hold_directory(project, "project"))
        project_descriptor = project_hold[0]
        container_holds = []
        for path in (generations, transactions):
            if os.name != "nt" and project_descriptor is not None:
                try:
                    os.mkdir(path.name, dir_fd=project_descriptor)
                except FileExistsError:
                    # Re-entry authenticates the existing directory immediately below.
                    pass
                expected = os.stat(
                    path.name, dir_fd=project_descriptor, follow_symlinks=False
                )
            else:
                path.mkdir(exist_ok=True)
                expected = os.stat(path, follow_symlinks=False)
            held = holds.enter_context(_hold_directory(path, path.name))
            if held[1] != (expected.st_dev, expected.st_ino):
                raise ValueError("dependency container identity changed during open")
            container_holds.append((path, held, path.name))
        generation_hold = container_holds[0][1]
        transaction_hold = container_holds[1][1]
        if os.name != "nt":
            os.mkdir(generation_name, dir_fd=generation_hold[0])
            created = os.stat(
                generation_name, dir_fd=generation_hold[0], follow_symlinks=False
            )
            created_generation = (created.st_dev, created.st_ino)
            os.mkdir(transaction_name, dir_fd=transaction_hold[0])
            created = os.stat(
                transaction_name, dir_fd=transaction_hold[0], follow_symlinks=False
            )
            created_transaction = (created.st_dev, created.st_ino)
        else:
            generation.mkdir()
            created = os.stat(generation, follow_symlinks=False)
            created_generation = (created.st_dev, created.st_ino)
            transaction.mkdir()
            created = os.stat(transaction, follow_symlinks=False)
            created_transaction = (created.st_dev, created.st_ino)
        candidate_hold = holds.enter_context(
            _hold_directory(generation, "candidate generation")
        )
        candidate_transaction_hold = holds.enter_context(
            _hold_directory(transaction, "candidate transaction")
        )
        if candidate_hold[1] != created_generation or candidate_transaction_hold[1] != created_transaction:
            raise ValueError("dependency candidate identity changed during open")
        held_paths = [
            (project, project_hold, "project"),
            *container_holds,
            (generation, candidate_hold, "candidate generation"),
            (transaction, candidate_transaction_hold, "candidate transaction"),
        ]

        def validate_holds() -> None:
            for path, held, label in held_paths:
                _assert_held_directory(path, held, label)

        if runner is None:
            provision_generation_runtimes(
                generation, transaction, specification, reporter=reporter,
                confirmer=confirmer,
            )
            validate_holds()

        result = _install_candidate_payload(
            project,
            generation,
            transaction,
            specification,
            core_sha256,
            generation_name,
            command_runner,
            validate_holds,
            now or datetime.now(timezone.utc),
            confirmer=confirmer,
        )
        succeeded = True
        return result
    finally:
        failed = not succeeded
        removals = (
            (
                transaction,
                transaction_name,
                transaction_hold,
                candidate_transaction_hold,
                created_transaction,
            ),
            (
                generation,
                generation_name,
                generation_hold,
                candidate_hold,
                created_generation if failed else None,
            ),
        )
        _cleanup_candidate_paths(removals, holds, failed)


def unlink_owned_marker(path: Path, identity: tuple[int, int]) -> None:
    if is_link_or_reparse(path):
        raise ValueError("dependency build marker ownership drift detected")
    scratch = path.with_name(f"{path.name}{BUILD_MARKER_OWNED_SUFFIX}")
    if scratch.exists() or is_link_or_reparse(scratch):
        raise ValueError(f"dependency build marker scratch collision: {scratch}")
    path.replace(scratch)
    try:
        named = os.stat(scratch, follow_symlinks=False)
        if (named.st_dev, named.st_ino) != identity:
            raise ValueError("dependency build marker ownership drift detected")
        if scratch.read_text(encoding="utf-8") != BUILD_MARKER_MAGIC:
            raise ValueError("dependency build marker ownership drift detected")
        scratch.unlink()
    except (Exception, KeyboardInterrupt, SystemExit):
        if scratch.exists() and not path.exists():
            scratch.replace(path)
        raise


def specification_digest(specification: dict[str, object]) -> str:
    tool_specification = {key: value for key, value in specification.items() if key != "components"}
    encoded = json.dumps(tool_specification, sort_keys=True, separators=(",", ":")).encode()
    return hashlib.sha256(encoded).hexdigest()


def capability_policy_digest(specification: dict[str, object]) -> str:
    components = specification.get("components")
    if not isinstance(components, dict):
        raise ValueError("dependency capability policy is missing or invalid")
    encoded = json.dumps(components, sort_keys=True, separators=(",", ":")).encode()
    return hashlib.sha256(encoded).hexdigest()


def is_generated_python_cache(relative: str, *, directory: bool = False) -> bool:
    parts = relative.split("/")
    if directory:
        return bool(parts) and parts[-1] == "__pycache__"
    return len(parts) >= 2 and parts[-2] == "__pycache__" and parts[-1].endswith(".pyc")


def ownership_digest(
    files: dict[str, str], links: list[dict[str, str]] | None = None
) -> str:
    digest = hashlib.sha256()
    for relative, file_digest in sorted(files.items()):
        digest.update(relative.encode())
        digest.update(b"\0")
        digest.update(bytes.fromhex(file_digest))
    for link in sorted(links or [], key=lambda item: item["path"]):
        digest.update(b"link\0")
        digest.update(link["path"].encode())
        digest.update(b"\0")
        digest.update(link["target"].encode())
        digest.update(b"\0")
        digest.update(str(link.get("type", "symlink")).encode())
        digest.update(b"\0")
        digest.update(str(link.get("tag", "")).encode())
    return digest.hexdigest()


def valid_link_records(value: object) -> bool:
    return isinstance(value, list) and all(
        isinstance(link, dict)
        and set(link) in (
            {"path", "target"},
            {"path", "target", "type", "tag"},
        )
        and isinstance(link["path"], str)
        and isinstance(link["target"], str)
        and (
            set(link) == {"path", "target"}
            or (link["type"] == "junction" and link["tag"] == "0xa0000003")
        )
        for link in value
    )


def _lexical_path(path: Path) -> Path:
    value = os.path.abspath(path)
    if os.name == "nt" and value.startswith("\\\\?\\UNC\\"):
        value = f"\\\\{value[8:]}"
    elif os.name == "nt" and value.startswith("\\\\?\\"):
        value = value[4:]
    return Path(value)


def _windows_uv_junction_record(runtime: Path, path: Path) -> dict[str, str]:
    if os.name != "nt":
        raise ValueError("dependency runtime contains an unsupported reparse point")
    relative = path.relative_to(runtime).as_posix()
    alias = WINDOWS_UV_ALIAS.fullmatch(relative)
    state = os.lstat(path)
    if alias is None or getattr(state, "st_reparse_tag", 0) != WINDOWS_UV_JUNCTION_TAG:
        raise ValueError(f"dependency runtime contains an unsupported reparse point: {relative}")
    try:
        target = _lexical_path(Path(os.readlink(path)))
    except OSError as error:
        raise ValueError(f"dependency runtime junction is unreadable: {relative}") from error
    root = _lexical_path(runtime)
    if not target.is_relative_to(root) or target.parent != _lexical_path(path.parent):
        raise ValueError(f"dependency runtime junction escapes the runtime: {relative}")
    expected = re.fullmatch(
        rf"cpython-{re.escape(alias.group('version'))}\.\d+-windows-"
        rf"{re.escape(alias.group('arch'))}-none",
        target.name,
    )
    if expected is None or is_link_or_reparse(target) or not target.is_dir():
        raise ValueError(f"dependency runtime junction target is invalid: {relative}")
    return {
        "path": relative,
        "target": target.relative_to(root).as_posix(),
        "type": "junction",
        "tag": "0xa0000003",
    }


def _managed_link_target(
    runtime: Path, path: Path, active: set[Path] | None = None
) -> Path:
    root = _lexical_path(runtime)
    candidate = _lexical_path(path)
    if not candidate.is_relative_to(root):
        raise ValueError(f"dependency runtime link escapes the runtime: {path}")
    seen = active or set()
    current = root
    for part in candidate.relative_to(root).parts:
        current /= part
        if current.is_symlink():
            identity = _lexical_path(current)
            if identity in seen:
                raise ValueError(f"dependency runtime link cycle detected: {path}")
            raw_target = Path(os.readlink(current))
            target = _lexical_path(
                raw_target if raw_target.is_absolute() else current.parent / raw_target
            )
            if not target.is_relative_to(root):
                raise ValueError(f"dependency runtime link escapes the runtime: {current}")
            current = _managed_link_target(root, target, seen | {identity})
        elif is_link_or_reparse(current):
            raise ValueError(f"dependency runtime contains an unsupported reparse point: {current}")
        elif not current.exists():
            raise DanglingRuntimeLink(f"dependency runtime link is dangling: {path}")
    return current


def canonicalize_runtime_links(runtime: Path) -> None:
    """Validate every managed link, then rewrite safe targets for relocation."""
    runtime = _lexical_path(runtime)
    links: list[tuple[Path, str, Path]] = []
    for path in runtime_entries(runtime):
        if path.is_symlink():
            resolved = _managed_link_target(runtime, path)
            raw = Path(os.readlink(path))
            immediate = _lexical_path(raw if raw.is_absolute() else path.parent / raw)
            relative = os.path.relpath(immediate, path.parent)
            links.append((path, relative, resolved))
        elif is_link_or_reparse(path):
            _windows_uv_junction_record(runtime, path)
    for path, target, resolved in links:
        if os.readlink(path) == target:
            continue
        path.unlink()
        path.symlink_to(target, target_is_directory=resolved.is_dir())
    for path, _, _ in links:
        _managed_link_target(runtime, path)


def normalized_ownership_record(ownership: object) -> object:
    if not isinstance(ownership, dict):
        return ownership
    directories = ownership.get("directories")
    files = ownership.get("files")
    if (
        not isinstance(directories, list)
        or not all(isinstance(path, str) for path in directories)
        or not isinstance(files, dict)
        or not all(isinstance(path, str) and isinstance(digest, str) for path, digest in files.items())
    ):
        return ownership
    normalized = dict(ownership)
    links = ownership.get("links", [])
    if not valid_link_records(links):
        return ownership
    normalized["directories"] = [
        path for path in directories if not is_generated_python_cache(path, directory=True)
    ]
    normalized_files = {
        path: digest
        for path, digest in files.items()
        if not is_generated_python_cache(path)
    }
    normalized["files"] = normalized_files
    normalized["links"] = links
    normalized["sha256"] = ownership_digest(normalized_files, links)
    return normalized


def ownership_record(
    runtime: Path, expected_links: dict[str, str] | None = None
) -> dict[str, object]:
    if is_link_or_reparse(runtime):
        raise ValueError(f"dependency runtime is a link or reparse point: {runtime}")
    files: dict[str, str] = {}
    directories: list[str] = []
    links: list[dict[str, str]] = []
    for path in runtime_entries(runtime):
        relative = path.relative_to(runtime).as_posix()
        if path.is_symlink():
            target = os.readlink(path)
            if Path(target).is_absolute():
                raise ValueError(f"dependency runtime link target is not relative: {relative}")
            try:
                _managed_link_target(runtime, path)
            except DanglingRuntimeLink:
                if expected_links is None or expected_links.get(relative) != target:
                    raise
            links.append({"path": relative, "target": target})
        elif is_link_or_reparse(path):
            links.append(_windows_uv_junction_record(runtime, path))
        elif path.is_dir():
            if not is_generated_python_cache(relative, directory=True):
                directories.append(relative)
        elif path.is_file() and relative != RECEIPT_NAME:
            if not is_generated_python_cache(relative):
                files[relative] = sha256(path)
    return {
        "directories": directories,
        "files": files,
        "links": links,
        "sha256": ownership_digest(files, links),
    }


def _identity_from_stat(value) -> dict[str, int]:
    return {
        "size": value.st_size,
        "mtimeNs": value.st_mtime_ns,
        "ctimeNs": value.st_ctime_ns,
        "mode": value.st_mode,
        "device": value.st_dev,
        "inode": value.st_ino,
    }


def _file_identity(path: Path) -> dict[str, int]:
    return _identity_from_stat(os.stat(path, follow_symlinks=False))


def sealed_ownership_record(runtime: Path) -> dict[str, object]:
    if is_link_or_reparse(runtime):
        raise ValueError(f"dependency runtime is a link or reparse point: {runtime}")
    files: dict[str, str] = {}
    identities: dict[str, dict[str, int]] = {}
    directories: list[str] = []
    links: list[dict[str, str]] = []
    for path in runtime_entries(runtime):
        relative = path.relative_to(runtime).as_posix()
        if path.is_symlink():
            target = os.readlink(path)
            if Path(target).is_absolute():
                raise ValueError(f"dependency runtime link target is not relative: {relative}")
            _managed_link_target(runtime, path)
            links.append({"path": relative, "target": target})
        elif is_link_or_reparse(path):
            links.append(_windows_uv_junction_record(runtime, path))
        elif path.is_dir():
            if not is_generated_python_cache(relative, directory=True):
                directories.append(relative)
        elif path.is_file() and relative != RECEIPT_NAME:
            if not is_generated_python_cache(relative):
                digest, identity = _capture_regular_relative(
                    runtime, relative, f"sealed generation file {relative}"
                )
                files[relative] = digest
                identities[relative] = identity
    return {
        "directories": directories,
        "files": files,
        "links": links,
        "sha256": ownership_digest(files, links),
        "identities": identities,
    }


def verify_sealed_ownership(
    runtime: Path, expected: object, *, full: bool = False
) -> None:
    if not isinstance(expected, dict):
        raise ValueError("dependency sealed generation ownership is invalid")
    files = expected.get("files")
    identities = expected.get("identities")
    directories = expected.get("directories")
    links = expected.get("links")
    if (
        not isinstance(files, dict)
        or not isinstance(identities, dict)
        or set(files) != set(identities)
        or not isinstance(directories, list)
        or not valid_link_records(links)
    ):
        raise ValueError("dependency sealed generation ownership is invalid")
    actual_files: set[str] = set()
    actual_directories: list[str] = []
    actual_links: list[dict[str, str]] = []
    for path in runtime_entries(runtime):
        relative = path.relative_to(runtime).as_posix()
        if path.is_symlink():
            target = os.readlink(path)
            _managed_link_target(runtime, path)
            actual_links.append({"path": relative, "target": target})
        elif is_link_or_reparse(path):
            actual_links.append(_windows_uv_junction_record(runtime, path))
        elif path.is_dir() and not is_generated_python_cache(relative, directory=True):
            actual_directories.append(relative)
        elif path.is_file() and relative != RECEIPT_NAME and not is_generated_python_cache(relative):
            actual_files.add(relative)
    if (
        actual_directories != directories
        or actual_links != links
        or actual_files != set(files)
    ):
        raise ValueError("dependency sealed generation contains unexpected or missing content")
    for relative, expected_identity in identities.items():
        if not isinstance(expected_identity, dict):
            raise ValueError("dependency sealed generation identity is invalid")
        current = _file_identity(runtime / relative)
        if not full and os.name != "nt" and current == expected_identity:
            continue
        digest, captured = _capture_regular_relative(
            runtime,
            relative,
            f"sealed generation file {relative}",
            current["size"],
        )
        immutable = {"size", "mode", "device", "inode"}
        if any(captured.get(key) != expected_identity.get(key) for key in immutable):
            raise ValueError("dependency sealed generation identity drift detected")
        if digest != files[relative]:
            raise ValueError("dependency sealed generation content drift detected")


def _capture_removal_files(
    generation: Path, files: dict, identities: dict
) -> dict[str, dict[str, int]]:
    captured: dict[str, dict[str, int]] = {}
    for relative, expected_digest in files.items():
        expected_identity = identities.get(relative)
        if not isinstance(relative, str) or not isinstance(expected_identity, dict):
            raise ValueError("dependency sealed generation ownership is invalid")
        digest, identity = _capture_regular_relative(
            generation,
            relative,
            f"sealed generation removal file {relative}",
            expected_identity.get("size"),
        )
        immutable = {"size", "mode", "device", "inode"}
        if digest != expected_digest or any(
            identity.get(key) != expected_identity.get(key) for key in immutable
        ):
            raise ValueError("dependency sealed generation changed before removal")
        captured[relative] = identity
    return captured


def _validate_removal_links(generation: Path, links: list[dict[str, str]]) -> None:
    for link in links:
        relative = str(link["path"])
        path = generation / relative
        if path.is_symlink():
            if os.readlink(path) != link["target"]:
                raise ValueError("dependency sealed generation link changed before removal")
            _managed_link_target(generation, path)
        elif is_link_or_reparse(path):
            if _windows_uv_junction_record(generation, path) != link:
                raise ValueError("dependency sealed generation link changed before removal")
        else:
            raise ValueError("dependency sealed generation link is missing before removal")


def _remove_captured_files(
    generation: Path, captured: dict[str, dict[str, int]]
) -> None:
    for relative, identity in captured.items():
        path = generation / relative
        current = _file_identity(path)
        if any(current.get(key) != identity.get(key) for key in ("size", "device", "inode")):
            raise ValueError("dependency sealed generation changed during removal")
        path.unlink()


def _remove_owned_links(generation: Path, links: list[dict[str, str]]) -> None:
    for link in sorted(links, key=lambda item: str(item["path"]), reverse=True):
        path = generation / str(link["path"])
        if path.is_symlink():
            path.unlink()
        else:
            path.rmdir()


def _remove_generated_caches(generation: Path) -> set[str]:
    generated_directories: set[str] = set()
    for path in reversed(runtime_entries(generation)):
        relative = path.relative_to(generation).as_posix()
        if is_link_or_reparse(path):
            continue
        if path.is_file() and is_generated_python_cache(relative):
            path.unlink()
        elif path.is_dir() and is_generated_python_cache(relative, directory=True):
            generated_directories.add(relative)
    return generated_directories


def _remove_empty_directories(generation: Path, directories: set[str]) -> bool:
    for relative in sorted(
        directories, key=lambda value: (value.count("/"), value), reverse=True
    ):
        path = generation / relative
        if path.is_dir() and not is_link_or_reparse(path):
            try:
                path.rmdir()
            except OSError as error:
                if error.errno not in {errno.ENOTEMPTY, errno.EEXIST}:
                    raise
    try:
        generation.rmdir()
    except OSError as error:
        if error.errno not in {errno.ENOTEMPTY, errno.EEXIST}:
            raise
        return False
    return True


def _remove_sealed_generation_contents(
    generation: Path,
    ownership: dict[str, object],
    receipt_sha256: str,
) -> bool:
    """Remove only receipt-owned entries; retain a nonempty generation as quarantine."""
    files = ownership["files"]
    identities = ownership["identities"]
    directories = ownership["directories"]
    links = ownership["links"]
    if not isinstance(files, dict) or not isinstance(identities, dict):
        raise ValueError("dependency sealed generation ownership is invalid")
    if not isinstance(directories, list) or not valid_link_records(links):
        raise ValueError("dependency sealed generation ownership is invalid")

    captured = _capture_removal_files(generation, files, identities)

    receipt_bytes = _read_regular_relative(
        generation, RECEIPT_NAME, "generation removal receipt", MAX_RECEIPT_BYTES
    )
    if hashlib.sha256(receipt_bytes).hexdigest() != receipt_sha256:
        raise ValueError("dependency generation receipt changed before removal")
    receipt_identity = _file_identity(generation / RECEIPT_NAME)

    _validate_removal_links(generation, links)
    _remove_captured_files(generation, captured)
    _remove_owned_links(generation, links)

    if _file_identity(generation / RECEIPT_NAME) != receipt_identity:
        raise ValueError("dependency generation receipt changed during removal")
    (generation / RECEIPT_NAME).unlink()

    generated_directories = _remove_generated_caches(generation)

    removable_directories = {
        str(relative) for relative in directories if isinstance(relative, str)
    } | generated_directories
    return _remove_empty_directories(generation, removable_directories)


def remove_generation(project: Path, record: dict[str, str]) -> None:
    """Delete one unselected verified generation without following path links."""
    project = _trusted_root(project.absolute(), "project")
    record = _validate_generation_record(record)
    pointer_path = project / POINTER_NAME
    if pointer_path.exists() or is_link_or_reparse(pointer_path):
        pointer = _read_pointer(project)
        if record in (pointer.get("active"), pointer.get("previous")):
            raise ValueError("dependency selected generation cannot be removed")
    generation = _validate_selected_generation(
        project,
        record,
        record["specificationSha256"],
        record["coreSha256"],
        verify_installed_core=False,
    )
    receipt, _ = _bounded_json(
        project,
        f"{GENERATIONS_NAME}/{record['generationId']}/{RECEIPT_NAME}",
        "generation receipt",
        MAX_RECEIPT_BYTES,
    )
    generations = project / GENERATIONS_NAME
    with ExitStack() as holds:
        parent = holds.enter_context(_hold_directory(generations, "generation container"))
        child = holds.enter_context(_hold_directory(generation, "retired generation"))
        expected = child[1]
        named = os.stat(record["generationId"], dir_fd=parent[0], follow_symlinks=False) if (
            os.name != "nt" and isinstance(parent[0], int)
        ) else os.stat(generation, follow_symlinks=False)
        if (named.st_dev, named.st_ino) != expected:
            raise ValueError("dependency generation identity changed before removal")
        verify_sealed_ownership(generation, receipt["ownership"], full=True)
    _remove_sealed_generation_contents(
        generation,
        receipt["ownership"],
        record["receiptSha256"],
    )


def _pointer_generation_records(pointer: dict[str, object]) -> list[dict[str, str]]:
    records = [_validate_generation_record(pointer["active"])]
    if pointer.get("previous") is not None:
        previous = _validate_generation_record(pointer["previous"])
        if previous not in records:
            records.append(previous)
    return records


def prepare_generation_remove(
    project: Path,
    *,
    expected_specification_sha256: str,
    expected_core_sha256: str,
) -> list[dict[str, str]]:
    """Authenticate selected generations and tombstone the active pointer."""
    project = _trusted_root(project.absolute(), "project")
    pointer_path = project / POINTER_NAME
    removing_path = project / POINTER_REMOVING_NAME
    if is_link_or_reparse(pointer_path) or is_link_or_reparse(removing_path):
        raise ValueError("dependency pointer removal path is a link or reparse point")
    if removing_path.exists():
        raise ValueError("dependency generation removal recovery is required")
    pointer = _read_pointer(project)
    records = _pointer_generation_records(pointer)
    for record in records:
        generation = _validate_selected_generation(
            project,
            record,
            record["specificationSha256"],
            record["coreSha256"],
            verify_installed_core=False,
        )
        receipt, _ = _bounded_json(
            project,
            f"{GENERATIONS_NAME}/{record['generationId']}/{RECEIPT_NAME}",
            "generation receipt",
            MAX_RECEIPT_BYTES,
        )
        verify_sealed_ownership(generation, receipt["ownership"], full=True)
    active = records[0]
    if (
        active["specificationSha256"] != expected_specification_sha256
        or active["coreSha256"] != expected_core_sha256
    ):
        raise ValueError("dependency active generation does not match installed core")
    pointer_path.replace(removing_path)
    return records


def cancel_generation_remove(project: Path) -> None:
    """Restore a prepared pointer after another uninstall resource failed."""
    project = _trusted_root(project.absolute(), "project")
    pointer_path = project / POINTER_NAME
    removing_path = project / POINTER_REMOVING_NAME
    if pointer_path.exists() or is_link_or_reparse(pointer_path):
        raise ValueError("dependency generation removal cannot be cancelled")
    pointer = _read_pointer_at(project, POINTER_REMOVING_NAME)
    for record in _pointer_generation_records(pointer):
        _validate_selected_generation(
            project,
            record,
            record["specificationSha256"],
            record["coreSha256"],
            verify_installed_core=False,
        )
    removing_path.replace(pointer_path)


def finalize_generation_remove(project: Path) -> None:
    """Remove only authenticated selected generations, leaving foreign files intact."""
    project = _trusted_root(project.absolute(), "project")
    pointer_path = project / POINTER_NAME
    removing_path = project / POINTER_REMOVING_NAME
    if pointer_path.exists() or is_link_or_reparse(pointer_path):
        raise ValueError("dependency generation removal is not prepared")
    if not removing_path.exists():
        return
    pointer = _read_pointer_at(project, POINTER_REMOVING_NAME)
    for record in _pointer_generation_records(pointer):
        generation = project / GENERATIONS_NAME / record["generationId"]
        if generation.exists() or is_link_or_reparse(generation):
            remove_generation(project, record)
    generations = project / GENERATIONS_NAME
    transactions = project / TRANSACTIONS_NAME
    for container in (generations, transactions):
        if is_link_or_reparse(container):
            raise ValueError("dependency runtime container is a link or reparse point")
        if container.is_dir() and not any(container.iterdir()):
            container.rmdir()
    removing_path.unlink()


def install_plan(runtime: Path, specification: dict[str, object]) -> dict[str, list[list[str]]]:
    tools = specification.get("tools")
    if specification.get("schemaVersion") not in {2, 3} or not isinstance(tools, dict):
        raise ValueError("dependency specification schema is unsupported")
    environment = runtime / "bootstrap"
    scripts = environment / ("Scripts" if os.name == "nt" else "bin")
    uv = executable(scripts, "uv")
    npm_prefix = runtime / "npm"
    npm = npm_executable(runtime / ("node" if os.name == "nt" else "node/bin"), "npm")
    graphify = tools["graphify"]
    if not isinstance(graphify, dict):
        raise ValueError("graphify dependency specification is invalid")
    return {
        "uv": [
            [sys.executable, "-m", "venv", "--copies", str(environment)],
            [executable(scripts, "python"), "-m", "pip", "install", "--upgrade", str(tools["uv"]["package"])],  # type: ignore[index]
        ],
        "mempalace": [
            [uv, "tool", "install", "--managed-python", "--link-mode", "copy", str(tools["mempalace"]["package"])],  # type: ignore[index]
        ],
        "graphify": [
            [uv, "tool", "install", "--managed-python", "--link-mode", "copy", "--with", str(graphify["with"][0]), str(graphify["package"])],  # type: ignore[index]
        ],
        "memory": [
            [npm, "install", "--prefix", str(npm_prefix), str(tools["memory"]["package"])],  # type: ignore[index]
        ],
    }


def tool_environment(runtime: Path) -> dict[str, str]:
    return {
        "UV_TOOL_DIR": str(runtime / "uv-tools"),
        "UV_TOOL_BIN_DIR": str(runtime / "bin"),
        "UV_CACHE_DIR": str(runtime / "uv-cache"),
        "UV_PYTHON_INSTALL_DIR": str(runtime / "uv-python"),
        "UV_PYTHON_BIN_DIR": str(runtime / "python-bin"),
        "UV_LINK_MODE": "copy",
        "NPM_CONFIG_PREFIX": str(runtime / "npm"),
        "PYTHONDONTWRITEBYTECODE": "1",
    }


def freshness(receipt: dict[str, object], now: datetime | None = None) -> str:
    current = now or datetime.now(timezone.utc)
    try:
        checked = datetime.fromisoformat(str(receipt["checkedAt"]))
    except (KeyError, ValueError) as error:
        raise ValueError("dependency receipt has no valid checkedAt timestamp") from error
    if checked.tzinfo is None:
        raise ValueError("dependency receipt timestamp must include a timezone")
    if checked > current + timedelta(minutes=5):
        raise ValueError("dependency receipt timestamp is in the future")
    return "stale" if current - checked > STALE_AFTER else "fresh"


def load_specification(path: Path) -> dict[str, object]:
    value = json.loads(path.read_text(encoding="utf-8"))
    if not isinstance(value, dict):
        raise ValueError("dependency specification must be an object")
    validate_runtime_specification(value)
    return value


def probe_plan(runtime: Path) -> dict[str, list[list[str]]]:
    bootstrap = runtime / "bootstrap" / ("Scripts" if os.name == "nt" else "bin")
    bin_dir = runtime / "bin"
    node = str(owned_node(runtime))
    return {
        "uv": [[executable(bootstrap, "uv"), "--version"]],
        "mempalace": [
            [executable(bin_dir, "mempalace"), "--version"],
            [executable(bin_dir, "mempalace-mcp"), "--help"],
        ],
        "graphify": [[executable(bin_dir, "graphify"), "--version"]],
        "memory": [
            [node, str(memory_javascript(runtime, "memory")), "--help"],
            [node, str(memory_javascript(runtime, "memory-mcp")), "--help"],
        ],
    }


def run_command(command: list[str], environment: dict[str, str]) -> subprocess.CompletedProcess[str]:
    merged = os.environ.copy()
    merged.update(environment)
    return subprocess.run(  # nosec B603
        command, check=True, capture_output=True, text=True, env=merged, timeout=300
    )


def execute_plan(
    runtime: Path,
    specification: dict[str, object],
    runner=run_command,
    now: datetime | None = None,
) -> dict[str, object]:
    environment = tool_environment(runtime)
    completed: dict[str, list[str]] = {}
    for tool, commands in install_plan(runtime, specification).items():
        completed[tool] = []
        for command in commands:
            try:
                result = runner(command, environment)
            except (OSError, subprocess.SubprocessError) as error:
                raise RuntimeError(f"{tool} install command failed: {command[0]}") from error
            completed[tool].append((result.stdout or result.stderr).strip())
    canonicalize_runtime_links(runtime)
    probes: dict[str, list[str]] = {}
    for tool, commands in probe_plan(runtime).items():
        probes[tool] = []
        for command in commands:
            try:
                result = runner(command, environment)
            except (OSError, subprocess.SubprocessError) as error:
                raise RuntimeError(f"{tool} entrypoint probe failed: {command[0]}") from error
            probes[tool].append((result.stdout or result.stderr).strip())
    records = {}
    tools = specification["tools"]  # validated by install_plan
    for tool, commands in probe_plan(runtime).items():
        records[tool] = {
            "requested": tools[tool]["package"],  # type: ignore[index]
            "commands": [relative_command(runtime, command) for command in commands],
            "entrypoints": {
                Path(command[0]).relative_to(runtime).as_posix(): sha256(Path(command[0]))
                for command in commands
            },
            "resolved": probes[tool],
        }
    receipt: dict[str, object] = {
        "schemaVersion": RECEIPT_SCHEMA,
        "runtimeContractVersion": RUNTIME_CONTRACT_VERSION,
        "checkedAt": (now or datetime.now(timezone.utc)).isoformat(),
        "specificationSha256": specification_digest(specification),
        "capabilityPolicySha256": capability_policy_digest(specification),
        "environment": {
            key: value
            if key in {"PYTHONDONTWRITEBYTECODE", "UV_LINK_MODE"}
            else Path(value).relative_to(runtime).as_posix()
            for key, value in environment.items()
        },
        "installed": completed,
        "tools": records,
    }
    metadata = json.dumps(receipt, sort_keys=True, separators=(",", ":")).encode()
    ownership = ownership_record(runtime)
    if not ownership["links"]:
        del ownership["links"]
    ownership["metadataSha256"] = hashlib.sha256(metadata).hexdigest()
    receipt["ownership"] = ownership
    integrity = json.dumps(receipt, sort_keys=True, separators=(",", ":")).encode()
    receipt["receiptIntegritySha256"] = hashlib.sha256(integrity).hexdigest()
    (runtime / RECEIPT_NAME).write_text(
        json.dumps(receipt, indent=2, sort_keys=True) + "\n", encoding="utf-8"
    )
    return receipt


def relative_command(runtime: Path, command: list[str]) -> list[str]:
    result = command.copy()
    for index, item in enumerate(result):
        try:
            result[index] = Path(item).relative_to(runtime).as_posix()
        except ValueError:
            # External system executables remain absolute; only runtime-owned paths relocate.
            pass
    return result


def runtime_ownership_state(
    runtime: Path,
    receipt: dict[str, object],
    specification: dict[str, object],
) -> str:
    """Classify authenticated runtime content without following foreign links."""
    verify_receipt_integrity(receipt)
    ownership = normalized_ownership_record(receipt.get("ownership"))
    if not isinstance(ownership, dict):
        raise ValueError("dependency runtime ownership record is invalid")
    metadata_receipt = {
        key: value
        for key, value in receipt.items()
        if key not in {"ownership", "receiptIntegritySha256"}
    }
    metadata = json.dumps(metadata_receipt, sort_keys=True, separators=(",", ":")).encode()
    expected_files = ownership.get("files")
    expected_directories = ownership.get("directories")
    expected_links = ownership.get("links", [])
    if (
        not isinstance(expected_files, dict)
        or not isinstance(expected_directories, list)
        or not valid_link_records(expected_links)
    ):
        raise ValueError("dependency runtime ownership record is invalid")
    expected_link_map = {item["path"]: item["target"] for item in expected_links}
    actual = ownership_record(runtime, expected_links=expected_link_map)
    actual["metadataSha256"] = hashlib.sha256(metadata).hexdigest()
    if actual == ownership:
        specification_changed = receipt.get("specificationSha256") != specification_digest(
            specification
        )
        capability_changed = (
            "capabilityPolicySha256" in receipt
            and receipt.get("capabilityPolicySha256")
            != capability_policy_digest(specification)
        )
        contract_changed = receipt.get("runtimeContractVersion") != RUNTIME_CONTRACT_VERSION
        return (
            "specification-stale"
            if specification_changed or capability_changed or contract_changed
            else "healthy"
        )
    actual_files = actual["files"]
    actual_directories = actual["directories"]
    actual_links = actual["links"]
    actual_link_map = {item["path"]: item["target"] for item in actual_links}
    if (
        not set(actual_files) <= set(expected_files)
        or not set(actual_directories) <= set(expected_directories)
        or not set(actual_link_map) <= set(expected_link_map)
    ):
        raise ValueError("dependency runtime contains foreign content")
    if (
        any(expected_files[path] != digest for path, digest in actual_files.items())
        or any(expected_link_map[path] != target for path, target in actual_link_map.items())
        or actual.get("metadataSha256") != ownership.get("metadataSha256")
    ):
        raise ValueError("dependency runtime ownership drift detected")
    return "missing-only"


def repair(  # noqa: MC0001 - one locked transaction keeps recovery and publication atomic.
    runtime: Path,
    specification: dict[str, object],
    runner=run_command,
    now: datetime | None = None,
    force: bool = False,
) -> dict[str, object]:
    runtime = runtime.absolute()
    if is_link_or_reparse(runtime):
        raise ValueError(f"dependency runtime is a link or reparse point: {runtime}")
    runtime.parent.mkdir(parents=True, exist_ok=True)
    with runtime_lock(runtime):
        backup = runtime.with_name(f"{runtime.name}.backup")
        removing = runtime.with_name(f"{runtime.name}.removing")
        building = runtime.with_name(f"{runtime.name}.building")
        building_owned = building.with_name(f"{building.name}{BUILD_MARKER_OWNED_SUFFIX}")
        if any(
            is_link_or_reparse(path)
            for path in (backup, removing, building, building_owned)
        ):
            raise ValueError("dependency recovery path is a link or reparse point")
        if building_owned.exists():
            if building.exists():
                raise ValueError("dependency build marker recovery collision")
            with building_owned.open("r", encoding="utf-8") as marker:
                if marker.read() != BUILD_MARKER_MAGIC:
                    raise ValueError(
                        f"dependency build marker scratch collision: {building_owned}"
                    )
                opened = os.fstat(marker.fileno())
                named = os.stat(building_owned, follow_symlinks=False)
                owned_identity = (opened.st_dev, opened.st_ino)
                if owned_identity != (named.st_dev, named.st_ino):
                    raise ValueError(
                        "dependency build marker scratch ownership drift detected"
                    )
            building_owned.replace(building)
            named = os.stat(building, follow_symlinks=False)
            if (named.st_dev, named.st_ino) != owned_identity:
                building.replace(building_owned)
                raise ValueError("dependency build marker scratch ownership drift detected")
        if removing.exists():
            if runtime.exists():
                remove(runtime, specification, removal_path=removing, already_locked=True)
            else:
                raise ValueError("dependency removal recovery is required before repair")
        if building.exists():
            if building.read_text(encoding="utf-8") != BUILD_MARKER_MAGIC:
                raise ValueError(f"dependency build marker collision: {building}")
            if runtime.exists() and not backup.exists():
                try:
                    verify_receipt(runtime, read_receipt(runtime), specification)
                except (OSError, ValueError):
                    shutil.rmtree(runtime)
                else:
                    building.unlink()
                    return read_receipt(runtime)
            elif runtime.exists():
                shutil.rmtree(runtime)
            if backup.exists():
                verify_receipt(backup, read_receipt(backup))
                backup.replace(runtime)
            building.unlink()
        if backup.exists() and not runtime.exists():
            verify_receipt(backup, read_receipt(backup))
            backup.replace(runtime)
        elif backup.exists():
            verify_receipt(runtime, read_receipt(runtime))
            backup.replace(removing)
            remove(runtime, specification, removal_path=removing, already_locked=True)
        previous = runtime.exists()
        if previous:
            current = read_receipt(runtime)
            state = runtime_ownership_state(runtime, current, specification)
            if state == "healthy" and not force:
                doctor(runtime, runner=runner, now=now, specification=specification)
                return upgrade_capability_receipt(runtime, current, specification)
        marker_created = False
        marker_identity: tuple[int, int] | None = None
        try:
            if previous:
                runtime.replace(backup)
            descriptor = os.open(
                building,
                os.O_WRONLY | os.O_CREAT | os.O_EXCL | getattr(os, "O_BINARY", 0),
                0o600,
            )
            marker_created = True
            with os.fdopen(descriptor, "w", encoding="utf-8") as marker:
                marker.write(BUILD_MARKER_MAGIC)
                marker.flush()
                os.fsync(marker.fileno())
                opened = os.fstat(marker.fileno())
                marker_identity = (opened.st_dev, opened.st_ino)
            runtime.mkdir()
            receipt = execute_plan(runtime, specification, runner=runner, now=now)
        except (Exception, KeyboardInterrupt, SystemExit):
            if runtime.exists():
                shutil.rmtree(runtime, ignore_errors=True)
            if previous and backup.exists() and not runtime.exists():
                backup.replace(runtime)
            if marker_created and building.exists() and not backup.exists():
                unlink_owned_marker(building, marker_identity)  # type: ignore[arg-type]
            raise
        else:
            unlink_owned_marker(building, marker_identity)  # type: ignore[arg-type]
            if backup.exists():
                backup.replace(removing)
                try:
                    remove(runtime, specification, removal_path=removing, already_locked=True)
                except OSError:
                    # The verified tombstone is durable recovery state for the next call.
                    pass
            return receipt


def read_receipt(runtime: Path) -> dict[str, object]:
    try:
        receipt = json.loads((runtime / RECEIPT_NAME).read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError) as error:
        raise ValueError("ChaosEngine dependency receipt is missing or invalid") from error
    required = {"schemaVersion", "checkedAt", "specificationSha256", "environment", "installed", "tools", "ownership", "receiptIntegritySha256"}
    if (
        not isinstance(receipt, dict)
        or receipt.get("schemaVersion") != RECEIPT_SCHEMA
        or not required <= set(receipt)
        or set(receipt.get("tools", {})) != {"uv", "mempalace", "graphify", "memory"}
    ):
        raise ValueError("ChaosEngine dependency receipt schema is unsupported")
    return receipt


def verify_receipt(
    runtime: Path,
    receipt: dict[str, object],
    specification: dict[str, object] | None = None,
) -> None:
    verify_receipt_integrity(receipt)
    if specification is not None and receipt["specificationSha256"] != specification_digest(specification):
        raise ValueError("dependency runtime specification drift detected")
    if (
        specification is not None
        and receipt.get("runtimeContractVersion") != RUNTIME_CONTRACT_VERSION
    ):
        raise ValueError("dependency runtime contract drift detected")
    if (
        specification is not None
        and "capabilityPolicySha256" in receipt
        and receipt["capabilityPolicySha256"] != capability_policy_digest(specification)
    ):
        raise ValueError("dependency capability policy drift detected")
    ownership = normalized_ownership_record(receipt["ownership"])
    metadata_receipt = {
        key: value
        for key, value in receipt.items()
        if key not in {"ownership", "receiptIntegritySha256"}
    }
    metadata = json.dumps(metadata_receipt, sort_keys=True, separators=(",", ":")).encode()
    expected = ownership_record(runtime)
    expected["metadataSha256"] = hashlib.sha256(metadata).hexdigest()
    if not isinstance(ownership, dict) or ownership != expected:
        raise ValueError("dependency runtime ownership drift detected")
    tools = receipt["tools"]
    if not isinstance(tools, dict):
        raise ValueError("dependency tool receipt is invalid")
    for record in tools.values():
        if not isinstance(record, dict) or not {"requested", "commands", "entrypoints", "resolved"} <= set(record):
            raise ValueError("dependency tool receipt is invalid")
        entrypoints = record["entrypoints"]
        if not isinstance(entrypoints, dict):
            raise ValueError("dependency entrypoint receipt is invalid")
        for path, expected_digest in entrypoints.items():
            candidate = runtime / path if isinstance(path, str) else runtime
            if not isinstance(path, str) or not isinstance(expected_digest, str) or sha256(candidate) != expected_digest:
                raise ValueError("dependency entrypoint drift detected")


def verify_receipt_integrity(receipt: dict[str, object]) -> None:
    integrity_receipt = {key: value for key, value in receipt.items() if key != "receiptIntegritySha256"}
    integrity = json.dumps(integrity_receipt, sort_keys=True, separators=(",", ":")).encode()
    if receipt["receiptIntegritySha256"] != hashlib.sha256(integrity).hexdigest():
        raise ValueError("dependency receipt integrity drift detected")


def upgrade_capability_receipt(
    runtime: Path, receipt: dict[str, object], specification: dict[str, object]
) -> dict[str, object]:
    if "capabilityPolicySha256" in receipt:
        return receipt
    upgraded = dict(receipt)
    upgraded["capabilityPolicySha256"] = capability_policy_digest(specification)
    ownership = dict(upgraded["ownership"])  # type: ignore[arg-type]
    upgraded["ownership"] = ownership
    metadata_receipt = {
        key: value
        for key, value in upgraded.items()
        if key not in {"ownership", "receiptIntegritySha256"}
    }
    metadata = json.dumps(metadata_receipt, sort_keys=True, separators=(",", ":")).encode()
    ownership["metadataSha256"] = hashlib.sha256(metadata).hexdigest()
    integrity_receipt = {
        key: value for key, value in upgraded.items() if key != "receiptIntegritySha256"
    }
    integrity = json.dumps(integrity_receipt, sort_keys=True, separators=(",", ":")).encode()
    upgraded["receiptIntegritySha256"] = hashlib.sha256(integrity).hexdigest()
    receipt_path = runtime / RECEIPT_NAME
    temporary = runtime / f".{RECEIPT_NAME}.capability-upgrade"
    if temporary.exists() or is_link_or_reparse(temporary):
        raise ValueError("dependency capability receipt upgrade collision")
    descriptor = os.open(
        temporary,
        os.O_WRONLY | os.O_CREAT | os.O_EXCL | getattr(os, "O_BINARY", 0),
        0o600,
    )
    try:
        with os.fdopen(descriptor, "w", encoding="utf-8") as stream:
            json.dump(upgraded, stream, indent=2, sort_keys=True)
            stream.write("\n")
            stream.flush()
            os.fsync(stream.fileno())
        temporary.replace(receipt_path)
    finally:
        if temporary.exists() and not is_link_or_reparse(temporary):
            temporary.unlink()
    verify_receipt(runtime, upgraded, specification)
    return upgraded


def doctor(
    runtime: Path,
    runner=run_command,
    now: datetime | None = None,
    specification: dict[str, object] | None = None,
) -> dict[str, object]:
    receipt = read_receipt(runtime)
    verify_receipt(runtime, receipt, specification)
    environment = tool_environment(runtime)
    for commands in probe_plan(runtime).values():
        for command in commands:
            runner(command, environment)
    return {
        "status": "healthy",
        "freshness": freshness(receipt, now),
        "checkedAt": receipt["checkedAt"],
    }


def status(
    runtime: Path,
    specification: dict[str, object],
    now: datetime | None = None,
) -> dict[str, object]:
    receipt = read_receipt(runtime)
    verify_receipt(runtime, receipt, specification)
    return {
        "status": "healthy",
        "freshness": freshness(receipt, now),
        "checkedAt": receipt["checkedAt"],
    }


def remove(
    runtime: Path,
    specification: dict[str, object],
    removal_path: Path | None = None,
    already_locked: bool = False,
) -> None:
    runtime = runtime.absolute()
    removing = removal_path or runtime.with_name(f"{runtime.name}.removing")
    with (nullcontext() if already_locked else runtime_lock(runtime)):
        if is_link_or_reparse(runtime) or is_link_or_reparse(removing):
            raise ValueError("dependency removal path is a link or reparse point")
        if removal_path is None and runtime.exists() and removing.exists():
            raise ValueError(f"dependency removal collision: {removing}")
        if removal_path is None and runtime.exists():
            receipt = read_receipt(runtime)
            verify_receipt(runtime, receipt, specification)
            runtime.replace(removing)
        elif not removing.exists():
            return
        if not any(removing.iterdir()):
            removing.rmdir()
            return
        receipt = read_receipt(removing)
        if removal_path is not None or not runtime.exists():
            verify_receipt_integrity(receipt)
        else:
            verify_receipt(removing, receipt, specification)
        ownership = normalized_ownership_record(receipt["ownership"])
        files = ownership.get("files") if isinstance(ownership, dict) else None
        directories = ownership.get("directories") if isinstance(ownership, dict) else None
        links = ownership.get("links", []) if isinstance(ownership, dict) else None
        if (
            not isinstance(files, dict)
            or not isinstance(directories, list)
            or not valid_link_records(links)
        ):
            raise ValueError("dependency removal ownership record is invalid")
        expected_links = {item["path"]: item["target"] for item in links}
        entries = runtime_entries(removing)
        present_links = {
            path.relative_to(removing).as_posix(): os.readlink(path)
            for path in entries
            if path.is_symlink()
        }
        if not set(present_links) <= set(expected_links) or any(
            expected_links[path] != target for path, target in present_links.items()
        ):
            raise ValueError("dependency removal link ownership drift detected")
        present = {
            path.relative_to(removing).as_posix()
            for path in entries
            if not is_link_or_reparse(path) and path.is_file()
        }
        generated_files = {
            relative for relative in present if is_generated_python_cache(relative)
        }
        allowed = set(files) | generated_files | {RECEIPT_NAME}
        if not present <= allowed:
            raise ValueError("dependency removal contains an unowned file")
        for relative in sorted(present - {RECEIPT_NAME}):
            path = removing / relative
            if relative not in generated_files and sha256(path) != files[relative]:
                raise ValueError("dependency removal ownership drift detected")
            path.unlink()
        for relative in sorted(present_links):
            (removing / relative).unlink()
        present_directories = {
            path.relative_to(removing).as_posix()
            for path in entries
            if not is_link_or_reparse(path) and path.is_dir()
        }
        expected_directories = set(directories) | {
            relative
            for relative in present_directories
            if is_generated_python_cache(relative, directory=True)
        }
        if not present_directories <= expected_directories:
            raise ValueError("dependency removal directory ownership drift detected")
        for directory in sorted(
            (
                path
                for path in entries
                if not is_link_or_reparse(path) and path.is_dir()
            ),
            key=lambda path: len(path.parts),
            reverse=True,
        ):
            directory.rmdir()
        (removing / RECEIPT_NAME).unlink()
        removing.rmdir()


def prepare_remove(runtime: Path, specification: dict[str, object]) -> Path:
    runtime = runtime.absolute()
    removing = runtime.with_name(f"{runtime.name}.removing")
    with runtime_lock(runtime):
        if is_link_or_reparse(runtime) or is_link_or_reparse(removing):
            raise ValueError("dependency removal path is a link or reparse point")
        if removing.exists():
            raise ValueError(f"dependency removal collision: {removing}")
        verify_receipt(runtime, read_receipt(runtime), specification)
        runtime.replace(removing)
    return removing


def cancel_remove(runtime: Path) -> None:
    runtime = runtime.absolute()
    removing = runtime.with_name(f"{runtime.name}.removing")
    with runtime_lock(runtime):
        if runtime.exists() or not removing.exists():
            raise ValueError("dependency removal cannot be cancelled")
        verify_receipt(removing, read_receipt(removing))
        removing.replace(runtime)


def finalize_remove(runtime: Path, specification: dict[str, object]) -> None:
    removing = runtime.absolute().with_name(f"{runtime.name}.removing")
    remove(runtime, specification, removal_path=removing)


def parser() -> argparse.ArgumentParser:
    result = argparse.ArgumentParser(description=__doc__)
    result.add_argument("command", choices=("doctor", "repair", "upgrade"))
    result.add_argument("--runtime", required=True, type=Path)
    result.add_argument(
        "--specification", type=Path, default=Path(__file__).with_name("dependencies.json")
    )
    return result


def main() -> int:
    args = parser().parse_args()
    try:
        if args.command == "doctor":
            with runtime_lock(args.runtime.absolute()):
                result = doctor(
                    args.runtime,
                    specification=load_specification(args.specification),
                )
        else:
            result = repair(
                args.runtime,
                load_specification(args.specification),
                force=args.command == "upgrade",
            )
    except (OSError, RuntimeError, ValueError, subprocess.SubprocessError) as error:
        print(str(error), file=sys.stderr)
        return 1
    print(json.dumps(result, sort_keys=True))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
