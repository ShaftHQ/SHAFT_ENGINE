#!/usr/bin/env python3
"""Run bounded real-tool acceptance for the immutable ChaosEngine installer."""

from __future__ import annotations

import argparse
import base64
import hashlib
import io
import importlib.util
import json
import os
import platform
import re
import shutil
import subprocess  # nosec B404 - fixed repository-owned commands only.
import sys
import tarfile
import tempfile
import time
import urllib.parse
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
TOOLS = ("uv", "mempalace", "mempalace-mcp", "graphify", "memory", "memory-mcp")
PROBES = {
    "uv": ["--version"],
    "mempalace": ["--version"],
    "graphify": ["--version"],
    "memory": ["--help"],
}
PHASE_TIMEOUT_SECONDS = 600
MCP_START_TIMEOUT_SECONDS = 10
MCP_PROTOCOL_VERSION = "2025-06-18"
COMMIT = re.compile(r"[0-9a-f]{40}")
HEX_ID = re.compile(r"[0-9a-f]{32}")
SECRET_NAME = re.compile(r"(?:TOKEN|SECRET|PASSWORD|API_KEY|PRIVATE_KEY)", re.I)
URL_START = re.compile(r"https?://", re.I)
ABSOLUTE_ROOT = re.compile(
    r"(?<![A-Za-z0-9])(?:"
    r"[A-Za-z]:[\\/]"
    r"|\\\\[^\\/\r\n\"';,:]+[\\/][^\\/\r\n\"';,:]+(?:[\\/]|$)"
    r"|/(?:tmp|var/folders|private/var/folders|home/runner/work)(?:/|$)"
    r"|/Users/[^/\r\n\"';,:]+(?:/|$)"
    r")"
)
PATH_DELIMITERS = frozenset(";,:\r\n\t\"'<>{}[]()")
SANITIZER_INPUT_LIMIT = 8192
SANITIZER_OUTPUT_LIMIT = 500
SANITIZER_TRUNCATION_MARKER = "\n...<truncated>...\n"
STATUS_SUMMARY_FIELDS = ("status", "commit", "kernel", "hosts", "dependencies")
HOST_COMPONENT_FIELDS = ("status", "detail", "code")
DEPENDENCY_COMPONENT_FIELDS = ("status", "action", "probe", "detail", "code")
KNOWN_BASE_SHA = "1dec809c7c43709a8fcceef5e53690d124012eb3"
POSIX_BASE_FAILURE_DETAIL = (
    "CE-INSTALL-FAILED: ChaosEngine doctor did not report a healthy installation; "
    "failed phase: Verify installation; unhealthy: hooks, mcps"
)
ACCOUNT_COMMAND_NAMES = frozenset((
    "uv", "uvx", "python3", "node", "npm", "npx", "java", "mempalace",
    "mempalace-mcp", "graphify", "memory", "memory-mcp", "ctx7",
))
PLATFORM_PREREQUISITE_COMMAND_NAMES = frozenset((
    "uv", "uvx", "python3", "node", "npm", "npx", "java",
))
OPTIONAL_ACCOUNT_COMMAND_NAMES = frozenset((
    "mempalace", "mempalace-mcp", "graphify", "memory", "memory-mcp", "ctx7",
))


class AcceptancePhaseFailure(RuntimeError):
    """Carry a bounded phase name without losing the original failure evidence."""

    def __init__(self, phase: str, cause: Exception):
        self.phase = phase
        self.cause = cause
        super().__init__(f"{phase}: {cause}")


class AcceptanceCommandFailure(RuntimeError):
    """Capture the failing fixed command separately from its sanitized output."""

    def __init__(self, command: list[str], returncode: int, detail: str):
        self.command = tuple(command)
        self.returncode = returncode
        super().__init__(f"command failed ({returncode}): {sanitize(detail)}")


def clean_environment(base: dict[str, str] | None = None) -> dict[str, str]:
    return {
        key: value
        for key, value in (base or os.environ).items()
        if SECRET_NAME.search(key) is None
    }


def download_environment(base: dict[str, str] | None = None) -> dict[str, str]:
    """Allow only GitHub's scoped CI token into repeated public downloads."""
    source = base or os.environ
    environment = clean_environment(source)
    if source.get("GITHUB_TOKEN"):
        environment["GITHUB_TOKEN"] = source["GITHUB_TOKEN"]
    return environment


def wrapper_environment() -> dict[str, str]:
    """Keep public-wrapper acceptance independent of ambient optional hosts."""
    environment = download_environment()
    if os.name == "nt":
        system_root = environment.get("SystemRoot", r"C:\\Windows")
        environment["PATH"] = os.pathsep.join((str(Path(system_root) / "System32"), system_root))
    else:
        environment["PATH"] = os.defpath
    return environment


def isolated_account_environment(root: Path) -> dict[str, str]:
    """Create one disposable account without inheriting any user cache root."""
    root = root.resolve()
    roots = {
        "home": root / "home",
        "appdata": root / "appdata",
        "localappdata": root / "localappdata",
        "xdg-cache": root / "xdg-cache",
        "xdg-config": root / "xdg-config",
        "xdg-data": root / "xdg-data",
        "xdg-state": root / "xdg-state",
        "xdg-runtime": root / "xdg-runtime",
        "xdg-bin": root / "xdg-bin",
        "temp": root / "tmp",
        "npm-cache": root / "npm-cache",
        "npm-prefix": root / "npm-prefix",
        "npm-config": root / "npm-config",
        "uv-cache": root / "uv-cache",
        "uv-tools": root / "uv-tools",
        "uv-tool-bin": root / "uv-tool-bin",
        "uv-python": root / "uv-python",
        "uv-python-bin": root / "uv-python-bin",
        "python-user": root / "python-user",
        "pip-cache": root / "pip-cache",
        "pip-config": root / "pip-config",
    }
    for path in roots.values():
        path.mkdir(parents=True, exist_ok=True)
    home = roots["home"]
    environment = wrapper_environment()
    for name in (
        "CONDA_PREFIX", "PIP_CONFIG_FILE", "PIP_REQUIRE_VIRTUALENV", "PIP_USER",
        "PYTHONHOME", "PYTHONPATH", "VIRTUAL_ENV", "UV_CONFIG_FILE",
        "UV_PROJECT_ENVIRONMENT", "XDG_BIN_HOME",
    ):
        environment.pop(name, None)
    environment.update({
        "HOME": str(home),
        "USERPROFILE": str(home),
        "APPDATA": str(roots["appdata"]),
        "LOCALAPPDATA": str(roots["localappdata"]),
        "XDG_CACHE_HOME": str(roots["xdg-cache"]),
        "XDG_CONFIG_HOME": str(roots["xdg-config"]),
        "XDG_DATA_HOME": str(roots["xdg-data"]),
        "XDG_STATE_HOME": str(roots["xdg-state"]),
        "XDG_RUNTIME_DIR": str(roots["xdg-runtime"]),
        "XDG_BIN_HOME": str(roots["xdg-bin"]),
        "TMPDIR": str(roots["temp"]),
        "TEMP": str(roots["temp"]),
        "TMP": str(roots["temp"]),
        "NPM_CONFIG_CACHE": str(roots["npm-cache"]),
        "NPM_CONFIG_PREFIX": str(roots["npm-prefix"]),
        "NPM_CONFIG_USERCONFIG": str(roots["npm-config"] / "npmrc"),
        "NPM_CONFIG_GLOBALCONFIG": str(roots["npm-config"] / "global-npmrc"),
        "UV_CACHE_DIR": str(roots["uv-cache"]),
        "UV_TOOL_DIR": str(roots["uv-tools"]),
        "UV_TOOL_BIN_DIR": str(roots["uv-tool-bin"]),
        "UV_PYTHON_INSTALL_DIR": str(roots["uv-python"]),
        "UV_PYTHON_BIN_DIR": str(roots["uv-python-bin"]),
        "UV_PYTHON_DIR": str(roots["uv-python"]),
        "PYTHONUSERBASE": str(roots["python-user"]),
        "PIP_CACHE_DIR": str(roots["pip-cache"]),
        "PIP_CONFIG_FILE": str(roots["pip-config"] / "pip.conf"),
        "PYTHONNOUSERSITE": "1",
    })
    npm_executable_bin = roots["npm-prefix"] / ("Scripts" if os.name == "nt" else "bin")
    npm_executable_bin.mkdir(parents=True, exist_ok=True)
    environment["PATH"] = os.pathsep.join((
        environment["UV_TOOL_BIN_DIR"], str(npm_executable_bin), environment["PATH"],
    ))
    if os.name == "nt":
        drive = home.drive
        if not drive:
            raise RuntimeError("isolated Windows account has no drive")
        environment["HOMEDRIVE"] = drive
        environment["HOMEPATH"] = str(home)[len(drive):]
    else:
        environment["HOMEDRIVE"] = str(root / "home-drive")
        environment["HOMEPATH"] = str(root / "home-path")
        Path(environment["HOMEDRIVE"]).mkdir(parents=True, exist_ok=True)
        Path(environment["HOMEPATH"]).mkdir(parents=True, exist_ok=True)
    return environment


def assert_account_command_roots(commands: object, account_root: Path) -> None:
    """Reject any optional account dispatcher escaping its disposable account."""
    if not isinstance(commands, dict):
        raise RuntimeError("account command receipt is invalid")
    account_root = account_root.resolve(strict=True)
    expected = set(ACCOUNT_COMMAND_NAMES)
    platform_prerequisites = set(PLATFORM_PREREQUISITE_COMMAND_NAMES)
    if os.name == "nt" and "python" in commands:
        expected.remove("python3")
        expected.add("python")
        platform_prerequisites.remove("python3")
        platform_prerequisites.add("python")
    if set(commands) != expected:
        raise RuntimeError("account command receipt executables are incomplete")
    for name, value in commands.items():
        if name not in expected or not isinstance(value, str):
            raise RuntimeError("unknown account command")
        command = Path(value)
        if not command.is_absolute():
            raise RuntimeError("account command is not absolute")
        try:
            resolved = command.resolve(strict=True)
        except OSError as error:
            raise RuntimeError("account command is not a file") from error
        if not resolved.is_file():
            raise RuntimeError("account command is not a file")
        if name in OPTIONAL_ACCOUNT_COMMAND_NAMES:
            if not resolved.is_relative_to(account_root):
                raise RuntimeError(f"account command outside isolated account: {name}")
        elif name not in platform_prerequisites:
            raise RuntimeError("unknown account command")


def known_base_component_statuses(base_sha: str) -> dict[str, dict[str, object]]:
    """Return the entire observed POSIX legacy-wrapper incompatibility shape."""
    components = {
        name: {"status": "healthy"}
        for name in (
            "core", "graphify", "hooks", "mempalace", "playbooks", "plugins",
            "projection-policy", "retrieval-config", "roles", "skills", "tools",
        )
    }
    components.update({
        "maven-tools-mcp": {"status": "absent"},
        "memory": {"status": "healthy"},
        "mcps": {"status": "healthy"},
    })
    dependency_components = {
        name: {
            "status": "healthy", "action": action, "probe": "passed", "detail": "passed",
        }
        for name, action in {
            "uv": "installed", "python": "upgraded", "node": "installed", "java": "upgraded",
            "mempalace": "installed", "graphify": "installed", "memory": "installed",
            "context7": "installed",
        }.items()
    }
    status = {
        "status": "healthy", "commit": base_sha, "kernel": "healthy", "hosts": "healthy",
        "dependencies": "healthy", "components": components,
        "dependencyComponents": dependency_components,
    }
    doctor_components = json.loads(json.dumps(components))
    doctor_components["memory"] = {"status": "recovery-required"}
    doctor_components["mcps"] = {"status": "recovery-required"}
    return {
        "status": status,
        "doctor": {
            **status,
            "status": "recovery-required",
            "components": doctor_components,
        },
    }


def exact_base_compatibility_transition(
    error: Exception, base_sha: str, *, windows: bool
) -> str:
    """Allow only the observed post-provision doctor incompatibility."""
    if (
        not isinstance(error, AcceptanceCommandFailure)
        or base_sha != KNOWN_BASE_SHA
        or windows
        or error.returncode != 1
        or tuple(error.command) != tuple(public_wrapper_command(base_sha, windows=windows))
    ):
        raise error
    if str(error) != f"command failed (1): {POSIX_BASE_FAILURE_DETAIL}":
        raise error
    statuses = getattr(error, "component_statuses", None)
    if statuses != known_base_component_statuses(base_sha):
        raise error
    return "post-provision-doctor"


def offline_environment(
    base: dict[str, str] | None = None, *, block_path: bool = False
) -> dict[str, str]:
    environment = clean_environment(base)
    if block_path:
        environment["PATH"] = ""
    environment.update(
        {
            "HTTP_PROXY": "http://127.0.0.1:9",
            "HTTPS_PROXY": "http://127.0.0.1:9",
            "ALL_PROXY": "http://127.0.0.1:9",
            "NO_PROXY": "",
            "PIP_NO_INDEX": "1",
            "NPM_CONFIG_OFFLINE": "true",
            "UV_OFFLINE": "1",
            "PYTHONDONTWRITEBYTECODE": "1",
        }
    )
    return environment


def _sanitize_http_url(token: str) -> str:
    trailing = ""
    while token and token[-1] in ".,;)}'":
        trailing = token[-1] + trailing
        token = token[:-1]
    try:
        parsed = urllib.parse.urlsplit(token)
    except ValueError:
        return ("<url>" if "@" in token else token) + trailing
    if parsed.scheme.casefold() not in {"http", "https"} or "@" not in parsed.netloc:
        return token + trailing
    host_port = parsed.netloc.rsplit("@", 1)[1]
    sanitized = urllib.parse.urlunsplit(
        (parsed.scheme, f"<redacted>@{host_port}", parsed.path, parsed.query, parsed.fragment)
    )
    return sanitized + trailing


def _protect_http_urls(text: str) -> tuple[str, list[str]]:
    protected: list[str] = []
    output: list[str] = []
    cursor = 0
    while match := URL_START.search(text, cursor):
        start = match.start()
        end = match.end()
        while end < len(text) and text[end] not in "\r\n\t <>\"":
            end += 1
        output.append(text[cursor:start])
        marker = f"\x00chaos-url-{len(protected)}\x00"
        output.append(marker)
        protected.append(_sanitize_http_url(text[start:end]))
        cursor = end
    output.append(text[cursor:])
    return "".join(output), protected


def _redact_absolute_paths(text: str) -> str:
    output: list[str] = []
    cursor = 0
    while match := ABSOLUTE_ROOT.search(text, cursor):
        start = match.start()
        end = match.end()
        quote = text[start - 1] if start and text[start - 1] in "\"'" else None
        if quote is not None:
            closing = text.find(quote, end)
            end = len(text) if closing < 0 else closing
        else:
            while end < len(text) and text[end] not in PATH_DELIMITERS:
                end += 1
        output.extend((text[cursor:start], "<path>"))
        cursor = end
    output.append(text[cursor:])
    return "".join(output)


def sanitize(value: object) -> str:
    text = str(value)[:SANITIZER_INPUT_LIMIT].replace("\x00", "<nul>")
    text, urls = _protect_http_urls(text)
    text = _redact_absolute_paths(text)
    for index, url in enumerate(urls):
        text = text.replace(f"\x00chaos-url-{index}\x00", url)
    if len(text) <= SANITIZER_OUTPUT_LIMIT:
        return text
    tail_size = SANITIZER_OUTPUT_LIMIT // 2
    head_size = SANITIZER_OUTPUT_LIMIT - tail_size - len(SANITIZER_TRUNCATION_MARKER)
    return text[:head_size] + SANITIZER_TRUNCATION_MARKER + text[-tail_size:]


def sanitized_command(value: object) -> list[str] | None:
    if not isinstance(value, (list, tuple)) or any(not isinstance(part, str) for part in value):
        return None
    return [sanitize(part) for part in value]


def sanitized_component_statuses(value: object) -> dict[str, dict[str, object]] | None:
    """Keep only health labels; diagnostic details can contain private user content."""
    if not isinstance(value, dict):
        return None
    result: dict[str, dict[str, object]] = {}
    for command, report in value.items():
        if not isinstance(command, str) or not isinstance(report, dict):
            return None
        summary: dict[str, object] = {}
        for name in STATUS_SUMMARY_FIELDS:
            status = report.get(name)
            if isinstance(status, str):
                summary[name] = sanitize(status)
        for name in ("components", "dependencyComponents"):
            components = report.get(name)
            if components is None:
                continue
            if not isinstance(components, dict):
                return None
            statuses: dict[str, object] = {}
            fields = (
                HOST_COMPONENT_FIELDS
                if name == "components"
                else DEPENDENCY_COMPONENT_FIELDS
            )
            for component, status in components.items():
                if not isinstance(component, str):
                    return None
                if isinstance(status, str):
                    statuses[sanitize(component)] = sanitize(status)
                    continue
                if not isinstance(status, dict):
                    return None
                detail = {
                    field: sanitize(status[field])
                    for field in fields
                    if isinstance(status.get(field), str)
                }
                if not detail:
                    return None
                statuses[sanitize(component)] = detail
            summary[name] = statuses
        result[sanitize(command)] = summary
    return result


def failure_evidence(error: Exception) -> dict[str, object]:
    phase: str | None = None
    cause = error
    if isinstance(error, AcceptancePhaseFailure):
        phase = error.phase
        cause = error.cause
    result: dict[str, object] = {
        "type": type(cause).__name__,
        "detail": sanitize(cause),
    }
    if phase is not None:
        result["phase"] = phase
    if command := sanitized_command(getattr(cause, "command", None)):
        result["command"] = command
    if statuses := sanitized_component_statuses(
        getattr(cause, "component_statuses", None)
    ):
        result["componentStatuses"] = statuses
    return result


def installer_failure_detail(value: str) -> str:
    headline = next(
        (line.strip() for line in value.splitlines() if "CE-INSTALL-" in line),
        None,
    )
    if headline is None:
        return value
    fields: list[str] = []
    for token in value.split():
        if "/issues/new?" not in token:
            continue
        query = urllib.parse.parse_qs(urllib.parse.urlsplit(token).query)
        for key, label in (("failed_phase", "failed phase"), ("unhealthy", "unhealthy")):
            if query.get(key):
                fields.append(f"{label}: {query[key][0]}")
        break
    return "; ".join((headline, *fields))


def run_checked(
    command: list[str],
    *,
    cwd: Path,
    environment: dict[str, str] | None = None,
    timeout: int = PHASE_TIMEOUT_SECONDS,
) -> subprocess.CompletedProcess[str]:
    result = subprocess.run(  # nosec B603
        command,
        cwd=cwd,
        env=environment or clean_environment(),
        text=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        timeout=timeout,
        check=False,
    )
    if result.returncode:
        detail = result.stderr.strip() or result.stdout.strip() or "no process output"
        detail = installer_failure_detail(detail)
        raise AcceptanceCommandFailure(command, result.returncode, detail)
    return result


def read_json(path: Path) -> dict[str, object]:
    value = json.loads(path.read_text(encoding="utf-8"))
    if not isinstance(value, dict):
        raise RuntimeError(f"expected JSON object: {path.name}")
    return value


def managed_python_version(installed: Path) -> str:
    dependencies = read_json(installed / "dependencies.json")
    runtimes = dependencies.get("runtimes")
    python = runtimes.get("python") if isinstance(runtimes, dict) else None
    version = python.get("version") if isinstance(python, dict) else None
    if not isinstance(version, str) or re.fullmatch(r"\d+\.\d+", version) is None:
        raise RuntimeError("installed managed Python version is invalid")
    return version


def stage_source(source: Path, destination: Path) -> Path:
    shutil.copytree(
        source,
        destination,
        ignore=shutil.ignore_patterns("__pycache__", "*.pyc"),
    )
    return destination


def assert_exact_base_source(source: Path, expected_files: object, *, source_files) -> None:
    """Require the offline base source to match every authenticated payload hash."""
    if (
        not source.is_dir()
        or source.is_symlink()
        or (source / "manifest.json").exists()
        or not isinstance(expected_files, dict)
        or any(
            not isinstance(relative, str)
            or not isinstance(digest, str)
            or re.fullmatch(r"[0-9a-f]{64}", digest) is None
            for relative, digest in expected_files.items()
        )
    ):
        raise RuntimeError("exact base manifest is invalid")
    source = source.resolve(strict=True)
    actual: dict[str, str] = {}
    for path in source_files(source):
        path = Path(path)
        try:
            relative = path.relative_to(source).as_posix()
        except ValueError as error:
            raise RuntimeError("exact base source escapes its root") from error
        if path.is_symlink() or not path.is_file():
            raise RuntimeError("exact base source contains an invalid payload entry")
        actual[relative] = hashlib.sha256(path.read_bytes()).hexdigest()
    if actual != expected_files:
        raise RuntimeError("exact base manifest does not match its source payload")


def _extract_git_archive(payload: bytes, destination: Path) -> None:
    """Extract only regular Git-tree entries into a fresh disposable directory."""
    destination.mkdir()
    root = destination.resolve(strict=True)
    with tarfile.open(fileobj=io.BytesIO(payload), mode="r:") as archive:
        for member in archive.getmembers():
            relative = Path(member.name)
            target = destination / relative
            if (
                relative.is_absolute()
                or ".." in relative.parts
                or not target.resolve().is_relative_to(root)
                or member.issym()
                or member.islnk()
            ):
                raise RuntimeError("exact base Git archive is unsafe")
            if member.isdir():
                target.mkdir(exist_ok=True)
                continue
            if not member.isfile():
                raise RuntimeError("exact base Git archive is unsafe")
            target.parent.mkdir(parents=True, exist_ok=True)
            stream = archive.extractfile(member)
            if stream is None:
                raise RuntimeError("exact base Git archive is unreadable")
            with target.open("xb") as output:
                shutil.copyfileobj(stream, output)


def fetch_exact_base_source(
    root: Path,
    base_sha: str,
    expected_manifest: object,
    installed_base: Path,
) -> Path:
    """Fetch one immutable base Git tree and authenticate its selected payload."""
    if COMMIT.fullmatch(base_sha) is None or not isinstance(expected_manifest, dict):
        raise RuntimeError("exact base source request is invalid")
    expected_files = expected_manifest.get("files")
    distribution = expected_manifest.get("distribution")
    distribution_id = distribution.get("id") if isinstance(distribution, dict) else None
    if not isinstance(distribution_id, str):
        raise RuntimeError("exact base manifest is invalid")
    git = shutil.which("git")
    if git is None:
        raise RuntimeError("git is required to fetch the exact base source")
    repository = root / "immutable-base-git"
    environment = download_environment()
    run_checked([git, "init", str(repository)], cwd=root, environment=environment)
    run_checked(
        [git, "-C", str(repository), "remote", "add", "origin", "https://github.com/ShaftHQ/SHAFT_ENGINE.git"],
        cwd=root,
        environment=environment,
    )
    run_checked(
        [git, "-C", str(repository), "fetch", "--no-tags", "--depth=1", "origin", base_sha],
        cwd=root,
        environment=environment,
    )
    actual_sha = run_checked(
        [git, "-C", str(repository), "rev-parse", "FETCH_HEAD^{commit}"],
        cwd=root,
        environment=environment,
    ).stdout.strip()
    if actual_sha != base_sha:
        raise RuntimeError("exact base Git commit authentication failed")
    archive = subprocess.run(  # nosec B603 - fixed Git archive for a validated SHA.
        [git, "-C", str(repository), "archive", "--format=tar", base_sha, "chaos-engine"],
        cwd=root,
        env=environment,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        timeout=PHASE_TIMEOUT_SECONDS,
        check=False,
    )
    if archive.returncode:
        raise RuntimeError("exact base Git archive could not be created")
    archive_root = root / "immutable-base-source"
    _extract_git_archive(archive.stdout, archive_root)
    source = archive_root / "chaos-engine"
    base_installer = load_source_controller(installed_base, "install")
    assert_exact_base_source(
        source,
        expected_files,
        source_files=lambda path: base_installer.source_files(path, distribution_id),
    )
    return source


def load_source_controller(source: Path, name: str):
    specification = importlib.util.spec_from_file_location(
        f"chaos_engine_acceptance_{name}", source / f"{name}.py"
    )
    if specification is None or specification.loader is None:
        raise RuntimeError(f"base {name} controller could not be loaded")
    module = importlib.util.module_from_spec(specification)
    specification.loader.exec_module(module)
    return module


def account_receipt_commands(project: Path, account_root: Path) -> dict[str, str]:
    receipt = read_json(project / ".chaos-engine-dependencies.json")
    commands = receipt.get("commands")
    assert_account_command_roots(commands, account_root)
    return dict(commands)


def assert_account_search_paths(
    base_project: Path, candidate_source: Path, environment: dict[str, str]
) -> None:
    """Prove both public base and candidate discovery see isolated executable bins."""
    expected = {
        environment["UV_TOOL_BIN_DIR"],
        str(Path(environment["NPM_CONFIG_PREFIX"]) / (
            "Scripts" if os.name == "nt" else "bin"
        )),
    }
    previous = os.environ.copy()
    try:
        os.environ.clear()
        os.environ.update(environment)
        for source in (base_project / ".chaos-engine", candidate_source):
            controller = load_source_controller(source, "dependencies")
            search = set(controller._account_search_path().split(os.pathsep))
            if not expected <= search:
                raise RuntimeError("account dependency search path omits configured executable bins")
    finally:
        os.environ.clear()
        os.environ.update(previous)


def capture_files(root: Path, relative: str) -> dict[str, bytes]:
    path = root / relative
    if not path.exists():
        return {}
    if path.is_file():
        return {relative: path.read_bytes()}
    return {
        child.relative_to(root).as_posix(): child.read_bytes()
        for child in sorted(path.rglob("*")) if child.is_file()
    }


def snapshot_base_state(project: Path) -> dict[str, object]:
    """Capture only immutable-base artifacts that the candidate must restore exactly."""
    installed = project / ".chaos-engine"
    manifest = (installed / "manifest.json").read_bytes()
    host_receipt = (project / ".chaos-engine-hosts.json").read_bytes()
    account_receipt = project / ".chaos-engine-dependencies.json"
    if not account_receipt.is_file() or account_receipt.is_symlink():
        raise RuntimeError("base account dependency receipt is invalid")
    try:
        receipt = json.loads(host_receipt.decode("utf-8"))
    except (UnicodeDecodeError, json.JSONDecodeError) as error:
        raise RuntimeError("base host receipt is invalid") from error
    after = receipt.get("after") if isinstance(receipt, dict) else None
    if not isinstance(after, dict):
        raise RuntimeError("base host receipt has no managed images")
    managed: dict[str, bytes | None] = {}
    for relative, encoded in after.items():
        if not isinstance(relative, str) or encoded is not None and not isinstance(encoded, str):
            raise RuntimeError("base host receipt has invalid managed image")
        expected = None if encoded is None else base64.b64decode(encoded, validate=True)
        current = project / relative
        actual = current.read_bytes() if current.is_file() else None
        if actual != expected:
            raise RuntimeError("base managed adapter does not match its receipt")
        managed[relative] = expected
    sentinels = {
        path.relative_to(project).as_posix(): path.read_bytes()
        for path in sorted(project.rglob("*sentinel*")) if path.is_file()
    }
    return {
        "manifest": manifest,
        "hostReceipt": host_receipt,
        "accountReceipt": account_receipt.read_bytes(),
        "managed": managed,
        "configuration": {
            **capture_files(project, "mempalace.yaml"),
            **capture_files(project, ".chaos-engine-state/mempalace"),
        },
        "sentinels": sentinels,
    }


def assert_base_state_restored(project: Path, snapshot: dict[str, object]) -> None:
    installed = project / ".chaos-engine"
    if (installed / "manifest.json").read_bytes() != snapshot["manifest"]:
        raise RuntimeError("rollback did not restore the exact base manifest")
    if (project / ".chaos-engine-hosts.json").read_bytes() != snapshot["hostReceipt"]:
        raise RuntimeError("rollback did not restore the exact base host receipt")
    if (project / ".chaos-engine-dependencies.json").read_bytes() != snapshot["accountReceipt"]:
        raise RuntimeError("rollback did not restore the exact base dependency receipt")
    managed = snapshot.get("managed")
    if not isinstance(managed, dict):
        raise RuntimeError("base snapshot is invalid")
    for relative, expected in managed.items():
        path = project / str(relative)
        actual = path.read_bytes() if path.is_file() else None
        if actual != expected:
            raise RuntimeError(f"rollback did not restore managed adapter: {relative}")
    configuration = snapshot.get("configuration")
    sentinels = snapshot.get("sentinels")
    if (
        not isinstance(configuration, dict)
        or not isinstance(sentinels, dict)
        or {
            **capture_files(project, "mempalace.yaml"),
            **capture_files(project, ".chaos-engine-state/mempalace"),
        } != configuration
        or {
            path.relative_to(project).as_posix(): path.read_bytes()
            for path in sorted(project.rglob("*sentinel*")) if path.is_file()
        } != sentinels
    ):
        raise RuntimeError("rollback rewrote base configuration or sentinel data")


def raw_wrapper_url(commit: str, *, windows: bool) -> str:
    if COMMIT.fullmatch(commit) is None:
        raise ValueError("candidate SHA must be 40 lowercase hexadecimal characters")
    suffix = "install.ps1" if windows else "install.sh"
    return (
        "https://raw.githubusercontent.com/ShaftHQ/SHAFT_ENGINE/"
        f"{commit}/chaos-engine/{suffix}"
    )


def public_wrapper_command(commit: str, *, windows: bool) -> list[str]:
    url = raw_wrapper_url(commit, windows=windows)
    if windows:
        shell = shutil.which("pwsh") or shutil.which("powershell") or "powershell"
        return [shell, "-NoProfile", "-Command", f'irm "{url}" | iex']
    shell = shutil.which("bash") or "/bin/bash"
    command = f'curl -fsSL "{url}" | bash -s -- "{url}"'
    return [shell, "-c", command]


OFFLINE_RERUN = """
import importlib.util, json, pathlib, sys
project, source = map(pathlib.Path, sys.argv[1:3])
installed = project / '.chaos-engine'
spec = importlib.util.spec_from_file_location('chaos_engine_offline_install', installed / 'install.py')
module = importlib.util.module_from_spec(spec)
spec.loader.exec_module(module)
manifest = json.loads((installed / 'manifest.json').read_text(encoding='utf-8'))
module.install(
    project, source, manifest['source']['commit'],
    source_record=manifest['source'], distribution=manifest['distribution']['id'],
)
"""


def run_offline_rerun(
    project: Path, source: Path, *, environment: dict[str, str] | None = None
) -> None:
    child_environment = (
        offline_environment(block_path=True)
        if environment is None
        else offline_environment(environment, block_path=True)
    )
    run_checked(
        [sys.executable, "-c", OFFLINE_RERUN, str(project), str(source)],
        cwd=project,
        environment=child_environment,
        timeout=180,
    )


def run_public_wrapper(
    commit: str, project: Path, *, require_current_action: bool = True,
    environment: dict[str, str] | None = None,
) -> None:
    result = run_checked(
        public_wrapper_command(commit, windows=os.name == "nt"),
        cwd=project,
        environment=environment or wrapper_environment(),
    )
    if not (project / ".chaos-engine/install.py").is_file():
        raise RuntimeError("public wrapper did not create the installation tree")
    if "Installing ChaosEngine" not in result.stderr:
        raise RuntimeError("public wrapper returned without durable installer progress")
    if require_current_action and "START " not in result.stderr and "Elapsed " not in result.stderr:
        raise RuntimeError("candidate wrapper omitted installer progress")
    payload = json.loads(result.stdout)
    if payload.get("status") != "installed":
        raise RuntimeError("public wrapper did not return an installed result")
    if not isinstance(payload.get("clients"), dict):
        raise RuntimeError("public wrapper did not report detected client activation")


def probe_mcp(
    command: list[str],
    project: Path,
    *,
    environment: dict[str, str] | None = None,
    base_environment: dict[str, str] | None = None,
    popen=subprocess.Popen,
) -> None:
    requests = (
        {
            "jsonrpc": "2.0",
            "id": 1,
            "method": "initialize",
            "params": {
                "protocolVersion": MCP_PROTOCOL_VERSION,
                "capabilities": {},
                "clientInfo": {"name": "chaos-engine-acceptance", "version": "1"},
            },
        },
        {"jsonrpc": "2.0", "method": "notifications/initialized", "params": {}},
        {"jsonrpc": "2.0", "id": 2, "method": "tools/list", "params": {}},
    )
    request = "".join(json.dumps(item) + "\n" for item in requests)
    process = popen(  # nosec B603
        command,
        cwd=project,
        env={
            **(base_environment or clean_environment()),
            "PYTHONDONTWRITEBYTECODE": "1",
            **(environment or {}),
        },
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
    )
    try:
        stdout, stderr = process.communicate(
            request, timeout=MCP_START_TIMEOUT_SECONDS
        )
    except subprocess.TimeoutExpired:
        process.terminate()
        try:
            process.communicate(timeout=5)
        except subprocess.TimeoutExpired:
            process.kill()
            process.communicate(timeout=5)
        raise RuntimeError("MCP initialize timed out")
    try:
        responses = [
            json.loads(line) for line in stdout.splitlines() if line.strip().startswith("{")
        ]
    except (IndexError, json.JSONDecodeError) as error:
        raise RuntimeError(
            f"MCP connection closed during initialize: {sanitize(stderr or stdout)}"
        ) from error
    response = next(
        (item for item in responses if item.get("id") == 1),
        None,
    )
    if not isinstance(response, dict):
        raise RuntimeError(
            f"MCP connection closed during initialize: {sanitize(stderr or stdout)}"
        )
    result = response.get("result")
    server = result.get("serverInfo") if isinstance(result, dict) else None
    if (
        process.returncode
        or response.get("jsonrpc") != "2.0"
        or type(response.get("id")) is not int
        or response.get("id") != 1
        or not isinstance(result, dict)
        or result.get("protocolVersion") != MCP_PROTOCOL_VERSION
        or not isinstance(result.get("capabilities"), dict)
        or not isinstance(server, dict)
        or not isinstance(server.get("name"), str)
        or not isinstance(server.get("version"), str)
    ):
        raise RuntimeError(
            f"MCP initialize failed: {sanitize(stderr or stdout)}"
        )

    listed = next((item for item in responses if item.get("id") == 2), None)
    tools = listed.get("result", {}).get("tools") if isinstance(listed, dict) else None
    if not isinstance(tools, list):
        raise RuntimeError(f"MCP tools/list failed: {sanitize(stderr or stdout)}")


def probe_project_mcps(
    tool: Path, project: Path, *, base_environment: dict[str, str] | None = None
) -> None:
    commands = (
        [sys.executable, str(tool), "memory-mcp"],
        [sys.executable, str(tool), "mempalace-mcp"],
    )
    for command in commands:
        probe_mcp(
            command,
            project,
            environment={"MEMPALACE_BACKEND": "sqlite_exact"},
            base_environment=base_environment,
        )


def generated_mcp_commands(
    project: Path, *, windows: bool
) -> list[tuple[str, list[str], Path, dict[str, str]]]:
    """Read generated project commands, including platform-specific launch fields."""
    servers = read_json(project / ".mcp.json").get("mcpServers")
    if not isinstance(servers, dict):
        raise RuntimeError("generated MCP configuration is missing servers")
    command_key = "commandWindows" if windows else "command"
    arguments_key = "argsWindows" if windows else "args"
    commands: list[tuple[str, list[str], Path, dict[str, str]]] = []
    for name in ("chaosengine-memory", "chaosengine-mempalace"):
        server = servers.get(name)
        if not isinstance(server, dict):
            raise RuntimeError(f"generated MCP configuration is missing {name}")
        executable = server.get(command_key, server.get("command"))
        arguments = server.get(arguments_key, server.get("args"))
        cwd = server.get("cwd", ".")
        environment = server.get("env", {})
        if (
            not isinstance(executable, str)
            or not isinstance(arguments, list)
            or any(not isinstance(argument, str) for argument in arguments)
            or not isinstance(cwd, str)
            or not isinstance(environment, dict)
            or any(not isinstance(key, str) or not isinstance(value, str)
                   for key, value in environment.items())
        ):
            raise RuntimeError(f"generated MCP command is invalid: {name}")
        working_directory = (project / cwd).resolve()
        if not working_directory.is_relative_to(project.resolve()):
            raise RuntimeError(f"generated MCP working directory escapes project: {name}")
        commands.append((name, [executable, *arguments], working_directory, environment))
    return commands


def probe_generated_mcps(
    project: Path, *, base_environment: dict[str, str] | None = None
) -> None:
    # Parse both platform forms before executing current host's generated command.
    generated_mcp_commands(project, windows=False)
    commands = generated_mcp_commands(project, windows=os.name == "nt")
    for _name, command, cwd, environment in commands:
        probe_mcp(
            command, cwd, environment=environment, base_environment=base_environment
        )


def verify_phase(project: Path, expected_commit: str) -> dict[str, object]:
    installed = project / ".chaos-engine"
    python_version = managed_python_version(installed)
    status = json.loads(
        run_checked(
            [
                sys.executable,
                str(installed / "install.py"),
                "status",
                "--project",
                str(project),
                "--json",
            ],
            cwd=project,
        ).stdout
    )
    if status.get("status") != "healthy" or status.get("commit") != expected_commit:
        raise RuntimeError("status did not report expected healthy commit")
    doctor = json.loads(
        run_checked(
            [
                sys.executable,
                str(installed / "install.py"),
                "doctor",
                "--project",
                str(project),
                "--json",
            ],
            cwd=project,
        ).stdout
    )
    if doctor.get("status") != "healthy" or doctor.get("commit") != expected_commit:
        raise RuntimeError("doctor did not report expected healthy commit")

    tool = installed / "tool.py"
    dispatches: dict[str, str] = {}
    for name, arguments in PROBES.items():
        run_checked([sys.executable, str(tool), name, *arguments], cwd=project, timeout=120)
        dispatches[name] = "pass"
    probe_project_mcps(tool, project)
    dispatches.update({"memory-mcp": "pass", "mempalace-mcp": "pass"})

    pointer_path = project / ".chaos-engine-runtime-current.json"
    if pointer_path.stat().st_size > 16 * 1024:
        raise RuntimeError("active pointer exceeded control-file bound")
    pointer = read_json(pointer_path)
    active = pointer.get("active")
    previous = pointer.get("previous")
    if not isinstance(active, dict) or HEX_ID.fullmatch(str(active.get("generationId", ""))) is None:
        raise RuntimeError("active generation identifier is invalid")
    if previous is not None and (
        not isinstance(previous, dict)
        or HEX_ID.fullmatch(str(previous.get("generationId", ""))) is None
    ):
        raise RuntimeError("previous generation identifier is invalid")

    generations = project / ".chaos-engine-runtime-generations"
    generation_names = sorted(path.name for path in generations.iterdir())
    if len(generation_names) > 3 or any(HEX_ID.fullmatch(name) is None for name in generation_names):
        raise RuntimeError("generation retention bound is invalid")
    active_root = generations / str(active["generationId"])
    receipt = read_json(active_root / "receipt.json")
    if set(receipt.get("tools", {})) != set(TOOLS):
        raise RuntimeError("active generation dispatch set is incomplete")
    for name in ("graphify", "mempalace"):
        dispatch = receipt["tools"][name]["dispatch"]
        version = run_checked(
            [str(active_root / dispatch["interpreter"]), "--version"],
            cwd=project,
            timeout=60,
        )
        output = f"{version.stdout}\n{version.stderr}"
        if f"Python {python_version}." not in output:
            raise RuntimeError(f"{name} is not using managed Python {python_version}")
    if any((active_root / name).exists() for name in ("uv-cache", "npm-cache", ".cache")):
        raise RuntimeError("transaction cache leaked into immutable generation")
    transactions = project / ".chaos-engine-runtime-transactions"
    if transactions.exists() and any(transactions.iterdir()):
        raise RuntimeError("transaction state remains after activation")
    return {
        "status": "healthy",
        "dispatches": dispatches,
        "active": str(active["generationId"]),
        "previous": None if previous is None else str(previous["generationId"]),
        "generationCount": len(generation_names),
        "managedPython": python_version,
        "cacheState": "absent",
    }


def component_status_summary(result: object) -> dict[str, object]:
    """Extract schema-stable health labels without retaining host diagnostics."""
    if not isinstance(result, dict):
        return {}
    summary: dict[str, object] = {
        name: result[name] for name in ("status", "commit")
        if isinstance(result.get(name), str)
    }
    for name in ("kernel", "hosts", "dependencies"):
        value = result.get(name)
        if isinstance(value, dict) and isinstance(value.get("status"), str):
            summary[name] = value["status"]
    components = result.get("components")
    if isinstance(components, dict):
        summary["components"] = {
            name: {
                field: record[field]
                for field in HOST_COMPONENT_FIELDS
                if isinstance(record.get(field), str)
            }
            for name, record in components.items()
            if isinstance(name, str)
            and isinstance(record, dict)
            and isinstance(record.get("status"), str)
        }
    dependencies = result.get("dependencies")
    dependency_components = dependencies.get("components") if isinstance(dependencies, dict) else None
    if isinstance(dependency_components, dict):
        summary["dependencyComponents"] = {
            name: {
                field: record[field]
                for field in DEPENDENCY_COMPONENT_FIELDS
                if isinstance(record.get(field), str)
            }
            for name, record in dependency_components.items()
            if isinstance(name, str)
            and isinstance(record, dict)
            and isinstance(record.get("status"), str)
        }
    return summary


def read_only_account_statuses(
    project: Path, *, environment: dict[str, str] | None = None
) -> dict[str, dict[str, object]]:
    """Collect status labels after a wrapper failure without replacing that failure."""
    installed = project / ".chaos-engine"
    install = installed / "install.py"
    if not install.is_file():
        return {}
    reports: dict[str, dict[str, object]] = {}
    for command in ("status", "doctor"):
        try:
            completed = run_checked(
                [
                    sys.executable, str(install), command, "--project", str(project),
                    "--json",
                ],
                cwd=project,
                environment=environment,
            )
            payload = json.loads(completed.stdout)
        except (AcceptanceCommandFailure, json.JSONDecodeError):
            continue
        summary = component_status_summary(payload)
        if summary:
            reports[command] = summary
    return reports


def run_public_wrapper_with_diagnostics(
    commit: str,
    project: Path,
    *,
    require_current_action: bool = True,
    environment: dict[str, str] | None = None,
) -> None:
    """Preserve the wrapper failure while recording its installed read-only state."""
    child_environment = environment or wrapper_environment()
    try:
        run_public_wrapper(
            commit,
            project,
            require_current_action=require_current_action,
            environment=child_environment,
        )
    except Exception as error:
        statuses = read_only_account_statuses(project, environment=child_environment)
        if statuses:
            error.component_statuses = statuses
        raise


def verify_account_phase(
    project: Path, expected_commit: str, *, probe_generated: bool = True,
    environment: dict[str, str] | None = None,
) -> dict[str, object]:
    installed = project / ".chaos-engine"
    environment = environment or wrapper_environment()
    status_reports: dict[str, dict[str, object]] = {}
    for command in ("status", "doctor"):
        command_line = [
            sys.executable,
            str(installed / "install.py"),
            command,
            "--project",
            str(project),
            "--json",
        ]
        result = json.loads(
            run_checked(
                command_line,
                cwd=project,
                environment=environment,
            ).stdout
        )
        summary = component_status_summary(result)
        status_reports[command] = summary
        if result.get("status") != "healthy" or result.get("commit") != expected_commit:
            error = RuntimeError(
                f"{command} did not report expected healthy account setup: "
                f"{sanitize(json.dumps(summary, sort_keys=True))}"
            )
            error.command = command_line
            error.component_statuses = {command: summary}
            raise error
    receipt = read_json(project / ".chaos-engine-dependencies.json")
    components = receipt.get("components")
    commands = receipt.get("commands")
    if (
        receipt.get("schemaVersion") != 2
        or not isinstance(components, dict)
        or not isinstance(commands, dict)
        or any(record.get("status") != "healthy" for record in components.values())
        or any(not Path(command).is_absolute() for command in commands.values())
    ):
        raise RuntimeError("account dependency receipt is incomplete")
    tool = installed / "tool.py"
    dispatches: dict[str, str] = {}
    for name, arguments in PROBES.items():
        run_checked(
            [sys.executable, str(tool), name, *arguments],
            cwd=project,
            environment=environment,
            timeout=120,
        )
        dispatches[name] = "pass"
    if probe_generated:
        probe_project_mcps(tool, project, base_environment=environment)
        probe_generated_mcps(project, base_environment=environment)
        dispatches.update({
            "project-memory-mcp": "pass", "project-mempalace-mcp": "pass",
            "generated-memory-mcp": "pass", "generated-mempalace-mcp": "pass",
        })
    return {
        "status": "healthy",
        "dispatches": dispatches,
        "schemaVersion": 2,
        "actions": {
            name: record.get("action") for name, record in components.items()
        },
        "versions": {
            name: {
                "installed": record.get("installedVersion"),
                "resolved": record.get("resolvedVersion"),
            }
            for name, record in components.items()
        },
        "verification": status_reports,
    }


def record_phase(
    evidence: dict[str, object], name: str, operation
) -> dict[str, object]:
    started = time.monotonic()
    try:
        checks = operation()
    except Exception as error:
        phase_error = AcceptancePhaseFailure(name, error)
        evidence["phases"].append({
            "name": name,
            "status": "fail",
            "durationSeconds": round(time.monotonic() - started, 3),
            "failure": failure_evidence(phase_error),
        })
        raise phase_error from error
    evidence["phases"].append({
        "name": name,
        "status": "pass",
        "durationSeconds": round(time.monotonic() - started, 3),
        "checks": checks,
    })
    return checks


def project_snapshot(project: Path) -> dict[str, bytes]:
    """Read disposable-project state for an exact offline no-mutation proof."""
    return {
        path.relative_to(project).as_posix(): path.read_bytes()
        for path in sorted(project.rglob("*"))
        if path.is_file()
    }


def assert_local_mempalace(project: Path) -> None:
    palace = project / ".chaos-engine-state/mempalace"
    if not (palace / "sqlite_exact.sqlite3").is_file():
        raise RuntimeError("candidate did not initialize the exact local MemPalace")
    if (palace / ".mined").read_bytes() not in {b"current\n", b"current\r\n"}:
        raise RuntimeError("candidate MemPalace mine marker is invalid")


def assert_single_generated_mempalace(project: Path) -> None:
    servers = read_json(project / ".mcp.json").get("mcpServers")
    if not isinstance(servers, dict):
        raise RuntimeError("generated MCP configuration is missing servers")
    registrations = [
        name for name in servers if "mempalace" in name.casefold()
    ]
    if registrations != ["chaosengine-mempalace"]:
        raise RuntimeError("generated MCP configuration has duplicate MemPalace servers")
    for _name, command, _cwd, _environment in generated_mcp_commands(
        project, windows=os.name == "nt"
    ):
        if "--palace" in command:
            raise RuntimeError("generated MCP configuration supplied MemPalace storage")


def run_acceptance(
    source: Path,
    evidence: dict[str, object],
    *,
    candidate_sha: str,
    base_sha: str,
) -> None:
    source = source.resolve()
    with tempfile.TemporaryDirectory(prefix="chaos-engine-live-") as temporary:
        root = Path(temporary)
        account_root = root / "base isolated account"
        account_environment = isolated_account_environment(account_root)
        fresh_account_root = root / "fresh isolated account"
        fresh_environment = isolated_account_environment(fresh_account_root)
        base_project = root / "base consumer with spaces Ω"
        fresh_project = root / "fresh consumer with spaces Ω"
        base_project.mkdir()
        fresh_project.mkdir()
        user_config = (
            "wing: acceptance\n"
            "rooms:\n  - name: general\n    description: Acceptance project\n"
            "exclude_patterns:\n  - .chaos-engine-state/**\n"
        ).encode()
        base_project.joinpath("mempalace.yaml").write_bytes(user_config)
        base_sentinel = base_project / "user-sentinel.txt"
        base_sentinel.write_bytes(b"preserve base user data\n")
        fresh_sentinel = fresh_project / "user-sentinel.txt"
        fresh_sentinel.write_bytes(b"preserve fresh user data\n")

        def install_and_verify(
            project: Path,
            commit: str,
            *,
            require_current_action: bool = True,
            probe_generated: bool = True,
            environment: dict[str, str] | None = None,
        ) -> dict[str, object]:
            run_public_wrapper_with_diagnostics(
                commit, project, require_current_action=require_current_action,
                environment=environment,
            )
            return verify_account_phase(
                project, commit, probe_generated=probe_generated, environment=environment
            )

        def install_candidate_and_verify(
            project: Path, account: Path, *, environment: dict[str, str]
        ) -> dict[str, object]:
            result = install_and_verify(project, candidate_sha, environment=environment)
            commands = account_receipt_commands(project, account)
            result["accountCommandNames"] = sorted(commands)
            return result

        def establish_base() -> dict[str, object]:
            try:
                run_public_wrapper_with_diagnostics(
                    base_sha,
                    base_project,
                    require_current_action=False,
                    environment=account_environment,
                )
            except Exception as error:
                transition = exact_base_compatibility_transition(
                    error, base_sha, windows=os.name == "nt"
                )
            else:
                transition = None
            commands = account_receipt_commands(base_project, account_root)
            assert_account_search_paths(base_project, source, account_environment)
            return {
                "status": "base-public-wrapper",
                "transition": transition or "none",
                "accountCommandNames": sorted(commands),
            }

        record_phase(
            evidence,
            "base-public-wrapper",
            establish_base,
        )
        if base_project.joinpath("mempalace.yaml").read_bytes() != user_config:
            raise RuntimeError("base public install rewrote valid user configuration")
        if base_sentinel.read_bytes() != b"preserve base user data\n":
            raise RuntimeError("base public install rewrote user sentinel")
        if (base_project / ".chaos-engine-runtime-current.json").exists():
            raise RuntimeError("base account install unexpectedly created a generation")
        base_snapshot = snapshot_base_state(base_project)
        offline_source = fetch_exact_base_source(
            root,
            base_sha,
            read_json(base_project / ".chaos-engine/manifest.json"),
            base_project / ".chaos-engine",
        )
        base_before_offline = project_snapshot(base_project)

        def base_offline_no_mutation() -> dict[str, object]:
            run_offline_rerun(
                base_project, offline_source, environment=account_environment
            )
            if project_snapshot(base_project) != base_before_offline:
                raise RuntimeError("offline base rerun mutated the account project")
            commands = account_receipt_commands(base_project, account_root)
            return {"status": "unchanged", "accountCommandNames": sorted(commands)}

        record_phase(evidence, "base-offline-no-mutation", base_offline_no_mutation)
        record_phase(
            evidence,
            "upgrade-candidate-wrapper",
            lambda: install_candidate_and_verify(
                base_project, account_root, environment=account_environment
            ),
        )
        if base_project.joinpath("mempalace.yaml").read_bytes() != user_config:
            raise RuntimeError("candidate upgrade rewrote valid user configuration")
        assert_local_mempalace(base_project)
        assert_single_generated_mempalace(base_project)

        def rollback_base() -> dict[str, object]:
            installed = base_project / ".chaos-engine/install.py"
            result = json.loads(
                run_checked(
                    [
                        sys.executable, str(installed), "rollback", "--project",
                        str(base_project),
                    ],
                    cwd=base_project,
                    environment=account_environment,
                ).stdout
            )
            if result.get("status") != "rolled-back":
                raise RuntimeError("candidate rollback did not report rolled-back")
            assert_base_state_restored(base_project, base_snapshot)
            commands = account_receipt_commands(base_project, account_root)
            return {
                "status": "rolled-back",
                "legacyDoctor": "recovery-required",
                "accountCommandNames": sorted(commands),
            }

        record_phase(evidence, "rollback-base-account-and-hosts", rollback_base)
        if base_project.joinpath("mempalace.yaml").read_bytes() != user_config:
            raise RuntimeError("rollback rewrote valid user configuration")
        if base_sentinel.read_bytes() != b"preserve base user data\n":
            raise RuntimeError("rollback rewrote user sentinel")

        record_phase(
            evidence,
            "reupgrade-candidate-wrapper",
            lambda: install_candidate_and_verify(
                base_project, account_root, environment=account_environment
            ),
        )
        if base_project.joinpath("mempalace.yaml").read_bytes() != user_config:
            raise RuntimeError("candidate reupgrade rewrote valid user configuration")
        assert_local_mempalace(base_project)
        assert_single_generated_mempalace(base_project)

        first_fresh = record_phase(
            evidence,
            "fresh-account-candidate-wrapper",
            lambda: install_candidate_and_verify(
                fresh_project, fresh_account_root, environment=fresh_environment
            ),
        )
        if all(action == "reused" for action in first_fresh["actions"].values()):
            raise RuntimeError("fresh account candidate install did not install isolated tools")
        if fresh_sentinel.read_bytes() != b"preserve fresh user data\n":
            raise RuntimeError("candidate install rewrote fresh user sentinel")
        assert_local_mempalace(fresh_project)
        assert_single_generated_mempalace(fresh_project)
        account_receipt = fresh_project / ".chaos-engine-dependencies.json"
        commands_before = read_json(account_receipt)["commands"]
        marker_before = (
            fresh_project / ".chaos-engine-state/mempalace/.mined"
        ).read_bytes()
        repeated = record_phase(
            evidence,
            "fresh-account-rerun",
            lambda: install_candidate_and_verify(
                fresh_project, fresh_account_root, environment=fresh_environment
            ),
        )
        if read_json(account_receipt)["commands"] != commands_before:
            raise RuntimeError("healthy account rerun changed resolved executable dispatch")
        if any(action != "reused" for action in repeated["actions"].values()):
            raise RuntimeError("healthy account rerun did not reuse latest stable tools")
        if (fresh_project / ".chaos-engine-state/mempalace/.mined").read_bytes() != marker_before:
            raise RuntimeError("healthy account rerun repeated MemPalace mining")
        if fresh_sentinel.read_bytes() != b"preserve fresh user data\n":
            raise RuntimeError("candidate rerun rewrote fresh user sentinel")
        assert_local_mempalace(fresh_project)
        assert_single_generated_mempalace(fresh_project)


def write_evidence(path: Path, evidence: dict[str, object]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(evidence, indent=2, sort_keys=True) + "\n", encoding="utf-8")


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--source", type=Path, default=ROOT / "chaos-engine")
    parser.add_argument("--candidate-sha", default=os.environ.get("GITHUB_SHA"))
    parser.add_argument("--base-sha", default=os.environ.get("GITHUB_BASE_SHA"))
    parser.add_argument(
        "--output", type=Path, default=Path("chaos-engine-live-installer-evidence.json")
    )
    args = parser.parse_args(argv)
    candidate_sha = args.candidate_sha
    if candidate_sha is None:
        git = shutil.which("git")
        if git is None:
            raise RuntimeError("git is required to resolve the candidate commit")
        candidate_sha = subprocess.run(  # nosec B603 - fixed git command and arguments.
            [git, "rev-parse", "HEAD"], cwd=ROOT, check=True,
            capture_output=True, text=True,
        ).stdout.strip()
    base_sha = args.base_sha
    if base_sha is None:
        git = shutil.which("git")
        if git is None:
            raise RuntimeError("git is required to resolve the base commit")
        base_sha = subprocess.run(  # nosec B603 - fixed git command and arguments.
            [git, "rev-parse", "HEAD^"], cwd=ROOT, check=True,
            capture_output=True, text=True,
        ).stdout.strip()
    if COMMIT.fullmatch(candidate_sha) is None or COMMIT.fullmatch(base_sha) is None:
        raise RuntimeError("candidate and base commits must be exact lowercase SHA-1 values")
    evidence: dict[str, object] = {
        "schemaVersion": 2,
        "accepted": False,
        "platform": platform.system(),
        "python": platform.python_version(),
        "phases": [],
    }
    try:
        if base_sha != KNOWN_BASE_SHA:
            raise RuntimeError("live acceptance requires the immutable known base commit")
        run_acceptance(
            args.source,
            evidence,
            candidate_sha=candidate_sha,
            base_sha=base_sha,
        )
    except Exception as error:
        evidence["failure"] = failure_evidence(error)
        write_evidence(args.output, evidence)
        return 1
    evidence["accepted"] = True
    write_evidence(args.output, evidence)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
