#!/usr/bin/env python3
"""Run bounded real-tool acceptance for the immutable ChaosEngine installer."""

from __future__ import annotations

import argparse
import importlib.util
import json
import os
import platform
import re
import shutil
import subprocess  # nosec B404 - fixed repository-owned commands only.
import sys
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
        raise RuntimeError(f"command failed ({result.returncode}): {sanitize(detail)}")
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


def download_commit_source(source: Path, commit: str, destination: Path) -> Path:
    specification = importlib.util.spec_from_file_location(
        "chaos_engine_acceptance_bootstrap", source / "bootstrap.py"
    )
    if specification is None or specification.loader is None:
        raise RuntimeError("candidate bootstrap could not be loaded")
    module = importlib.util.module_from_spec(specification)
    specification.loader.exec_module(module)
    destination.mkdir()
    return module.download_source("ShaftHQ/SHAFT_ENGINE", commit, destination)


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


def run_offline_rerun(project: Path, source: Path) -> None:
    run_checked(
        [sys.executable, "-c", OFFLINE_RERUN, str(project), str(source)],
        cwd=project,
        environment=offline_environment(block_path=True),
        timeout=180,
    )


def run_public_wrapper(
    commit: str, project: Path, *, require_current_action: bool = True
) -> None:
    result = run_checked(
        public_wrapper_command(commit, windows=os.name == "nt"),
        cwd=project,
        environment=wrapper_environment(),
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
            **clean_environment(),
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


def probe_project_mcps(tool: Path, project: Path) -> None:
    commands = (
        [sys.executable, str(tool), "memory-mcp"],
        [sys.executable, str(tool), "mempalace-mcp"],
    )
    for command in commands:
        probe_mcp(
            command,
            project,
            environment={"MEMPALACE_BACKEND": "sqlite_exact"},
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


def probe_generated_mcps(project: Path) -> None:
    # Parse both platform forms before executing current host's generated command.
    generated_mcp_commands(project, windows=False)
    commands = generated_mcp_commands(project, windows=os.name == "nt")
    for _name, command, cwd, environment in commands:
        probe_mcp(command, cwd, environment=environment)


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


def verify_account_phase(
    project: Path, expected_commit: str, *, probe_generated: bool = True
) -> dict[str, object]:
    installed = project / ".chaos-engine"
    for command in ("status", "doctor"):
        result = json.loads(
            run_checked(
                [
                    sys.executable,
                    str(installed / "install.py"),
                    command,
                    "--project",
                    str(project),
                    "--json",
                ],
                cwd=project,
            ).stdout
        )
        if result.get("status") != "healthy" or result.get("commit") != expected_commit:
            components = result.get("components", {})
            dependencies = result.get("dependencies", {}).get("components", {})
            summary = {
                "status": result.get("status"),
                "commit": result.get("commit"),
                "kernel": result.get("kernel", {}).get("status"),
                "hosts": result.get("hosts", {}).get("status"),
                "dependencies": result.get("dependencies", {}).get("status"),
                "unhealthyComponents": {
                    name: record
                    for name, record in components.items()
                    if record.get("status") != "healthy"
                },
                "unhealthyDependencies": {
                    name: record.get("status")
                    for name, record in dependencies.items()
                    if record.get("status") != "healthy"
                },
            }
            raise RuntimeError(
                f"{command} did not report expected healthy account setup: "
                f"{sanitize(json.dumps(summary, sort_keys=True))}"
            )
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
        run_checked([sys.executable, str(tool), name, *arguments], cwd=project, timeout=120)
        dispatches[name] = "pass"
    if probe_generated:
        probe_generated_mcps(project)
        dispatches.update({"memory-mcp": "pass", "mempalace-mcp": "pass"})
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
    }


def record_phase(
    evidence: dict[str, object], name: str, operation
) -> dict[str, object]:
    started = time.monotonic()
    checks = operation()
    phase = {
        "name": name,
        "status": "pass",
        "durationSeconds": round(time.monotonic() - started, 3),
        "checks": checks,
    }
    evidence["phases"].append(phase)
    return checks


def project_snapshot(project: Path) -> dict[str, bytes]:
    """Read disposable-project state for an exact offline no-mutation proof."""
    return {
        path.relative_to(project).as_posix(): path.read_bytes()
        for path in sorted(project.rglob("*"))
        if path.is_file()
    }


def seed_exact_mempalace(source: Path, project: Path) -> None:
    """Make the disposable legacy account project healthy without global mining."""
    specification = importlib.util.spec_from_file_location(
        "chaos_engine_acceptance_hosts", source / "hosts.py"
    )
    if specification is None or specification.loader is None:
        raise RuntimeError("candidate hosts controller could not be loaded")
    controller = importlib.util.module_from_spec(specification)
    specification.loader.exec_module(controller)
    controller.initialize_mempalace_runtime(project)
    marker = project / ".chaos-engine-state/mempalace/.mined"
    marker.write_bytes(b"current\n")


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
        base_project = root / "base consumer with spaces Ω"
        blank_project = root / "blank consumer with spaces Ω"
        base_project.mkdir()
        blank_project.mkdir()
        user_config = (
            "wing: acceptance\n"
            "rooms:\n  - name: general\n    description: Acceptance project\n"
            "exclude_patterns:\n  - .chaos-engine-state/**\n"
        ).encode()
        base_project.joinpath("mempalace.yaml").write_bytes(user_config)
        seed_exact_mempalace(source, base_project)
        base_sentinel = base_project / "user-sentinel.txt"
        base_sentinel.write_bytes(b"preserve base user data\n")
        blank_sentinel = blank_project / "user-sentinel.txt"
        blank_sentinel.write_bytes(b"preserve blank user data\n")

        def install_and_verify(
            project: Path,
            commit: str,
            *,
            require_current_action: bool = True,
            probe_generated: bool = True,
        ) -> dict[str, object]:
            run_public_wrapper(
                commit, project, require_current_action=require_current_action
            )
            return verify_account_phase(
                project, commit, probe_generated=probe_generated
            )

        record_phase(
            evidence,
            "preseeded-base-wrapper",
            lambda: install_and_verify(
                base_project, base_sha, require_current_action=False, probe_generated=False
            ),
        )
        if base_project.joinpath("mempalace.yaml").read_bytes() != user_config:
            raise RuntimeError("base public install rewrote valid user configuration")
        if base_sentinel.read_bytes() != b"preserve base user data\n":
            raise RuntimeError("base public install rewrote user sentinel")
        if (base_project / ".chaos-engine-runtime-current.json").exists():
            raise RuntimeError("base account install unexpectedly created a generation")
        offline_source = download_commit_source(
            source, base_sha, root / "offline-base-source"
        )
        base_before_offline = project_snapshot(base_project)

        def base_offline_no_mutation() -> dict[str, object]:
            run_offline_rerun(base_project, offline_source)
            if project_snapshot(base_project) != base_before_offline:
                raise RuntimeError("offline base rerun mutated the account project")
            return verify_account_phase(base_project, base_sha, probe_generated=False)

        record_phase(evidence, "base-offline-no-mutation", base_offline_no_mutation)
        record_phase(
            evidence,
            "upgrade-candidate-wrapper",
            lambda: install_and_verify(base_project, candidate_sha),
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
                        str(base_project), "--json",
                    ],
                    cwd=base_project,
                ).stdout
            )
            if result.get("status") != "rolled-back":
                raise RuntimeError("candidate rollback did not report rolled-back")
            return verify_account_phase(base_project, base_sha)

        record_phase(evidence, "rollback-base-account-and-hosts", rollback_base)
        if base_project.joinpath("mempalace.yaml").read_bytes() != user_config:
            raise RuntimeError("rollback rewrote valid user configuration")
        if base_sentinel.read_bytes() != b"preserve base user data\n":
            raise RuntimeError("rollback rewrote user sentinel")

        first_blank = record_phase(
            evidence,
            "blank-candidate-wrapper",
            lambda: install_and_verify(blank_project, candidate_sha),
        )
        if any(action != "reused" for action in first_blank["actions"].values()):
            raise RuntimeError("blank candidate install did not reuse account tools")
        if blank_sentinel.read_bytes() != b"preserve blank user data\n":
            raise RuntimeError("candidate install rewrote blank user sentinel")
        assert_local_mempalace(blank_project)
        assert_single_generated_mempalace(blank_project)
        account_receipt = blank_project / ".chaos-engine-dependencies.json"
        commands_before = read_json(account_receipt)["commands"]
        marker_before = (
            blank_project / ".chaos-engine-state/mempalace/.mined"
        ).read_bytes()
        repeated = record_phase(
            evidence,
            "blank-candidate-rerun",
            lambda: install_and_verify(blank_project, candidate_sha),
        )
        if read_json(account_receipt)["commands"] != commands_before:
            raise RuntimeError("healthy account rerun changed resolved executable dispatch")
        if any(action != "reused" for action in repeated["actions"].values()):
            raise RuntimeError("healthy account rerun did not reuse latest stable tools")
        if (blank_project / ".chaos-engine-state/mempalace/.mined").read_bytes() != marker_before:
            raise RuntimeError("healthy account rerun repeated MemPalace mining")
        if blank_sentinel.read_bytes() != b"preserve blank user data\n":
            raise RuntimeError("candidate rerun rewrote blank user sentinel")
        assert_local_mempalace(blank_project)
        assert_single_generated_mempalace(blank_project)


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
        "schemaVersion": 1,
        "accepted": False,
        "platform": platform.system(),
        "python": platform.python_version(),
        "phases": [],
    }
    try:
        run_acceptance(
            args.source,
            evidence,
            candidate_sha=candidate_sha,
            base_sha=base_sha,
        )
    except Exception as error:
        evidence["failure"] = {"type": type(error).__name__, "detail": sanitize(error)}
        write_evidence(args.output, evidence)
        return 1
    evidence["accepted"] = True
    write_evidence(args.output, evidence)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
