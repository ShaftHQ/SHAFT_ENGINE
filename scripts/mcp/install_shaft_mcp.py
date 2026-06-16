#!/usr/bin/env python3
"""Standalone shaft-mcp installer used by the thin ps1/sh launchers."""

from __future__ import annotations

import argparse
import json
import os
import platform
import queue
import re
import shutil
import subprocess
import sys
import tarfile
import tempfile
import threading
import time
import urllib.error
import urllib.request
import xml.etree.ElementTree as ET
import zipfile
from hashlib import sha256
from pathlib import Path
from typing import Any

SERVER_NAME = "shaft-mcp"
ARTIFACT_PATH = "io/github/shafthq/shaft-mcp"
DEFAULT_REPOSITORY = "https://repo.maven.apache.org/maven2"
TARGETS = ("codex", "claude", "claude-desktop", "copilot", "copilot-intellij")
TARGET_CHOICES = (
    ("codex", "Codex CLI / IDE"),
    ("claude", "Claude Code"),
    ("claude-desktop", "Claude Desktop"),
    ("copilot", "GitHub Copilot CLI"),
    ("copilot-intellij", "GitHub Copilot for IntelliJ IDEA"),
)


class InstallError(RuntimeError):
    def __init__(self, message: str, code: int = 1) -> None:
        super().__init__(message)
        self.code = code


def log(message: str) -> None:
    print(message, file=sys.stderr)


def debug(message: str) -> None:
    if os.environ.get("SHAFT_MCP_DEBUG") == "1":
        log(f"install-shaft-mcp debug: {message}")


def fail(message: str, code: int = 1) -> None:
    raise InstallError(message, code)


def normalize_client(value: str | None) -> str | None:
    if value is None:
        return None
    normalized = value.strip()
    if normalized.startswith("--"):
        normalized = normalized[2:]
    return normalized or None


def choose_client() -> str:
    if not sys.stdin.isatty():
        fail("Pass a client target when running non-interactively.", 2)
    print("Choose the MCP client to configure:")
    for index, (_, label) in enumerate(TARGET_CHOICES, start=1):
        print(f"  {index}. {label}")
    while True:
        answer = input("Enter a number: ").strip()
        if answer.isdigit():
            index = int(answer)
            if 1 <= index <= len(TARGET_CHOICES):
                return TARGET_CHOICES[index - 1][0]
        normalized = normalize_client(answer)
        if normalized in TARGETS:
            return normalized
        print("Enter a number from 1 to 5, or a target name.")


def parse_args(argv: list[str]) -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Install and configure shaft-mcp for a supported MCP client.",
        formatter_class=argparse.RawTextHelpFormatter,
    )
    parser.add_argument("--client", choices=TARGETS)
    parser.add_argument("--version", default=os.environ.get("SHAFT_MCP_VERSION", "LATEST"))
    for target, _ in TARGET_CHOICES:
        parser.add_argument(f"--{target}", action="store_true", dest=target.replace("-", "_"))
    parser.add_argument(
        "target",
        nargs="?",
        help="Optional target name: codex, claude, claude-desktop, copilot, or copilot-intellij.",
    )
    args = parser.parse_args(argv)

    selected: list[str] = []
    if args.client:
        selected.append(args.client)
    if args.target:
        selected.append(normalize_client(args.target) or "")
    for target, _ in TARGET_CHOICES:
        if getattr(args, target.replace("-", "_")):
            selected.append(target)

    selected = [target for target in selected if target]
    if len(set(selected)) > 1:
        fail("Specify only one MCP client target.", 2)
    args.client = selected[0] if selected else choose_client()
    if args.client not in TARGETS:
        fail("Usage: install-shaft-mcp.py [--client <codex|claude|claude-desktop|copilot|copilot-intellij>]", 2)
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


def download_bytes(url: str, attempts: int = 5) -> bytes:
    headers = {"User-Agent": "shaft-mcp-installer"}
    last_error: BaseException | None = None
    for attempt in range(1, attempts + 1):
        try:
            request = urllib.request.Request(url, headers=headers)
            with urllib.request.urlopen(request, timeout=120) as response:
                return response.read()
        except (urllib.error.URLError, TimeoutError) as exc:
            last_error = exc
            time.sleep(min(attempt * 2, 10))
    assert last_error is not None
    raise last_error


def download_file(url: str, output: Path) -> None:
    output.parent.mkdir(parents=True, exist_ok=True)
    temporary = output.with_name(f".{output.name}.{os.getpid()}.tmp")
    headers = {"User-Agent": "shaft-mcp-installer"}
    last_error: BaseException | None = None
    for attempt in range(1, 6):
        try:
            request = urllib.request.Request(url, headers=headers)
            with urllib.request.urlopen(request, timeout=120) as response, temporary.open("wb") as target:
                shutil.copyfileobj(response, target, length=1024 * 1024)
            os.replace(temporary, output)
            return
        except (urllib.error.URLError, TimeoutError, OSError) as exc:
            last_error = exc
            temporary.unlink(missing_ok=True)
            time.sleep(min(attempt * 2, 10))
    assert last_error is not None
    raise last_error


def url_text(url: str) -> str:
    return download_bytes(url).decode("utf-8")


def file_sha256(path: Path) -> str:
    digest = sha256()
    with path.open("rb") as source:
        for chunk in iter(lambda: source.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


def java_feature(java: Path) -> int | None:
    try:
        result = subprocess.run([str(java), "-version"], text=True, capture_output=True, timeout=20)
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
    log(f"Downloading Java 25 for {os_name} {arch}...")
    if java_root.exists():
        shutil.rmtree(java_root)
    java_root.mkdir(parents=True, exist_ok=True)
    download_file(url, archive)
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
                return candidate.resolve()
        path_java = shutil.which(executable) or shutil.which("java")
        debug(f"Checking PATH Java candidate {path_java}.")
        if path_java and is_java25(Path(path_java)):
            return Path(path_java).resolve()
        cached = find_cached_java(root / "tools" / "jdk")
        if cached:
            return cached
    return download_java25(root)


def java_home_for(java: Path) -> Path:
    return java.resolve().parent.parent


def resolve_shaft_mcp_version(requested_version: str | None, repository: str, root: Path) -> str:
    requested = (requested_version or "LATEST").strip()
    if requested and requested != "LATEST":
        return requested
    log("Resolving io.github.shafthq:shaft-mcp:LATEST...")
    metadata_url = f"{repository}/{ARTIFACT_PATH}/maven-metadata.xml"
    metadata_path = root / "downloads" / "shaft-mcp-maven-metadata.xml"
    download_file(metadata_url, metadata_path)
    try:
        xml_root = ET.fromstring(metadata_path.read_text(encoding="utf-8"))
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


def install_shaft_mcp_jar(version: str, repository: str, root: Path) -> Path:
    version_path = f"{ARTIFACT_PATH}/{version}"
    filename = f"shaft-mcp-{version}.jar"
    url = f"{repository}/{version_path}/{filename}"
    expected = expected_sha256(url)
    target_dir = application_data_root() / "versions" / version
    target = target_dir / "shaft-mcp.jar"
    if target.is_file() and file_sha256(target) == expected:
        return target.resolve()

    downloaded = root / "downloads" / filename
    log(f"Downloading io.github.shafthq:shaft-mcp:{version}...")
    download_file(url, downloaded)
    if file_sha256(downloaded) != expected:
        fail(f"Checksum verification failed for {downloaded}.", 4)

    target_dir.mkdir(parents=True, exist_ok=True)
    temporary = target_dir / f".shaft-mcp.jar.{os.getpid()}.tmp"
    shutil.copyfile(downloaded, temporary)
    if file_sha256(temporary) != expected:
        temporary.unlink(missing_ok=True)
        fail("Copied shaft-mcp JAR failed SHA-256 verification.", 4)
    os.replace(temporary, target)
    if file_sha256(target) != expected:
        fail("Installed shaft-mcp JAR failed SHA-256 verification.", 4)
    return target.resolve()


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
            pass


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
    fail("Timed out while probing the installed shaft-mcp JAR.", 4)


def probe_stdio(java: Path, jar: Path) -> None:
    process = subprocess.Popen(
        [str(java), "-jar", str(jar)],
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
    assert process.stdout is not None
    assert process.stderr is not None
    assert process.stdin is not None
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
                pass


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
        result = subprocess.run(command, text=True, capture_output=True, timeout=60)
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


def stdio_entry(java: Path, jar: Path, copilot: bool = False) -> dict[str, Any]:
    entry: dict[str, Any] = {"command": str(java), "args": ["-jar", str(jar)]}
    if copilot:
        entry["type"] = "local"
        entry["tools"] = ["*"]
    return entry


def verify_json_entry(path: Path, root_property: str, java: Path, jar: Path) -> None:
    root = read_json_object(path)
    servers = root.get(root_property)
    if not isinstance(servers, dict) or SERVER_NAME not in servers:
        fail(f"The resulting configuration does not contain {SERVER_NAME}.", 5)
    entry = servers[SERVER_NAME]
    if not isinstance(entry, dict):
        fail("The resulting shaft-mcp entry is not an object.", 5)
    if entry.get("command") != str(java):
        fail("The resulting shaft-mcp Java command is incorrect.", 5)
    if entry.get("args") != ["-jar", str(jar)]:
        fail("The resulting shaft-mcp JAR arguments are incorrect.", 5)


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
    user_home = home().resolve()
    directory = Path.cwd().resolve()
    while True:
        for candidate in project_candidates(directory, client):
            if project_entry_exists(candidate, client):
                fail(f"Project configuration at {candidate} defines {SERVER_NAME} and would override the per-user entry.", 5)
        if directory == user_home or directory.parent == directory:
            break
        directory = directory.parent


def configure_codex(java: Path, jar: Path) -> None:
    codex = require_command("codex", "Codex")
    run_checked([str(codex), "mcp", "remove", SERVER_NAME], "Codex could not remove the previous shaft-mcp entry.", True)
    run_checked([str(codex), "mcp", "add", SERVER_NAME, "--", str(java), "-jar", str(jar)], "Codex MCP configuration command failed.")
    result = run_checked([str(codex), "mcp", "get", SERVER_NAME, "--json"], "Codex could not verify the shaft-mcp entry.")
    try:
        entry = json.loads(result.stdout).get("transport", {})
    except json.JSONDecodeError as exc:
        fail(f"Codex verification returned malformed JSON: {exc}", 5)
    if entry.get("command") != str(java) or entry.get("args") != ["-jar", str(jar)]:
        fail("Codex verification returned an unexpected shaft-mcp command.", 5)


def configure_claude_code(java: Path, jar: Path) -> None:
    claude = require_command("claude", "Claude Code")
    configuration = configuration_path("claude")
    try:
        root = read_json_object(configuration)
    except json.JSONDecodeError:
        fail("Claude Code configuration is malformed.", 5)
    existing = isinstance(root.get("mcpServers"), dict) and SERVER_NAME in root["mcpServers"]
    if existing:
        run_checked([str(claude), "mcp", "remove", SERVER_NAME, "-s", "user"], "Claude Code could not remove the previous shaft-mcp entry.")
    run_checked([str(claude), "mcp", "add", "-s", "user", SERVER_NAME, "--", str(java), "-jar", str(jar)], "Claude Code MCP configuration command failed.")
    verify_json_entry(configuration, "mcpServers", java, jar)


def configure_claude_desktop(java: Path, jar: Path) -> None:
    configuration = configuration_path("claude-desktop")

    def mutate() -> None:
        root = read_json_object(configuration)
        servers = root.setdefault("mcpServers", {})
        if not isinstance(servers, dict):
            fail("Configuration property must be an object: mcpServers", 5)
        servers[SERVER_NAME] = stdio_entry(java, jar)
        write_json_atomically(configuration, root)

    update_json_configuration(configuration, mutate, lambda: verify_json_entry(configuration, "mcpServers", java, jar))
    print("Restart Claude Desktop to load shaft-mcp.")


def configure_copilot(java: Path, jar: Path) -> None:
    copilot = require_command("copilot", "GitHub Copilot CLI")
    run_checked([str(copilot), "--version"], "GitHub Copilot CLI is not available.")
    configuration = configuration_path("copilot")

    def mutate() -> None:
        root = read_json_object(configuration)
        servers = root.setdefault("mcpServers", {})
        if not isinstance(servers, dict):
            fail("Configuration property must be an object: mcpServers", 5)
        servers[SERVER_NAME] = stdio_entry(java, jar, copilot=True)
        write_json_atomically(configuration, root)

    update_json_configuration(configuration, mutate, lambda: verify_json_entry(configuration, "mcpServers", java, jar))


def configure_copilot_intellij(java: Path, jar: Path) -> None:
    configuration = configuration_path("copilot-intellij")

    def mutate() -> None:
        root = read_json_object(configuration)
        servers = root.setdefault("servers", {})
        if not isinstance(servers, dict):
            fail("Configuration property must be an object: servers", 5)
        servers[SERVER_NAME] = {"type": "stdio", "command": str(java), "args": ["-jar", str(jar)]}
        write_json_atomically(configuration, root)

    update_json_configuration(configuration, mutate, lambda: verify_json_entry(configuration, "servers", java, jar))
    print("Restart IntelliJ IDEA after GitHub Copilot reloads its MCP configuration.")


def configure_client(client: str, java: Path, jar: Path) -> None:
    if client == "codex":
        configure_codex(java, jar)
    elif client == "claude":
        configure_claude_code(java, jar)
    elif client == "claude-desktop":
        configure_claude_desktop(java, jar)
    elif client == "copilot":
        configure_copilot(java, jar)
    elif client == "copilot-intellij":
        configure_copilot_intellij(java, jar)
    else:
        fail(f"Unsupported client: {client}", 2)


def install(args: argparse.Namespace) -> None:
    root = bootstrap_root()
    root.mkdir(parents=True, exist_ok=True)
    repository = os.environ.get("SHAFT_MCP_REPOSITORY_URL", DEFAULT_REPOSITORY).rstrip("/")

    java = get_java25(root)
    java_home = java_home_for(java)
    os.environ["JAVA_HOME"] = str(java_home)
    os.environ["PATH"] = f"{java.parent}{os.pathsep}{os.environ.get('PATH', '')}"

    detect_project_override(args.client)
    version = resolve_shaft_mcp_version(args.version, repository, root)
    jar = install_shaft_mcp_jar(version, repository, root)

    log(f"Verifying shaft-mcp {version} over stdio...")
    probe_stdio(java, jar)

    log(f"Configuring shaft-mcp for {args.client}...")
    configure_client(args.client, java, jar)
    print(f"shaft-mcp {version} is installed and configured for {args.client}.")


def main(argv: list[str]) -> int:
    try:
        args = parse_args(argv)
        install(args)
        return 0
    except InstallError as exc:
        print(f"install-shaft-mcp: {exc}", file=sys.stderr)
        return exc.code
    except KeyboardInterrupt:
        print("install-shaft-mcp: interrupted", file=sys.stderr)
        return 130
    except Exception as exc:
        print(f"install-shaft-mcp: {exc}", file=sys.stderr)
        if os.environ.get("SHAFT_MCP_DEBUG") == "1":
            raise
        return 1


if __name__ == "__main__":
    raise SystemExit(main(sys.argv[1:]))
