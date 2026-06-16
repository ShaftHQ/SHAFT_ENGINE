#!/usr/bin/env python3
"""Run the public shaft-mcp LATEST installer twice in an isolated user profile."""

from __future__ import annotations

import hashlib
import json
import os
import platform
import shutil
import stat
import subprocess
import tempfile
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]


def write_fake_copilot(bin_directory: Path) -> None:
    bin_directory.mkdir(parents=True, exist_ok=True)
    if platform.system() == "Windows":
        executable = bin_directory / "copilot.cmd"
        executable.write_text(
            "@echo off\r\n"
            "if \"%1\"==\"--version\" (\r\n"
            "  echo copilot release verifier\r\n"
            "  exit /b 0\r\n"
            ")\r\n"
            "exit /b 1\r\n",
            encoding="utf-8",
        )
        return
    executable = bin_directory / "copilot"
    executable.write_text(
        "#!/bin/sh\n"
        "if [ \"$1\" = \"--version\" ]; then\n"
        "  echo 'copilot release verifier'\n"
        "  exit 0\n"
        "fi\n"
        "exit 1\n",
        encoding="utf-8",
    )
    executable.chmod(executable.stat().st_mode | stat.S_IXUSR)


def installer_command(client: str) -> list[str]:
    if platform.system() == "Windows":
        return [
            "powershell",
            "-NoProfile",
            "-ExecutionPolicy",
            "Bypass",
            "-File",
            str(ROOT / "scripts" / "mcp" / "install-shaft-mcp.ps1"),
            "-Client",
            client,
        ]
    return ["sh", str(ROOT / "scripts" / "mcp" / "install-shaft-mcp.sh"), f"--{client}"]


def sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as source:
        for chunk in iter(lambda: source.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


def installed_jar(root: Path, home: Path, environment: dict[str, str]) -> Path:
    system = platform.system()
    if system == "Windows":
        versions = root / "local-app-data" / "ShaftHQ" / "shaft-mcp" / "versions"
    elif system == "Darwin":
        versions = home / "Library" / "Application Support" / "ShaftHQ" / "shaft-mcp" / "versions"
    elif system == "Linux":
        versions = Path(environment["XDG_DATA_HOME"]) / "shafthq" / "shaft-mcp" / "versions"
    else:
        raise RuntimeError(f"Unsupported release-verification operating system: {system}")
    jars = list(versions.glob("*/shaft-mcp.jar"))
    if len(jars) != 1:
        raise RuntimeError(f"Expected one installed shaft-mcp JAR, found: {jars}")
    return jars[0].resolve()


def expected_java(environment: dict[str, str]) -> Path:
    executable = "java.exe" if platform.system() == "Windows" else "java"
    java_home = environment.get("JAVA_HOME", "")
    if java_home:
        candidate = Path(java_home) / "bin" / executable
        if candidate.is_file():
            return candidate.resolve()
    resolved = shutil.which(executable, path=environment.get("PATH"))
    if not resolved:
        raise RuntimeError("Expected Java executable was not found on PATH.")
    return Path(resolved).resolve()


def configuration_path(client: str, root: Path, home: Path, environment: dict[str, str]) -> Path:
    system = platform.system()
    if client == "copilot":
        return home / ".copilot" / "mcp-config.json"
    if client == "copilot-intellij":
        if system == "Windows":
            return root / "local-app-data" / "github-copilot" / "intellij" / "mcp.json"
        return Path(environment["XDG_CONFIG_HOME"]) / "github-copilot" / "intellij" / "mcp.json"
    if client == "claude-desktop":
        if system == "Windows":
            return root / "roaming-app-data" / "Claude" / "claude_desktop_config.json"
        return home / "Library" / "Application Support" / "Claude" / "claude_desktop_config.json"
    raise RuntimeError(f"Unsupported installer verification client: {client}")


def verify_configuration(configuration: Path, java: Path, jar: Path, root_property: str) -> None:
    root = json.loads(configuration.read_text(encoding="utf-8"))
    entry = root[root_property]["shaft-mcp"]
    if Path(entry["command"]).resolve() != java.resolve():
        raise RuntimeError(f"Unexpected Java command in {configuration}: {entry['command']}")
    if entry["args"] != ["-jar", str(jar)]:
        raise RuntimeError(f"Unexpected shaft-mcp arguments in {configuration}: {entry['args']}")


def main() -> int:
    with tempfile.TemporaryDirectory(prefix="shaft-mcp-public-installer-") as temporary:
        root = Path(temporary).resolve()
        home = root / "home"
        java_temp = root / "java-temp"
        fake_bin = root / "bin"
        copilot_home = home / ".copilot"
        for directory in (home, java_temp, copilot_home):
            directory.mkdir(parents=True, exist_ok=True)
        write_fake_copilot(fake_bin)

        environment = os.environ.copy()
        environment["HOME"] = str(home)
        environment["USERPROFILE"] = str(home)
        environment["COPILOT_HOME"] = str(copilot_home)
        environment["LOCALAPPDATA"] = str(root / "local-app-data")
        environment["APPDATA"] = str(root / "roaming-app-data")
        environment["XDG_DATA_HOME"] = str(root / "xdg-data")
        environment["XDG_CONFIG_HOME"] = str(root / "xdg-config")
        environment["SHAFT_MCP_BOOTSTRAP_HOME"] = str(root / "bootstrap")
        environment["PATH"] = str(fake_bin) + os.pathsep + environment.get("PATH", "")
        java_options = f"-Duser.home={home} -Djava.io.tmpdir={java_temp}"
        existing_options = environment.get("JAVA_TOOL_OPTIONS", "").strip()
        environment["JAVA_TOOL_OPTIONS"] = f"{existing_options} {java_options}".strip()

        clients = ["copilot", "copilot-intellij"]
        if platform.system() in {"Darwin", "Windows"}:
            clients.append("claude-desktop")

        java = expected_java(environment)
        for client in clients:
            subprocess.run(installer_command(client), cwd=root, env=environment, check=True)

        jar = installed_jar(root, home, environment)
        first_hash = sha256(jar)
        first_timestamp = jar.stat().st_mtime_ns

        for client in clients:
            root_property = "mcpServers" if client in {"copilot", "claude-desktop"} else "servers"
            verify_configuration(configuration_path(client, root, home, environment), java, jar, root_property)

        subprocess.run(installer_command(clients[0]), cwd=root, env=environment, check=True)
        if sha256(jar) != first_hash or jar.stat().st_mtime_ns != first_timestamp:
            raise RuntimeError("Repeated public installation did not reuse the verified JAR.")
        verify_configuration(configuration_path(clients[0], root, home, environment), java, jar, "mcpServers")
        print(f"Verified public shaft-mcp LATEST installer on {platform.system()}: {jar.parent.name}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
