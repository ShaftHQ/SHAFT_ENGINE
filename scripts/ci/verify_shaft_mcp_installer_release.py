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


def installer_command() -> list[str]:
    if platform.system() == "Windows":
        return [
            "powershell",
            "-NoProfile",
            "-ExecutionPolicy",
            "Bypass",
            "-File",
            str(ROOT / "scripts" / "mcp" / "install-shaft-mcp.ps1"),
            "-Client",
            "copilot",
        ]
    return ["sh", str(ROOT / "scripts" / "mcp" / "install-shaft-mcp.sh"), "--copilot"]


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


def verify_configuration(configuration: Path, java: Path, jar: Path) -> None:
    root = json.loads(configuration.read_text(encoding="utf-8"))
    entry = root["mcpServers"]["shaft-mcp"]
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
        environment["SHAFT_MCP_FORCE_BOOTSTRAP_MAVEN"] = "1"
        environment["PATH"] = str(fake_bin) + os.pathsep + environment.get("PATH", "")
        java_options = f"-Duser.home={home} -Djava.io.tmpdir={java_temp}"
        existing_options = environment.get("JAVA_TOOL_OPTIONS", "").strip()
        environment["JAVA_TOOL_OPTIONS"] = f"{existing_options} {java_options}".strip()

        command = installer_command()
        subprocess.run(command, cwd=root, env=environment, check=True)
        jar = installed_jar(root, home, environment)
        first_hash = sha256(jar)
        first_timestamp = jar.stat().st_mtime_ns
        configuration = copilot_home / "mcp-config.json"
        verify_configuration(configuration, Path(shutil.which("java") or "java"), jar)

        subprocess.run(command, cwd=root, env=environment, check=True)
        if sha256(jar) != first_hash or jar.stat().st_mtime_ns != first_timestamp:
            raise RuntimeError("Repeated public installation did not reuse the verified JAR.")
        verify_configuration(configuration, Path(shutil.which("java") or "java"), jar)
        print(f"Verified public shaft-mcp LATEST installer on {platform.system()}: {jar.parent.name}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
