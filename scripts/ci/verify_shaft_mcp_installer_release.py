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
import time
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
INSTALLER_CLIENTS = {"claude-desktop", "copilot", "copilot-intellij", "intellij-plugin"}
INSTALLER_RETRY_ATTEMPTS = 3
INSTALLER_RETRY_DELAY_SECONDS = 3.0


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


def installer_command(client: str, *extra_args: str) -> list[str]:
    if client not in INSTALLER_CLIENTS:
        raise ValueError(f"Unsupported installer verification client: {client}")
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
            *extra_args,
        ]
    return ["sh", str(ROOT / "scripts" / "mcp" / "install-shaft-mcp.sh"), f"--{client}", *extra_args]


def run_installer(
    command: list[str],
    *,
    cwd: Path,
    environment: dict[str, str],
    capture: bool = False,
    attempts: int = INSTALLER_RETRY_ATTEMPTS,
    retry_delay_seconds: float = INSTALLER_RETRY_DELAY_SECONDS,
) -> subprocess.CompletedProcess[str]:
    if attempts < 1:
        raise ValueError(f"attempts must be at least 1, got {attempts}")
    # The public installer resolves shaft-mcp's default properties from
    # raw.githubusercontent.com over anonymous HTTP; GitHub's rate limiting can
    # transiently return 429 for that request, so retry a few times before
    # failing the whole release verification.
    for attempt in range(1, attempts + 1):
        try:
            return subprocess.run(  # nosec B603 - command comes from installer_command.
                command,
                cwd=cwd,
                env=environment,
                stdin=subprocess.DEVNULL,
                check=True,
                capture_output=capture,
                text=True,
            )
        except subprocess.CalledProcessError as error:
            if attempt == attempts:
                details = []
                if error.stdout:
                    details.append(f"stdout:\n{error.stdout}")
                if error.stderr:
                    details.append(f"stderr:\n{error.stderr}")
                suffix = "\n" + "\n".join(details) if details else ""
                raise RuntimeError(
                    f"Installer command failed after {attempts} attempts: {error.cmd}{suffix}"
                ) from error
            print(
                f"Installer command failed (attempt {attempt}/{attempts}); retrying: {error.cmd}",
                flush=True,
            )
            time.sleep(retry_delay_seconds * attempt)


def last_json_line(output: str) -> dict[str, object]:
    for line in reversed(output.splitlines()):
        line = line.strip()
        if line.startswith("{") and line.endswith("}"):
            return json.loads(line)
    raise RuntimeError(f"Expected JSON installer output, got:\n{output}")


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


def installed_args(jar: Path) -> Path:
    args = jar.with_name("shaft-mcp.args")
    if not args.is_file():
        raise RuntimeError(f"Expected installed shaft-mcp argfile: {args}")
    return args.resolve()


def installed_dependencies(home: Path) -> list[Path]:
    # Runtime dependencies install into the local Maven repository (the isolated HOME's
    # ~/.m2/repository) so Maven builds reuse them and reinstalls skip them.
    repository = home / ".m2" / "repository"
    dependencies = sorted(repository.rglob("*.jar"))
    if not dependencies:
        raise RuntimeError(f"Expected installed shaft-mcp runtime dependencies under: {repository}")
    return dependencies


def verify_argfile(args: Path, jar: Path) -> None:
    text = args.read_text(encoding="utf-8")
    runtime_root = jar.parent.parent.parent / "work"
    required = {
        f'"-Duser.dir={runtime_root.as_posix()}"',
        f'"-Dshaft.mcp.workspaceRoot={runtime_root.as_posix()}"',
        "-cp",
        "com.shaft.mcp.ShaftMcpApplication",
    }
    missing = [token for token in sorted(required) if token not in text]
    if missing:
        raise RuntimeError(f"Installed shaft-mcp argfile is missing required entries: {missing}")


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


def verify_configuration(configuration: Path, java: Path, args: Path, root_property: str) -> None:
    root = json.loads(configuration.read_text(encoding="utf-8"))
    entry = root[root_property]["shaft-mcp"]
    if Path(entry["command"]).resolve() != java.resolve():
        raise RuntimeError(f"Unexpected Java command in {configuration}: {entry['command']}")
    if entry["args"] != [f"@{args}"]:
        raise RuntimeError(f"Unexpected shaft-mcp arguments in {configuration}: {entry['args']}")


def verify_plugin_json(result: dict[str, object], java: Path, args: Path) -> None:
    if result.get("client") != "intellij-plugin":
        raise RuntimeError(f"Unexpected IntelliJ plugin installer target: {result}")
    if result.get("server") != "shaft-mcp":
        raise RuntimeError(f"Unexpected MCP server name in IntelliJ plugin installer output: {result}")
    if Path(str(result.get("command"))).resolve() != java.resolve():
        raise RuntimeError(f"Unexpected Java command in IntelliJ plugin installer output: {result}")
    if result.get("args") != [f"@{args}"]:
        raise RuntimeError(f"Unexpected SHAFT MCP arguments in IntelliJ plugin installer output: {result}")
    shaft_skills = result.get("shaftSkills")
    if not isinstance(shaft_skills, dict) or shaft_skills.get("installed") is not True:
        raise RuntimeError(f"Expected IntelliJ plugin installer to install SHAFT skills by default: {result}")


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
        plugin_install = run_installer(
            installer_command("intellij-plugin", "--json"),
            cwd=root,
            environment=environment,
            capture=True,
        )
        plugin_result = last_json_line(plugin_install.stdout)
        shaft_skills = plugin_result.get("shaftSkills")
        if not isinstance(shaft_skills, dict):
            raise RuntimeError(f"Expected SHAFT skills metadata in IntelliJ plugin installer output: {plugin_result}")
        skills_path = Path(str(shaft_skills.get("path", "")))
        if not (skills_path / "writing-shaft-tests" / "SKILL.md").is_file():
            raise RuntimeError(f"Expected SHAFT skills to be installed by the unattended plugin installer: {skills_path}")
        for client in clients:
            run_installer(installer_command(client), cwd=root, environment=environment)

        jar = installed_jar(root, home, environment)
        args = installed_args(jar)
        verify_argfile(args, jar)
        installed_dependencies(home)
        first_hash = sha256(jar)
        first_timestamp = jar.stat().st_mtime_ns

        for client in clients:
            root_property = "mcpServers" if client in {"copilot", "claude-desktop"} else "servers"
            verify_configuration(configuration_path(client, root, home, environment), java, args, root_property)
        verify_plugin_json(plugin_result, java, args)

        run_installer(installer_command(clients[0]), cwd=root, environment=environment)
        if sha256(jar) != first_hash or jar.stat().st_mtime_ns != first_timestamp:
            raise RuntimeError("Repeated public installation did not reuse the verified JAR.")
        verify_configuration(configuration_path(clients[0], root, home, environment), java, args, "mcpServers")
        print(f"Verified public shaft-mcp LATEST installer on {platform.system()}: {jar.parent.name}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
