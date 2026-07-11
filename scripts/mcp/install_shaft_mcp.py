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
import urllib.parse
import urllib.request
import xml.etree.ElementTree as ET
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
WORKSPACE_SYSTEM_PROPERTY = "shaft.mcp.workspaceRoot"
USER_GUIDE_URL = "https://shafthq.github.io/docs/agentic/mcp"
BOOTSTRAP_BANNER_SHOWN = "SHAFT_MCP_BOOTSTRAP_BANNER_SHOWN"
SHAFT_SKILLS_DIRECTORY = "shaft-skills"
SHAFT_SKILLS_SOURCE_FILES = (
    "evaluation-prompts.md",
    "writing-shaft-tests/SKILL.md",
    "writing-shaft-tests/agents/openai.yaml",
    "choosing-shaft-locators/SKILL.md",
    "choosing-shaft-locators/agents/openai.yaml",
    "recording-shaft-tests-with-mcp/SKILL.md",
    "recording-shaft-tests-with-mcp/agents/openai.yaml",
    "analyzing-shaft-failures/SKILL.md",
    "analyzing-shaft-failures/agents/openai.yaml",
    "verifying-and-applying-shaft-changes/SKILL.md",
    "verifying-and-applying-shaft-changes/agents/openai.yaml",
)
SHAFT_SKILLS_SOURCE_MARKERS = (
    "writing-shaft-tests/SKILL.md",
    "choosing-shaft-locators/SKILL.md",
    "recording-shaft-tests-with-mcp/SKILL.md",
    "analyzing-shaft-failures/SKILL.md",
    "verifying-and-applying-shaft-changes/SKILL.md",
)
AGENT_VALIDATION_SCRIPT_FILES = (
    "scripts/ci/validate_agent_setup.py",
    "scripts/ci/validate_agent_guidance.py",
    "scripts/ci/validate_documentation_boundaries.py",
    "scripts/ci/agent_guidance_budget.json",
)
AGENT_GUIDANCE_SCAFFOLD_MARKER = "AGENTS.md"
TARGETS = ("codex", "claude", "claude-desktop", "copilot", "copilot-intellij", "intellij-plugin")
TARGET_CHOICES = (
    ("codex", "Codex CLI / IDE"),
    ("claude", "Claude Code"),
    ("claude-desktop", "Claude Desktop"),
    ("copilot", "GitHub Copilot CLI"),
    ("copilot-intellij", "GitHub Copilot for IntelliJ IDEA"),
    ("intellij-plugin", "SHAFT IntelliJ IDEA plugin"),
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
              MCP installer
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
        print("Enter a number from 1 to 6, or a target name.")


def parse_args(argv: list[str]) -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Install and configure shaft-mcp for a supported MCP client.",
        formatter_class=argparse.RawTextHelpFormatter,
    )
    parser.add_argument("--client", choices=TARGETS)
    env_version = (os.environ.get("SHAFT_MCP_VERSION") or "").strip()
    parser.add_argument("--version", nargs="?", const="LATEST", default=env_version or "LATEST")
    parser.add_argument("--json", action="store_true", help="Print machine-readable install details to stdout.")
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
    for target, _ in TARGET_CHOICES:
        parser.add_argument(f"--{target}", action="store_true", dest=target.replace("-", "_"))
    parser.add_argument(
        "target",
        nargs="?",
        help="Optional target name: codex, claude, claude-desktop, copilot, or copilot-intellij.",
    )
    args = parser.parse_args(argv)

    # Normalize empty or whitespace-only version to LATEST
    if isinstance(args.version, str):
        args.version = (args.version.strip() or "LATEST")

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
    if args.install_shaft_skills and args.skip_shaft_skills:
        fail("Specify only one of --install-shaft-skills or --skip-shaft-skills.", 2)
    args.client = selected[0] if selected else choose_client()
    if args.client not in TARGETS:
        fail("Usage: install-shaft-mcp.py [--client <codex|claude|claude-desktop|copilot|copilot-intellij|intellij-plugin>]", 2)
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


def download_file(
        url: str,
        output: Path,
        label: str | None = None,
        show_progress: bool = True,
        announce: bool = True) -> None:
    output.parent.mkdir(parents=True, exist_ok=True)
    temporary = output.with_name(f".{output.name}.{os.getpid()}.tmp")
    headers = {"User-Agent": "shaft-mcp-installer"}
    last_error: BaseException | None = None
    display = label or output.name
    for attempt in range(1, 6):
        try:
            request = urllib.request.Request(url, headers=headers)
            if announce:
                log(f"Downloading {display}...")
            with urllib.request.urlopen(request, timeout=120) as response, temporary.open("wb") as target:
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
        except (urllib.error.URLError, TimeoutError, OSError) as exc:
            last_error = exc
            temporary.unlink(missing_ok=True)
            time.sleep(min(attempt * 2, 10))
    assert last_error is not None
    raise last_error


def url_text(url: str) -> str:
    return download_bytes(url).decode("utf-8")


def url_text_or_none(url: str) -> str | None:
    headers = {"User-Agent": "shaft-mcp-installer"}
    request = urllib.request.Request(url, headers=headers)
    try:
        with urllib.request.urlopen(request, timeout=120) as response:
            return response.read().decode("utf-8")
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
    return path.is_dir() and all((path / marker).is_file() for marker in SHAFT_SKILLS_SOURCE_MARKERS)


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
    for item in source.iterdir():
        destination = target / item.name
        if item.is_dir():
            shutil.copytree(item, destination, dirs_exist_ok=True)
        elif item.is_file():
            shutil.copy2(item, destination)
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
    for relative in SHAFT_SKILLS_SOURCE_FILES:
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
    # Verify the main entry point was downloaded
    main_script = target / "scripts" / "ci" / "validate_agent_setup.py"
    if not main_script.is_file():
        fail("Agent validation script did not download correctly.", 4)
    return target / "scripts" / "ci"


def install_shaft_skills(current_directory: Path, root: Path) -> Path:
    target = current_directory.resolve() / SHAFT_SKILLS_DIRECTORY
    source = local_shaft_skills_source()
    if source is not None:
        return copy_shaft_skills(source, target)
    return download_shaft_skills_files(target)


def should_install_shaft_skills(args: argparse.Namespace, current_directory: Path) -> bool:
    if args.install_shaft_skills:
        return True
    if args.skip_shaft_skills:
        return False
    if not sys.stdin.isatty():
        log(f"Installing SHAFT skills into current directory by default: {current_directory}")
        return True
    print(
        f"Install SHAFT skills into the current directory?\n  {current_directory}\n[Y/n]: ",
        end="",
        file=sys.stderr,
        flush=True,
    )
    try:
        answer = input().strip().lower()
    except EOFError:
        return True
    return answer not in {"n", "no"}


def java_argfile_quote(value: str) -> str:
    return '"' + value.replace("\\", "/").replace('"', '\\"') + '"'


def write_launcher_args(jar: Path, dependencies: list[Path]) -> Path:
    args_file = jar.parent / "shaft-mcp.args"
    runtime_root = application_data_root() / "work"
    runtime_root.mkdir(parents=True, exist_ok=True)
    classpath = os.pathsep.join(str(path.resolve()) for path in [jar, *dependencies])
    content = "\n".join((
        java_argfile_quote(f"-Duser.dir={runtime_root}"),
        java_argfile_quote(f"-D{WORKSPACE_SYSTEM_PROPERTY}={runtime_root}"),
        "-cp",
        java_argfile_quote(classpath),
        MAIN_CLASS,
    )) + "\n"
    temporary = args_file.with_name(f".{args_file.name}.{os.getpid()}.tmp")
    temporary.write_text(content, encoding="utf-8", newline="\n")
    os.replace(temporary, args_file)
    return args_file.resolve()


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
    fail("Timed out while probing the installed shaft-mcp launcher.", 4)


def probe_stdio(java: Path, args_file: Path) -> None:
    process = subprocess.Popen(
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
    else:
        fail(f"Unsupported client: {client}", 2)


def activation_hint(client: str) -> str:
    if client == "intellij-plugin":
        return "Return to the SHAFT IntelliJ IDEA plugin and test the connection."
    if client == "claude-desktop":
        return "Restart Claude Desktop, then open a new chat and use the shaft-mcp tools."
    if client == "copilot-intellij":
        return "Restart IntelliJ IDEA or reload Copilot Chat, then use the shaft-mcp tools."
    return "Start a fresh client session, then use the shaft-mcp tools."


def install(args: argparse.Namespace) -> None:
    banner()
    root = bootstrap_root()
    root.mkdir(parents=True, exist_ok=True)
    repository = os.environ.get("SHAFT_MCP_REPOSITORY_URL", DEFAULT_REPOSITORY).rstrip("/")
    log(f"Client target: {args.client}")

    java = get_java25(root)
    java_home = java_home_for(java)
    os.environ["JAVA_HOME"] = str(java_home)
    os.environ["PATH"] = f"{java.parent}{os.pathsep}{os.environ.get('PATH', '')}"

    if args.client != "intellij-plugin":
        detect_project_override(args.client)
    version = resolve_shaft_mcp_version(args.version, repository, root)
    log(f"Installing io.github.shafthq:shaft-mcp:{version}")
    jar = install_shaft_mcp_jar(version, repository, root)
    dependencies = install_runtime_dependencies(jar, repository)
    args_file = write_launcher_args(jar, dependencies)

    log(f"Verifying shaft-mcp {version} over stdio...")
    probe_stdio(java, args_file)

    if args.client != "intellij-plugin":
        log(f"Configuring shaft-mcp for {args.client}...")
        configure_client(args.client, java, args_file)
    current_directory = Path.cwd().resolve()
    skills_path = current_directory / SHAFT_SKILLS_DIRECTORY
    skills_installed = False
    validation_script_dir = None
    if should_install_shaft_skills(args, current_directory):
        log(f"Installing SHAFT skills to {skills_path}...")
        skills_path = install_shaft_skills(current_directory, root)
        skills_installed = True
        if has_agent_guidance_scaffold(current_directory):
            log("Fetching agent validation script files...")
            validation_script_dir = download_agent_validation_script_files(current_directory)
        else:
            log(f"Skipped agent validation scripts: no {AGENT_GUIDANCE_SCAFFOLD_MARKER} guidance "
                f"scaffold found in {current_directory}.")
    else:
        log(f"Skipped SHAFT skills installation for {skills_path}.")
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
        },
    }
    if validation_script_dir:
        result["agentValidationScript"] = {
            "installed": True,
            "path": str(validation_script_dir),
        }
    if args.json:
        print(json.dumps(result, separators=(",", ":")))
    else:
        action = "installed and ready for" if args.client == "intellij-plugin" else "installed and configured for"
        print(f"shaft-mcp {version} is {action} {args.client}.")
        if skills_installed:
            print(f"SHAFT skills installed at {skills_path}.")
            if validation_script_dir:
                print(f"Agent validation script files installed at {validation_script_dir}.")
        else:
            print(f"SHAFT skills installation skipped for {skills_path}.")
        print(activation_hint(args.client))
        print(f"User guide: {USER_GUIDE_URL}")


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
