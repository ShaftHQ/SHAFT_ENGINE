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
    "memory-mcp": ["--help"],
}
PHASE_TIMEOUT_SECONDS = 600
MCP_START_TIMEOUT_SECONDS = 10
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
        raise RuntimeError(f"command failed ({result.returncode}): {sanitize(detail)}")
    return result


def read_json(path: Path) -> dict[str, object]:
    value = json.loads(path.read_text(encoding="utf-8"))
    if not isinstance(value, dict):
        raise RuntimeError(f"expected JSON object: {path.name}")
    return value


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
        environment=download_environment(),
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


def probe_mempalace_mcp(tool: Path, project: Path) -> None:
    command = [
        sys.executable,
        str(tool),
        "mempalace-mcp",
        "--palace",
        ".chaos-engine-state/mempalace",
        "--backend",
        "sqlite_exact",
    ]
    process = subprocess.Popen(  # nosec B603
        command,
        cwd=project,
        env={**clean_environment(), "PYTHONDONTWRITEBYTECODE": "1"},
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
    )
    try:
        stdout, stderr = process.communicate(timeout=MCP_START_TIMEOUT_SECONDS)
    except subprocess.TimeoutExpired:
        process.terminate()
        try:
            process.communicate(timeout=5)
        except subprocess.TimeoutExpired:
            process.kill()
            process.communicate(timeout=5)
        return
    if process.returncode:
        raise RuntimeError(
            f"mempalace-mcp exited during startup: {sanitize(stderr or stdout)}"
        )


def verify_phase(project: Path, expected_commit: str) -> dict[str, object]:
    installed = project / ".chaos-engine"
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
    probe_mempalace_mcp(tool, project)
    dispatches["mempalace-mcp"] = "pass"

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
        if "Python 3.10." not in output:
            raise RuntimeError(f"{name} is not using managed Python 3.10")
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
        "managedPython": "3.10",
        "cacheState": "absent",
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
        project = root / "consumer with spaces Ω"
        project.mkdir()

        def install_and_verify(
            commit: str, *, require_current_action: bool = True
        ) -> dict[str, object]:
            run_public_wrapper(
                commit, project, require_current_action=require_current_action
            )
            return verify_phase(project, commit)

        fresh = record_phase(
            evidence,
            "fresh-base-wrapper",
            lambda: install_and_verify(base_sha, require_current_action=False),
        )
        first_pointer = (project / ".chaos-engine-runtime-current.json").read_bytes()
        first_generations = sorted(
            path.name for path in (project / ".chaos-engine-runtime-generations").iterdir()
        )
        offline_source = download_commit_source(
            source, base_sha, root / "offline-base-source"
        )

        def healthy_rerun() -> dict[str, object]:
            run_offline_rerun(project, offline_source)
            if first_pointer != (project / ".chaos-engine-runtime-current.json").read_bytes():
                raise RuntimeError("healthy rerun rewrote active pointer")
            if first_generations != sorted(
                path.name
                for path in (project / ".chaos-engine-runtime-generations").iterdir()
            ):
                raise RuntimeError("healthy rerun built a dependency generation")
            return verify_phase(project, base_sha)

        record_phase(evidence, "healthy-offline-rerun-base", healthy_rerun)
        upgrade = record_phase(
            evidence,
            "upgrade-candidate-wrapper",
            lambda: install_and_verify(candidate_sha),
        )

        damaged_id = str(upgrade["active"])
        damaged_root = project / ".chaos-engine-runtime-generations" / damaged_id
        damaged_receipt = read_json(damaged_root / "receipt.json")
        for name in ("graphify", "mempalace"):
            dispatch = damaged_receipt["tools"][name]["dispatch"]
            (damaged_root / dispatch["interpreter"]).unlink()

        repaired = record_phase(
            evidence,
            "repair-candidate-wrapper",
            lambda: install_and_verify(candidate_sha),
        )
        if repaired["active"] == damaged_id or repaired["previous"] != fresh["active"]:
            raise RuntimeError("repair did not retire damaged active and retain valid A")

        def offline_rollback() -> dict[str, object]:
            installed = project / ".chaos-engine/install.py"
            run_checked(
                [sys.executable, str(installed), "rollback", "--project", str(project)],
                cwd=project,
                environment=offline_environment(block_path=False),
            )
            checks = verify_phase(project, base_sha)
            if checks["active"] != fresh["active"]:
                raise RuntimeError("offline rollback did not reactivate generation A")
            return checks

        record_phase(evidence, "offline-rollback-base", offline_rollback)


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
