#!/usr/bin/env python3
"""Run bounded real-tool acceptance for the immutable ChaosEngine installer."""

from __future__ import annotations

import argparse
import ast
import json
import os
import platform
import re
import shutil
import subprocess  # nosec B404 - fixed repository-owned commands only.
import sys
import tempfile
import time
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
COMMIT_A = "a" * 40
COMMIT_B = "b" * 40
HEX_ID = re.compile(r"[0-9a-f]{32}")
SECRET_NAME = re.compile(r"(?:TOKEN|SECRET|PASSWORD|API_KEY|PRIVATE_KEY)", re.I)


def clean_environment(base: dict[str, str] | None = None) -> dict[str, str]:
    return {
        key: value
        for key, value in (base or os.environ).items()
        if SECRET_NAME.search(key) is None
    }


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


def sanitize(value: object) -> str:
    text = str(value)
    text = re.sub(r"[A-Za-z]:\\[^\r\n\t\"']+", "<path>", text)
    text = re.sub(
        r"/(?:tmp|private/var/folders|home/runner/work)/[^\r\n\t\"']+",
        "<path>",
        text,
    )
    return text[:500]


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


def install_command(source: Path, project: Path, commit: str) -> list[str]:
    return [
        sys.executable,
        str(source / "install.py"),
        "install",
        "--project",
        str(project),
        "--source",
        str(source),
        "--commit",
        commit,
        "--distribution",
        "portable",
    ]


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
    doctor = json.loads(
        run_checked(
            [sys.executable, str(installed / "install.py"), "doctor", "--project", str(project)],
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
        "doctor": "healthy",
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


def run_acceptance(source: Path, evidence: dict[str, object]) -> None:
    source = source.resolve()
    if shutil.which("node") is None or shutil.which("npm") is None:
        raise RuntimeError("Node.js and npm are required")
    with tempfile.TemporaryDirectory(prefix="chaos-engine-live-") as temporary:
        root = Path(temporary)
        staged = stage_source(source, root / "source")
        project = root / "consumer with spaces Ω"
        project.mkdir()

        fresh = record_phase(
            evidence,
            "fresh-a",
            lambda: (
                run_checked(install_command(staged, project, COMMIT_A), cwd=project),
                verify_phase(project, COMMIT_A),
            )[1],
        )
        first_pointer = (project / ".chaos-engine-runtime-current.json").read_bytes()
        first_generations = sorted(
            path.name for path in (project / ".chaos-engine-runtime-generations").iterdir()
        )

        def healthy_rerun() -> dict[str, object]:
            run_checked(
                install_command(staged, project, COMMIT_A),
                cwd=project,
                environment=offline_environment(block_path=True),
                timeout=180,
            )
            if first_pointer != (project / ".chaos-engine-runtime-current.json").read_bytes():
                raise RuntimeError("healthy rerun rewrote active pointer")
            if first_generations != sorted(
                path.name
                for path in (project / ".chaos-engine-runtime-generations").iterdir()
            ):
                raise RuntimeError("healthy rerun built a dependency generation")
            return verify_phase(project, COMMIT_A)

        record_phase(evidence, "healthy-offline-rerun-a", healthy_rerun)
        upgrade = record_phase(
            evidence,
            "upgrade-b",
            lambda: (
                run_checked(install_command(staged, project, COMMIT_B), cwd=project),
                verify_phase(project, COMMIT_B),
            )[1],
        )

        damaged_id = str(upgrade["active"])
        damaged_root = project / ".chaos-engine-runtime-generations" / damaged_id
        damaged_receipt = read_json(damaged_root / "receipt.json")
        for name in ("graphify", "mempalace"):
            dispatch = damaged_receipt["tools"][name]["dispatch"]
            (damaged_root / dispatch["interpreter"]).unlink()

        repaired = record_phase(
            evidence,
            "repair-b",
            lambda: (
                run_checked(install_command(staged, project, COMMIT_B), cwd=project),
                verify_phase(project, COMMIT_B),
            )[1],
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
            checks = verify_phase(project, COMMIT_A)
            if checks["active"] != fresh["active"]:
                raise RuntimeError("offline rollback did not reactivate generation A")
            return checks

        record_phase(evidence, "offline-rollback-a", offline_rollback)


def write_evidence(path: Path, evidence: dict[str, object]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(evidence, indent=2, sort_keys=True) + "\n", encoding="utf-8")


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--source", type=Path, default=ROOT / "chaos-engine")
    parser.add_argument(
        "--output", type=Path, default=Path("chaos-engine-live-installer-evidence.json")
    )
    args = parser.parse_args(argv)
    evidence: dict[str, object] = {
        "schemaVersion": 1,
        "accepted": False,
        "platform": platform.system(),
        "python": platform.python_version(),
        "phases": [],
    }
    try:
        run_acceptance(args.source, evidence)
    except BaseException as error:
        evidence["failure"] = {"type": type(error).__name__, "detail": sanitize(error)}
        write_evidence(args.output, evidence)
        return 1
    evidence["accepted"] = True
    write_evidence(args.output, evidence)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
