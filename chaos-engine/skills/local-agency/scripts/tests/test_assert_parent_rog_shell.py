"""Smoke tests for assert_parent_rog_shell (#6051)."""

from __future__ import annotations

import importlib.util
import json
import socket
import subprocess
import sys
from pathlib import Path

SCRIPTS = Path(__file__).resolve().parents[1]
ASSERT = SCRIPTS / "assert_parent_rog_shell.py"


def _load(name: str, path: Path):
    spec = importlib.util.spec_from_file_location(name, path)
    assert spec and spec.loader
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


def test_script_runs_and_emits_json():
    proc = subprocess.run(
        [sys.executable, str(ASSERT), "--json-only"],
        check=False,
        capture_output=True,
        text=True,
    )
    payload = json.loads(proc.stdout)
    assert payload["gate"] == "rog-freetoken"
    assert "platform_gap" in payload
    assert payload["issue"] == "#6051"
    # On ROG expect 0; on box expect 2
    host = socket.gethostname().lower()
    if "rog" in host:
        assert proc.returncode == 0
        assert payload.get("state") == "READY"
    else:
        assert proc.returncode == 2
        assert payload.get("parent_required") is True


if __name__ == "__main__":
    test_script_runs_and_emits_json()
    print("ok")
