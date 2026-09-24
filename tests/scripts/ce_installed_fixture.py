"""Installed empty-project fixture shared by the #6173 contract tests.

The fixture copies the portable payload exactly as ``install.py`` stages it
(``source_files(..., "portable")``) into ``<tmp>/.chaos-engine``. It never
runs the installer, never touches the network, and never reads untracked
host files from the checkout, so it behaves the same in a fresh worktree,
a fresh clone, and CI (#6197).
"""

from __future__ import annotations

import importlib.util
import json
import shutil
import sys
from functools import lru_cache
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
SOURCE = ROOT / "chaos-engine"
PARITY = ROOT / "scripts/ci/agent_harness_parity.json"
HOOK_HOSTS = ("claude", "codex", "gemini", "grok", "copilot")
READ_EVENTS = {
    "claude": {"hook_event_name": "PreToolUse", "tool_name": "Read"},
    "codex": {"hook_event_name": "PreToolUse", "tool_name": "read_file"},
    "gemini": {"hook_event_name": "BeforeTool", "tool_name": "read_file"},
    "grok": {"hook_event_name": "pre_tool_use", "tool_name": "read_file"},
    "copilot": {"hook_event_name": "preToolUse", "toolName": "read_file"},
}


def load_module(name: str, path: Path):
    spec = importlib.util.spec_from_file_location(name, path)
    if spec is None or spec.loader is None:
        raise ImportError(path)
    module = importlib.util.module_from_spec(spec)
    sys.modules[name] = module  # dataclasses resolve their module during exec.
    spec.loader.exec_module(module)
    return module


@lru_cache(maxsize=1)
def installer():
    return load_module("ce_fixture_install", SOURCE / "install.py")


@lru_cache(maxsize=1)
def installed_layout() -> frozenset[str]:
    """Relative paths under `.chaos-engine/` in a portable install."""
    files = installer().source_files(SOURCE, "portable")
    return frozenset(path.relative_to(SOURCE).as_posix() for path in files)


def parity_hosts() -> list[str]:
    return list(json.loads(PARITY.read_text(encoding="utf-8"))["hosts"])


def build_installed_project(target: Path) -> Path:
    """Materialize `.chaos-engine/` plus one project source file."""
    tree = target / ".chaos-engine"
    for relative in sorted(installed_layout()):
        destination = tree / relative
        destination.parent.mkdir(parents=True, exist_ok=True)
        shutil.copyfile(SOURCE / relative, destination)
    (target / "src/sub").mkdir(parents=True, exist_ok=True)
    (target / "src/Foo.java").write_text("class Foo {}\n", encoding="utf-8")
    (target / "src/sub/Bar.java").write_text("class Bar {}\n", encoding="utf-8")
    return target
