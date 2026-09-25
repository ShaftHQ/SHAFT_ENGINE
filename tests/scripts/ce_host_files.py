"""Host files for contract tests without depending on an installer run (#6197).

Generated host files (`.codex/config.toml`, `.claude/settings.json`, host
skill adapters, ...) are untracked. A fresh worktree or clone has none of them,
so tests read the checkout copy when present and otherwise render the same
bytes the installer would write, from `chaos-engine/hosts.py`.
"""

from __future__ import annotations

import importlib.util
import sys
from functools import lru_cache
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]


@lru_cache(maxsize=1)
def _rendered() -> dict[str, bytes]:
    name = "ce_host_files_hosts"
    spec = importlib.util.spec_from_file_location(name, ROOT / "chaos-engine/hosts.py")
    if spec is None or spec.loader is None:
        raise ImportError("chaos-engine/hosts.py")
    hosts = importlib.util.module_from_spec(spec)
    sys.modules[name] = hosts
    spec.loader.exec_module(hosts)
    before = {relative: None for relative in hosts.managed_paths()}
    rendered = hosts.desired_content(before, maven_runtime=None, project=ROOT)
    return {relative: content for relative, content in rendered.items() if isinstance(content, bytes)}


def host_file_path(relative: str) -> Path:
    """Checkout path when it exists; otherwise a rendered copy under a cache dir."""
    path = ROOT / relative
    if path.is_file():
        return path
    cache = ROOT / "target/ce-host-files" / relative
    cache.parent.mkdir(parents=True, exist_ok=True)
    cache.write_bytes(host_file_bytes(relative))
    return cache


def host_file_bytes(relative: str) -> bytes:
    path = ROOT / relative
    if path.is_file():
        return path.read_bytes()
    rendered = _rendered()
    if relative not in rendered:
        raise FileNotFoundError(f"{relative} is neither in the checkout nor rendered by hosts.py")
    return rendered[relative]


def host_file_text(relative: str) -> str:
    return host_file_bytes(relative).decode("utf-8")


def installed_overlay_text(relative: str) -> str:
    """`.chaos-engine/<relative>` when installed; else the source it is copied from."""
    installed = ROOT / ".chaos-engine" / relative
    source = ROOT / "chaos-engine" / relative
    return (installed if installed.is_file() else source).read_text(encoding="utf-8")


def host_link_targets(relative: str) -> list[Path]:
    """Markdown link targets of a host file, resolved from where it is installed.

    A rendered copy lives under a cache dir, so its relative links would resolve
    nowhere; resolve them from `ROOT/relative` instead and fall back from the
    installed `.chaos-engine/` overlay to its `chaos-engine/` source.
    """
    import re

    anchor = (ROOT / relative).parent
    overlay = ROOT / ".chaos-engine"
    targets = []
    for raw in re.findall(r"(?<!!)\[[^]]*\]\(([^)]+)\)", host_file_text(relative)):
        target = raw.strip().strip("<>").split("#", 1)[0]
        if not target or re.match(r"^[a-z][a-z0-9+.-]*:", target, re.I):
            continue
        resolved = (anchor / target).resolve()
        if not resolved.exists() and resolved.is_relative_to(overlay):
            resolved = ROOT / "chaos-engine" / resolved.relative_to(overlay)
        targets.append(resolved)
    return targets
