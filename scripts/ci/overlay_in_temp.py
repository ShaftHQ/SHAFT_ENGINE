"""Materialize a generated ChaosEngine overlay in a temporary project (#5713)."""

from __future__ import annotations

import importlib.util
import shutil
import tempfile
from pathlib import Path


def load_installer(source: Path):
    path = source / "install.py"
    spec = importlib.util.spec_from_file_location("ce_overlay_temp_install", path)
    if spec is None or spec.loader is None:
        raise RuntimeError(f"cannot load installer: {path}")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def materialize_overlay(origin: Path) -> Path:
    source = origin / "chaos-engine"
    installer = load_installer(source)
    temporary = Path(tempfile.mkdtemp(prefix="ce-overlay-"))
    installer.install_with_dependencies(
        temporary,
        source,
        "0" * 40,
        provisioner=lambda *_a, **_k: None,
    )
    return temporary


GENERATED_TREES = (
    ".chaos-engine",
    ".agents",
    ".claude",
    ".claude-plugin",
    ".codex",
    ".gemini",
    ".grok",
    ".github/skills",
    ".github/hooks",
    "plugins/chaos-engine",
    "plugins/caveman",
    "plugins/ponytail",
)


def ensure_overlay(root: Path) -> Path:
    pointer = root / ".agents/skills/chaos-engine/SKILL.md"
    if pointer.is_file():
        return root
    if not (root / "chaos-engine/install.py").is_file():
        return root
    overlay = materialize_overlay(root)
    try:
        for relative in GENERATED_TREES:
            source = overlay / relative
            destination = root / relative
            if not source.exists() or destination.exists():
                continue
            if source.is_dir():
                shutil.copytree(source, destination)
            else:
                destination.parent.mkdir(parents=True, exist_ok=True)
                shutil.copy2(source, destination)
    finally:
        cleanup_overlay(overlay)
    return root


def cleanup_overlay(path: Path) -> None:
    shutil.rmtree(path, ignore_errors=True)
