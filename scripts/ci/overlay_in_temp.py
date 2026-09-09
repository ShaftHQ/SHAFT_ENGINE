"""Materialize a generated ChaosEngine overlay in a temporary project (#5713)."""

from __future__ import annotations

import atexit
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


def materialize_overlay(origin: Path, *, copy_pom: bool = False) -> Path:
    source = origin / "chaos-engine"
    installer = load_installer(source)
    temporary = Path(tempfile.mkdtemp(prefix="ce-overlay-"))
    if copy_pom:
        pom = origin / "pom.xml"
        if pom.is_file():
            shutil.copy2(pom, temporary / "pom.xml")
    installer.install_with_dependencies(
        temporary,
        source,
        "0" * 40,
        provisioner=lambda *_a, **_k: None,
    )
    return temporary


def ensure_overlay(root: Path) -> Path:
    """Return a generated overlay tree. Never mutates origin."""
    if not (root / "chaos-engine/install.py").is_file():
        return root
    return materialize_overlay(root, copy_pom=False)


_session_overlay: Path | None = None


def session_overlay(origin: Path) -> Path:
    """Reuse one temp overlay per process for tests; never writes into origin."""
    global _session_overlay
    if _session_overlay is None:
        _session_overlay = ensure_overlay(origin)
        if _session_overlay != origin:
            atexit.register(cleanup_overlay, _session_overlay)
    return _session_overlay


def cleanup_overlay(path: Path) -> None:
    shutil.rmtree(path, ignore_errors=True)
