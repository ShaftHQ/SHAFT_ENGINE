#!/usr/bin/env python3
"""Origin overlay must byte-match SOURCE for installer-owned files."""

from __future__ import annotations

import hashlib
import importlib.util
import sys
from pathlib import Path

SOURCE_DIR = "chaos-engine"
OVERLAY_DIR = ".chaos-engine"
REPOSITORY_DISTRIBUTION = "repository"


def _sha256(path: Path) -> str:
    return hashlib.sha256(path.read_bytes()).hexdigest()


def is_repository_checkout(project: Path) -> bool:
    return (project / SOURCE_DIR / "skills/chaos-engine/SKILL.md").is_file()


def _install_module():
    """Load sibling install.py without creating an import cycle."""
    install_path = Path(__file__).resolve().with_name("install.py")
    for module in list(sys.modules.values()):
        file_name = getattr(module, "__file__", None)
        if not file_name:
            continue
        try:
            if Path(file_name).resolve() == install_path and hasattr(module, "source_files"):
                return module
        except OSError:
            continue
    spec = importlib.util.spec_from_file_location(
        "ce_install_for_overlay_match", install_path
    )
    if spec is None or spec.loader is None:
        raise ImportError(f"unable to load ChaosEngine installer: {install_path}")
    module = importlib.util.module_from_spec(spec)
    sys.modules[spec.name] = module
    spec.loader.exec_module(module)
    return module


def owned_source_files(source: Path) -> tuple[Path, ...]:
    """Same payload source_files() copies for distribution=repository."""
    return _install_module().source_files(source, REPOSITORY_DISTRIBUTION)


def core_matches_source(project: Path) -> dict[str, object]:
    """Compare overlay owned files to SOURCE. Adopters have no SOURCE tree."""
    root = project.resolve()
    if not is_repository_checkout(root):
        return {"coreMatchesSource": True, "scope": "adopter"}
    source = root / SOURCE_DIR
    overlay = root / OVERLAY_DIR
    if not overlay.is_dir():
        return {
            "coreMatchesSource": False,
            "scope": "repository",
            "detail": "overlay-absent",
        }
    mismatches: list[str] = []
    for path in owned_source_files(source):
        relative = path.relative_to(source).as_posix()
        other = overlay / relative
        if not other.is_file() or _sha256(path) != _sha256(other):
            mismatches.append(relative)
    return {
        "coreMatchesSource": not mismatches,
        "scope": "repository",
        "mismatches": mismatches[:8],
    }


def apply_doctor_overlay_match(
    result: dict[str, object], project: Path
) -> None:
    """Record coreMatchesSource; flip recovery-required only on mismatch."""
    components = result.get("components")
    if not isinstance(components, dict):
        return
    matched = core_matches_source(project)
    core = components.get("core")
    if not isinstance(core, dict):
        return
    core["coreMatchesSource"] = bool(matched.get("coreMatchesSource"))
    if matched.get("scope") == "repository" and not matched.get("coreMatchesSource"):
        result["status"] = "recovery-required"
        core["status"] = "recovery-required"
        core["detail"] = "overlay-source-mismatch"
        core["fixNext"] = (
            "Reinstall so .chaos-engine owned files match chaos-engine/."
        )
