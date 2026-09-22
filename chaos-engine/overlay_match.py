#!/usr/bin/env python3
"""Origin overlay must byte-match SOURCE for installer-owned files."""

from __future__ import annotations

import hashlib
import importlib.util
import json
import os
import secrets
import subprocess
import sys
from pathlib import Path

SOURCE_DIR = "chaos-engine"
OVERLAY_DIR = ".chaos-engine"
REPOSITORY_DISTRIBUTION = "repository"
OVERLAY_HANDOFF_RELATIVE = ".chaos-engine-state/overlay-handoff.md"


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


def owned_tree_differs_from_commit(project: Path, tree: Path, commit: str) -> list[str] | None:
    probe = subprocess.run(
        ["git", "-C", str(project), "cat-file", "-e", f"{commit}^{{commit}}"],
        capture_output=True,
        check=False,
    )
    if probe.returncode != 0:
        return None
    differing: list[str] = []
    source = project / "chaos-engine"
    for file in owned_source_files(source):
        relative = file.relative_to(source).as_posix()
        shown = subprocess.run(
            ["git", "-C", str(project), "show", f"{commit}:chaos-engine/{relative}"],
            capture_output=True,
            check=False,
        )
        current = tree / relative
        if shown.returncode != 0 or not current.is_file() or shown.stdout != current.read_bytes():
            differing.append(relative)
    return differing


def _manifest_commit(project: Path) -> str | None:
    manifest_path = project / OVERLAY_DIR / "manifest.json"
    try:
        manifest = json.loads(manifest_path.read_text(encoding="utf-8"))
    except (OSError, UnicodeDecodeError, json.JSONDecodeError):
        return None
    source = manifest.get("source") if isinstance(manifest, dict) else None
    commit = source.get("commit") if isinstance(source, dict) else None
    return commit if isinstance(commit, str) else None


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


def _doctor_command() -> str:
    cli = "py -3" if os.name == "nt" else "python3"
    return f"{cli} .chaos-engine/install.py doctor --project ."


def overlay_handoff_prompt(doctor_command: str | None = None) -> str:
    """One pasteable agentic prompt (merge-handoff UX)."""
    doctor = doctor_command or _doctor_command()
    return (
        "Heal ChaosEngine overlay using .chaos-engine-state/overlay-handoff.md. "
        "Copy only the listed owned files from chaos-engine/ to .chaos-engine/ "
        "(byte-identical; create parents as needed). Preserve every foreign "
        "overlay bit. Rewrite .chaos-engine/manifest.json files digests to match "
        f"the overlay. Then run {doctor} and follow each fix-next."
    )


def clear_overlay_handoff(project: Path) -> None:
    target = Path(project) / OVERLAY_HANDOFF_RELATIVE
    if target.is_file() and not target.is_symlink():
        try:
            target.unlink()
        except OSError:
            # Stale handoff cleanup must not fail doctor/install success.
            pass


def write_overlay_handoff(
    project: Path,
    mismatches: list[str],
    *,
    doctor_command: str | None = None,
    detail: str | None = None,
) -> Path:
    """Persist agent heal steps when deterministic SOURCE→overlay sync failed."""
    doctor = doctor_command or _doctor_command()
    target = Path(project) / OVERLAY_HANDOFF_RELATIVE
    target.parent.mkdir(parents=True, exist_ok=True)
    lines = [
        "# Overlay handoff",
        "",
        "Deterministic sync from local SOURCE could not make",
        "`.chaos-engine/` owned files byte-match `chaos-engine/`.",
        "",
    ]
    if detail:
        lines.extend([f"Detail: {detail}", ""])
    lines.extend(
        [
            "Copy only these owned paths from `chaos-engine/` → `.chaos-engine/`",
            "(byte-identical; create parents). Preserve foreign overlay bits.",
            "Then rewrite `.chaos-engine/manifest.json` `files` digests to match.",
            "",
        ]
    )
    if mismatches:
        lines.append("Mismatched owned paths:")
        lines.append("")
        for relative in mismatches:
            lines.append(f"- `{relative}`")
        lines.append("")
    else:
        lines.extend(["Mismatched owned paths: (see doctor core detail)", ""])
    lines.extend(
        [
            "Doctor:",
            "",
            f"`{doctor}`",
            "",
            "Agent prompt:",
            "",
            f"`{overlay_handoff_prompt(doctor)}`",
            "",
        ]
    )
    target.write_text("\n".join(lines), encoding="utf-8")
    return target


def _rewrite_manifest_files(overlay: Path, install) -> None:
    """Keep verify_install green after SOURCE→overlay byte sync."""
    manifest_name = str(getattr(install, "MANIFEST_NAME", "manifest.json"))
    manifest_path = overlay / manifest_name
    if not manifest_path.is_file():
        return
    if hasattr(install, "is_link_or_reparse") and install.is_link_or_reparse(manifest_path):
        raise ValueError(f"overlay manifest is a link or reparse point: {manifest_path}")
    try:
        manifest = json.loads(manifest_path.read_text(encoding="utf-8"))
    except (OSError, UnicodeDecodeError, json.JSONDecodeError) as error:
        raise ValueError(f"overlay manifest is missing or invalid: {manifest_path}") from error
    if not isinstance(manifest, dict):
        raise ValueError(f"overlay manifest is missing or invalid: {manifest_path}")
    payload = install.installed_payload(overlay)
    if manifest.get("files") == payload:
        return
    manifest["files"] = payload
    temporary = overlay / f"{manifest_name}.overlay-sync-{secrets.token_hex(8)}"
    try:
        temporary.write_text(
            json.dumps(manifest, indent=2, sort_keys=True) + "\n",
            encoding="utf-8",
        )
        temporary.replace(manifest_path)
    finally:
        if temporary.exists():
            temporary.unlink()


def sync_overlay_from_source(project: Path) -> dict[str, object]:
    """Copy owned SOURCE files into overlay (byte-identical) and refresh digests.

    Repository checkouts only. Adopters have no SOURCE tree and are a no-op.
    """
    root = project.resolve()
    if not is_repository_checkout(root):
        return {"synced": False, "scope": "adopter", "copiedCount": 0}
    source = root / SOURCE_DIR
    overlay = root / OVERLAY_DIR
    if not overlay.is_dir():
        return {
            "synced": False,
            "scope": "repository",
            "detail": "overlay-absent",
            "copiedCount": 0,
        }
    install = _install_module()
    if hasattr(install, "is_link_or_reparse") and install.is_link_or_reparse(overlay):
        raise ValueError(f"overlay is a link or reparse point: {overlay}")
    copied: list[str] = []
    for path in owned_source_files(source):
        if hasattr(install, "is_link_or_reparse") and install.is_link_or_reparse(path):
            raise ValueError(
                f"source contains a link or reparse point: {path.relative_to(source)}"
            )
        relative = path.relative_to(source)
        destination = overlay / relative
        if hasattr(install, "is_link_or_reparse") and destination.exists():
            if install.is_link_or_reparse(destination):
                raise ValueError(
                    f"overlay path is a link or reparse point: {relative.as_posix()}"
                )
        destination.parent.mkdir(parents=True, exist_ok=True)
        payload = path.read_bytes()
        if destination.is_file() and destination.read_bytes() == payload:
            continue
        destination.write_bytes(payload)
        copied.append(relative.as_posix())
    _rewrite_manifest_files(overlay, install)
    return {
        "synced": True,
        "scope": "repository",
        "copiedCount": len(copied),
        "copied": copied[:32],
    }


def apply_doctor_overlay_match(
    result: dict[str, object], project: Path
) -> None:
    """Record coreMatchesSource; heal once from SOURCE before recovery-required."""
    components = result.get("components")
    if not isinstance(components, dict):
        return
    matched = core_matches_source(project)
    core = components.get("core")
    if not isinstance(core, dict):
        return

    commit = _manifest_commit(Path(project))
    if commit:
        overlay_diff = owned_tree_differs_from_commit(
            Path(project), Path(project) / OVERLAY_DIR, commit
        )
        source_diff = owned_tree_differs_from_commit(
            Path(project), Path(project) / SOURCE_DIR, commit
        )
        if overlay_diff is not None and source_diff is not None:
            if not overlay_diff:
                core["coreMatchesSource"] = True
                clear_overlay_handoff(Path(project))
                return
            if source_diff:
                result["status"] = "recovery-required"
                core["status"] = "recovery-required"
                core["detail"] = "overlay-commit-mismatch"
                core["coreMatchesSource"] = False
                core["fixNext"] = (
                    "Owned overlay bytes differ from the manifest commit and local "
                    "SOURCE is not that commit. Do not rewrite manifest file digests."
                )
                return

    heal_error: str | None = None
    if matched.get("scope") == "repository" and not matched.get("coreMatchesSource"):
        try:
            sync_overlay_from_source(project)
            matched = core_matches_source(project)
        except (OSError, RuntimeError, ValueError, ImportError) as error:
            heal_error = str(error)
            matched = core_matches_source(project)

    core["coreMatchesSource"] = bool(matched.get("coreMatchesSource"))
    if matched.get("scope") != "repository" or matched.get("coreMatchesSource"):
        clear_overlay_handoff(project)
        return

    mismatches = matched.get("mismatches")
    mismatch_list = (
        [str(item) for item in mismatches] if isinstance(mismatches, list) else []
    )
    detail = heal_error or str(matched.get("detail") or "overlay-source-mismatch")
    write_overlay_handoff(project, mismatch_list, detail=detail)
    result["status"] = "recovery-required"
    core["status"] = "recovery-required"
    core["detail"] = "overlay-source-mismatch"
    core["fixNext"] = (
        "Complete the agent overlay heal using .chaos-engine-state/overlay-handoff.md, "
        "then rerun doctor. Do not rerun the install one-liner."
    )
    core["agentPrompt"] = overlay_handoff_prompt()
