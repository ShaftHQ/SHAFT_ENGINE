#!/usr/bin/env python3
"""Origin overlay must byte-match SOURCE for installer-owned files."""

from __future__ import annotations

import hashlib
import importlib.util
import io
import json
import os
import re
import secrets
import subprocess  # nosec B404 - git show of a validated commit, no shell
import sys
import tarfile
import tempfile
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


_GIT_COMMIT = re.compile(r"[0-9a-f]{40}")
_GIT_RELATIVE = re.compile(r"[A-Za-z0-9_./+-]+")


def _git_show_owned(project: Path, commit: str, relative: str) -> bytes | None:
    """Return the blob for one owned path, or None when git cannot show it."""
    if _GIT_COMMIT.fullmatch(commit) is None or _GIT_RELATIVE.fullmatch(relative) is None:
        return None
    if ".." in Path(relative).parts:
        return None
    shown = subprocess.run(  # nosec B603 B607 - fixed git binary, validated commit and path
        ["git", "-C", str(project), "show", f"{commit}:chaos-engine/{relative}"],
        capture_output=True,
        check=False,
        shell=False,
    )
    if shown.returncode != 0:
        return None
    return shown.stdout


def _owned_rels_at_commit(project: Path, commit: str) -> list[str] | None:
    """Owned paths from the commit tree, including paths the working tree lacks."""
    archived = subprocess.run(  # nosec B603 B607 - fixed git archive of one tree
        ["git", "-C", str(project), "archive", commit, "chaos-engine"],
        capture_output=True,
        check=False,
        shell=False,
    )
    if archived.returncode != 0 or not archived.stdout:
        return None
    with tempfile.TemporaryDirectory() as temporary:
        with tarfile.open(fileobj=io.BytesIO(archived.stdout), mode="r:") as bundle:
            bundle.extractall(temporary, filter="data")  # nosec B202 - archive of our own commit
        source = Path(temporary) / "chaos-engine"
        if not (source / "skills/chaos-engine/SKILL.md").is_file():
            return None
        return [path.relative_to(source).as_posix() for path in owned_source_files(source)]


def owned_tree_differs_from_commit(project: Path, tree: Path, commit: str) -> list[str] | None:
    if _GIT_COMMIT.fullmatch(commit) is None:
        return None
    probe = subprocess.run(  # nosec B603 B607 - fixed git binary, validated commit
        ["git", "-C", str(project), "cat-file", "-e", f"{commit}^{{commit}}"],
        capture_output=True,
        check=False,
        shell=False,
    )
    if probe.returncode != 0:
        return None
    relatives = _owned_rels_at_commit(project, commit)
    if relatives is None:
        return None
    differing: list[str] = []
    for relative in relatives:
        current = tree / relative
        blob = _git_show_owned(project, commit, relative)
        if blob is None or not current.is_file() or current.read_bytes() != blob:
            differing.append(relative)
    return differing


def _apply_commit_byte_gate(result: dict[str, object], core: dict[str, object], project: Path) -> bool:
    """True when the commit object decides the doctor result and the caller must return."""
    commit = _manifest_commit(project)
    if not commit:
        return False
    overlay_diff = owned_tree_differs_from_commit(project, project / OVERLAY_DIR, commit)
    source_diff = owned_tree_differs_from_commit(project, project / SOURCE_DIR, commit)
    if overlay_diff is None or source_diff is None:
        return False
    if not overlay_diff:
        core["coreMatchesSource"] = True
        clear_overlay_handoff(project)
        return True
    if not source_diff:
        return False
    result["status"] = "recovery-required"
    core["status"] = "recovery-required"
    core["detail"] = "overlay-commit-mismatch"
    core["coreMatchesSource"] = False
    core["fixNext"] = (
        "Owned overlay bytes differ from the manifest commit and local "
        "SOURCE is not that commit. Do not rewrite manifest file digests."
    )
    return True


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

    if _apply_commit_byte_gate(result, core, Path(project)):
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


_POLICY_GROUPS = ("skills", "references", "hooks")
_POLICY_SUFFIXES = {".md", ".py", ".json", ".js"}
_NESTED_RELATIVES = ("identity.md", "skills/chaos-engine/SKILL.md")


def _policy_digest_map(root: Path) -> dict[str, str]:
    files: dict[str, str] = {}
    identity = root / "identity.md"
    if identity.is_file():
        files["identity.md"] = _sha256(identity)
    for group in _POLICY_GROUPS:
        base = root / group
        if not base.is_dir():
            continue
        for path in base.rglob("*"):
            if not path.is_file() or path.suffix.lower() not in _POLICY_SUFFIXES:
                continue
            if ".chaos-engine" in path.relative_to(root).parts:
                continue
            files[path.relative_to(root).as_posix()] = _sha256(path)
    return files


def policy_hash_mismatches(portable: Path, overlay: Path) -> list[str]:
    """Return policy paths whose sha256 differs between SOURCE and the overlay."""
    left = _policy_digest_map(portable)
    right = _policy_digest_map(overlay)
    return sorted(key for key in set(left) | set(right) if left.get(key) != right.get(key))


def nested_overlay_drift(overlay: Path) -> list[str]:
    """Fail when a nested overlay identity or router skill differs from the overlay."""
    nested = overlay / ".chaos-engine"
    if not nested.is_dir():
        return []
    drifted: list[str] = []
    for relative in _NESTED_RELATIVES:
        outer = overlay / relative
        inner = nested / relative
        if outer.is_file() and inner.is_file() and _sha256(outer) != _sha256(inner):
            drifted.append(relative)
    return drifted


def _retrieve_row(store: str, reason: str) -> dict[str, str]:
    return {
        "status": "recovery-required",
        "taskImpact": "required",
        "reason": reason,
        "code": "CE_RETRIEVE_DEGRADED",
        "fixNext": (
            f"python3 .chaos-engine/install.py repair --project . --component {store}. "
            "Do not auto-migrate ~/.mempalace."
        ),
    }


def apply_policy_hash_doctor(
    result: dict[str, object],
    project: Path,
    *,
    retrieve_reports: list[dict[str, object]] | None = None,
    probe_retrieve: bool = False,
) -> None:
    """Attach policy-overlay and degraded-retrieve rows. Does not refresh stores."""
    components = result.get("components")
    if not isinstance(components, dict):
        return
    root = Path(project)
    portable = root / SOURCE_DIR
    overlay = root / OVERLAY_DIR
    if portable.is_dir() and overlay.is_dir():
        mismatches = policy_hash_mismatches(portable, overlay)
        nested = nested_overlay_drift(overlay)
        if mismatches or nested:
            components["policy-overlay"] = {
                "status": "recovery-required",
                "taskImpact": "required",
                "reason": "policy hash drift",
                "mismatches": mismatches,
                "nestedDrift": nested,
                "fixNext": (
                    "python3 .chaos-engine/install.py repair --project . --component core. "
                    "Do not hand-edit a nested .chaos-engine/.chaos-engine tree."
                ),
            }
            result["status"] = "recovery-required"
    reports = list(retrieve_reports or [])
    if probe_retrieve and not reports and (root / OVERLAY_DIR / "tool.py").is_file():
        reports = _probe_degraded_retrieves(root)
    for report in reports:
        if not isinstance(report, dict) or report.get("status") != "degraded":
            continue
        store = str(report.get("store") or "")
        if store not in {"mempalace", "graphify"}:
            continue
        reason = str(report.get("reason") or "degraded")
        components[f"retrieve-{store}"] = _retrieve_row(store, reason)
        result["status"] = "recovery-required"


def _probe_degraded_retrieves(project: Path) -> list[dict[str, object]]:
    """One attempt per store. Never migrates a palace and never refreshes indexes."""
    import importlib.util

    path = Path(__file__).resolve().with_name("retrieve.py")
    if not path.is_file():
        return []
    spec = importlib.util.spec_from_file_location("ce_retrieve_doctor_probe", path)
    if spec is None or spec.loader is None:
        return []
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    reports: list[dict[str, object]] = []
    for store in ("mempalace", "graphify"):
        try:
            receipt = module.retrieve("doctor policy", store=store, project=project)
        except (OSError, RuntimeError, ValueError):
            reports.append({"store": store, "status": "degraded", "reason": "probe-failed"})
            continue
        if isinstance(receipt, dict):
            reports.append(receipt)
    return reports
