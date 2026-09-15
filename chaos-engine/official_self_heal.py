#!/usr/bin/env python3
r"""
Universal self-heal via each dependency's official install command (#5811).

Deterministic first: run the documented official install / CE vendor rematerialize.
If impossible: write an agentic handoff with the exact official command + pasteable
agentPrompt — never a bare "rerun doctor".
"""

from __future__ import annotations

import importlib.util
import json
import os
import shutil
from pathlib import Path
from typing import Callable


OFFICIAL_SELF_HEAL_HANDOFF_RELATIVE = ".chaos-engine-state/official-self-heal-handoff.md"
COMPANION_HANDOFF_RELATIVE = ".chaos-engine-state/companion-handoff.md"

# Inventory: item → official install command (upstream docs or CE vendor publish).
OFFICIAL_INSTALL_COMMANDS: dict[str, str] = {
    "caveman": (
        "CE vendor rematerialize: hosts.rematerialize_companions "
        "(chaos-engine/vendor/caveman → plugins/caveman/)"
    ),
    "ponytail": (
        "CE vendor rematerialize: hosts.rematerialize_companions "
        "(chaos-engine/vendor/ponytail → plugins/ponytail/)"
    ),
    "mempalace": "uv tool install --with chromadb==1.5.9 mempalace==3.8.0",
    "graphify": "uv tool install --with tree-sitter-sql==0.3.11 graphifyy==0.9.43",
    "memory": "npm install -g @aictx/memory@0.2.1",
    "context7": "npm install -g ctx7@latest",
    "uv": (
        "curl -fsSL https://github.com/astral-sh/uv/releases/download/"
        "<pinned>/uv-installer.sh | env UV_INSTALL_DIR=\"$HOME/.local/bin\" "
        "UV_NO_MODIFY_PATH=1 sh"
    ),
    "node": (
        "Official Node LTS from https://nodejs.org/download/release/ "
        "(CE: dependencies.install_exact_node)"
    ),
    "java": (
        "Temurin 25 from Adoptium "
        "(CE: hosts.ensure_managed_temurin_jdk / dependencies.install_exact_java)"
    ),
    "maven": (
        "Apache Maven from https://maven.apache.org/download.cgi "
        "(CE: hosts.ensure_managed_maven)"
    ),
    "gh": "Official GitHub CLI from https://cli.github.com/",
    "mcps": (
        "python3 .chaos-engine/install.py repair --project . --component mcps "
        "(rebind CE-owned MCP catalog from hosts.install)"
    ),
    "skills": (
        "python3 .chaos-engine/install.py repair --project . --component skills "
        "(rebind skill adapters from hosts.install)"
    ),
}

BUNDLE_TOOL_ITEMS = ("memory", "mempalace", "graphify")
COMPANION_ITEMS = ("caveman", "ponytail")
REPAIR_COMPONENT_ITEMS = ("mcps", "skills")


def _doctor_cli() -> str:
    return "py -3" if os.name == "nt" else "python3"


def _doctor_command() -> str:
    return f"{_doctor_cli()} .chaos-engine/install.py doctor --project ."


def official_command_for(item: str) -> str:
    """Return the documented official install command for one inventory item."""
    command = OFFICIAL_INSTALL_COMMANDS.get(item)
    if not command:
        raise KeyError(f"no official install command registered for: {item}")
    return command


def inventory_table() -> list[dict[str, str]]:
    """Return the locked inventory for docs/tests (#5811)."""
    return [
        {"item": name, "officialCommand": command}
        for name, command in sorted(OFFICIAL_INSTALL_COMMANDS.items())
    ]


def _load_module(name: str, path: Path):
    spec = importlib.util.spec_from_file_location(name, path)
    if spec is None or spec.loader is None:
        raise ImportError(f"cannot load {path}")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def _hosts_module():
    return _load_module(
        "ce_official_self_heal_hosts",
        Path(__file__).resolve().with_name("hosts.py"),
    )


def _install_module():
    return _load_module(
        "ce_official_self_heal_install",
        Path(__file__).resolve().with_name("install.py"),
    )


def companion_skill_present(project: Path, name: str) -> bool:
    return (Path(project) / f"plugins/{name}/skills/{name}/SKILL.md").is_file()


def rematerialize_companions(
    project: Path,
    *,
    names: tuple[str, ...] | None = None,
) -> dict[str, object]:
    """Run CE vendor rematerialize for companions (official publish path)."""
    hosts = _hosts_module()
    return hosts.rematerialize_companions(project, names=names)



def _call_repair_component(
    repair_fn: Callable[..., dict[str, object]],
    project: Path,
    name: str,
    *,
    runner=None,
) -> dict[str, object] | object:
    """Invoke repair_component; never let TypeError escape as a doctor crash."""
    kwargs = {}
    if runner is not None:
        kwargs["runner"] = runner
    try:
        return repair_fn(Path(project).resolve(), name, **kwargs)
    except TypeError as type_error:
        # Signature mismatch only — subprocess TypeError must not be retried naked.
        if kwargs and "unexpected keyword" in str(type_error).lower():
            try:
                return repair_fn(Path(project).resolve(), name)
            except Exception as error:  # noqa: BLE001
                return {
                    "status": "failed",
                    "item": name,
                    "officialCommand": official_command_for(name),
                    "error": f"{type(error).__name__}: {error}",
                }
        return {
            "status": "failed",
            "item": name,
            "officialCommand": official_command_for(name),
            "error": f"{type(type_error).__name__}: {type_error}",
        }
    except Exception as error:  # noqa: BLE001 - surface as heal failure for handoff
        return {
            "status": "failed",
            "item": name,
            "officialCommand": official_command_for(name),
            "error": f"{type(error).__name__}: {error}",
        }


def heal_bundle_tool(
    project: Path,
    name: str,
    *,
    runner=None,
    repair: Callable[..., dict[str, object]] | None = None,
) -> dict[str, object]:
    """Heal one bundle tool via install.repair_component (account official path)."""
    if name not in BUNDLE_TOOL_ITEMS:
        raise ValueError(f"not a bundle tool item: {name}")
    install = _install_module()
    repair_fn = repair or install.repair_component
    result = _call_repair_component(repair_fn, project, name, runner=runner)
    if isinstance(result, dict) and result.get("status") == "failed" and "error" in result:
        return result
    status = "healed"
    if isinstance(result, dict) and result.get("status") not in {None, "repaired", "healed"}:
        status = "failed"
    return {
        "status": status,
        "item": name,
        "officialCommand": official_command_for(name),
        "repair": result if isinstance(result, dict) else {"raw": str(result)},
    }


def heal_repair_component(
    project: Path,
    name: str,
    *,
    runner=None,
    repair: Callable[..., dict[str, object]] | None = None,
) -> dict[str, object]:
    """Heal MCPs/skills via official repair component path."""
    if name not in REPAIR_COMPONENT_ITEMS:
        raise ValueError(f"not a repair-component heal item: {name}")
    install = _install_module()
    repair_fn = repair or install.repair_component
    result = _call_repair_component(repair_fn, project, name, runner=runner)
    if isinstance(result, dict) and result.get("status") == "failed" and "error" in result:
        return result
    status = "healed"
    if isinstance(result, dict) and result.get("status") not in {None, "repaired", "healed"}:
        status = "failed"
    return {
        "status": status,
        "item": name,
        "officialCommand": official_command_for(name),
        "repair": result if isinstance(result, dict) else {"raw": str(result)},
    }


def handoff_prompt(item: str, doctor_command: str | None = None) -> str:
    """Pasteable agent prompt with the exact official install command."""
    doctor = doctor_command or _doctor_command()
    command = official_command_for(item)
    handoff = (
        COMPANION_HANDOFF_RELATIVE
        if item in COMPANION_ITEMS
        else OFFICIAL_SELF_HEAL_HANDOFF_RELATIVE
    )
    return (
        f"Heal ChaosEngine required dependency `{item}` using `{handoff}`. "
        f"Run the official install command exactly: `{command}`. "
        f"Do not invent an alternate installer. Then run `{doctor}` and follow "
        "each remaining fix-next. Never answer with only \"rerun doctor\"."
    )


def write_companion_handoff(
    project: Path,
    *,
    names: list[str],
    detail: str | None = None,
    doctor_command: str | None = None,
) -> Path:
    """Persist agent heal steps when companion rematerialize failed."""
    doctor = doctor_command or _doctor_command()
    target = Path(project) / COMPANION_HANDOFF_RELATIVE
    target.parent.mkdir(parents=True, exist_ok=True)
    lines = [
        "# Companion handoff",
        "",
        "Deterministic CE vendor rematerialize could not restore enabled companions.",
        "",
    ]
    if detail:
        lines.extend([f"Detail: {detail}", ""])
    lines.extend(
        [
            "Missing or incomplete companions:",
            "",
        ]
    )
    for name in names:
        lines.append(f"- `{name}` — official: `{official_command_for(name)}`")
    lines.extend(
        [
            "",
            "Doctor:",
            "",
            f"`{doctor}`",
            "",
            "Agent prompt:",
            "",
            f"`{handoff_prompt(names[0] if names else 'caveman', doctor)}`",
            "",
        ]
    )
    target.write_text("\n".join(lines), encoding="utf-8")
    return target


def write_official_self_heal_handoff(
    project: Path,
    *,
    item: str,
    detail: str | None = None,
    doctor_command: str | None = None,
) -> Path:
    """Persist agent heal steps when official install heal failed."""
    doctor = doctor_command or _doctor_command()
    target = Path(project) / OFFICIAL_SELF_HEAL_HANDOFF_RELATIVE
    target.parent.mkdir(parents=True, exist_ok=True)
    command = official_command_for(item)
    lines = [
        "# Official self-heal handoff",
        "",
        f"Deterministic official install for `{item}` could not complete.",
        "",
        f"Official install command: `{command}`",
        "",
    ]
    if detail:
        lines.extend([f"Detail: {detail}", ""])
    lines.extend(
        [
            "Doctor:",
            "",
            f"`{doctor}`",
            "",
            "Agent prompt:",
            "",
            f"`{handoff_prompt(item, doctor)}`",
            "",
        ]
    )
    target.write_text("\n".join(lines), encoding="utf-8")
    return target


def clear_companion_handoff(project: Path) -> None:
    target = Path(project) / COMPANION_HANDOFF_RELATIVE
    if target.is_file() and not target.is_symlink():
        try:
            target.unlink()
        except OSError:
            # Best-effort cleanup; heal path still proceeds without the stale file.
            pass


def clear_official_self_heal_handoff(project: Path) -> None:
    target = Path(project) / OFFICIAL_SELF_HEAL_HANDOFF_RELATIVE
    if target.is_file() and not target.is_symlink():
        try:
            target.unlink()
        except OSError:
            # Best-effort cleanup; heal path still proceeds without the stale file.
            pass


def read_bundle_enabled(project: Path) -> dict[str, bool]:
    """Load bundle enablement without importing install.py (doctor-safe)."""
    defaults = {name: True for name in (*BUNDLE_TOOL_ITEMS, *COMPANION_ITEMS)}
    path = Path(project).resolve() / ".chaos-engine-state" / "bundle-options.json"
    if not path.is_file() or path.is_symlink():
        return defaults
    try:
        document = json.loads(path.read_text(encoding="utf-8"))
    except (OSError, UnicodeDecodeError, json.JSONDecodeError):
        return defaults
    enabled = document.get("enabled") if isinstance(document, dict) else None
    if not isinstance(enabled, dict):
        return defaults
    out = dict(defaults)
    for name in defaults:
        if name in enabled:
            out[name] = bool(enabled[name])
    return out


def _bundle_enabled(
    project: Path, name: str, *, bundle: dict[str, bool] | None = None
) -> bool:
    options = bundle if bundle is not None else read_bundle_enabled(project)
    return bool(options.get(name, True))


def _component_needs_heal(item: dict[str, object] | None) -> bool:
    if not isinstance(item, dict):
        return False
    status = str(item.get("status") or "")
    impact = str(item.get("taskImpact") or "required")
    if status == "healthy":
        return False
    if status == "absent" and impact == "optional":
        return False
    return status in {
        "absent",
        "recovery-required",
        "broken",
        "migration-required",
    }


def _mark_recovery_required(result: dict[str, object]) -> None:
    if result.get("status") == "healthy":
        result["status"] = "recovery-required"


def _collect_missing_companions(
    project: Path,
    *,
    components: dict[str, object],
    summary: dict[str, object],
    bundle_options: dict[str, bool],
) -> list[str]:
    """Classify companions; return names that still need rematerialize."""
    missing: list[str] = []
    for name in COMPANION_ITEMS:
        enabled = _bundle_enabled(project, name, bundle=bundle_options)
        key = f"companion-{name}"
        if not enabled:
            components[key] = {
                "status": "absent",
                "taskImpact": "optional",
                "detail": f"{name}-disabled-by-bundle",
            }
            summary["skipped"].append(name)  # type: ignore[index]
            continue
        if companion_skill_present(project, name):
            components[key] = {
                "status": "healthy",
                "taskImpact": "required",
                "officialCommand": official_command_for(name),
            }
            continue
        missing.append(name)
    return missing


def _apply_companion_heal_success(
    components: dict[str, object],
    summary: dict[str, object],
    project: Path,
    missing_companions: list[str],
) -> None:
    clear_companion_handoff(project)
    for name in missing_companions:
        components[f"companion-{name}"] = {
            "status": "healthy",
            "taskImpact": "required",
            "detail": "healed-via-official-vendor-rematerialize",
            "officialCommand": official_command_for(name),
        }
        summary["healed"].append(name)  # type: ignore[index]


def _apply_companion_heal_failure(
    result: dict[str, object],
    components: dict[str, object],
    summary: dict[str, object],
    project: Path,
    missing_companions: list[str],
    heal: dict[str, object],
) -> None:
    detail = str(heal.get("error") or heal.get("status") or "rematerialize-failed")
    handoff = write_companion_handoff(
        project, names=missing_companions, detail=detail
    )
    prompt = handoff_prompt(missing_companions[0])
    for name in missing_companions:
        components[f"companion-{name}"] = {
            "status": "absent",
            "taskImpact": "required",
            "detail": f"{name}-missing-for-implementation",
            "fixNext": (
                f"Complete the agent heal using {COMPANION_HANDOFF_RELATIVE}. "
                f"Official command: `{official_command_for(name)}`."
            ),
            "agentPrompt": prompt,
            "handoff": COMPANION_HANDOFF_RELATIVE,
            "officialCommand": official_command_for(name),
        }
        summary["failed"].append(name)  # type: ignore[index]
    _mark_recovery_required(result)
    summary["companionHandoff"] = str(handoff)


def _doctor_heal_companions(
    result: dict[str, object],
    project: Path,
    *,
    components: dict[str, object],
    summary: dict[str, object],
    rematerialize_fn: Callable[..., dict[str, object]],
    bundle_options: dict[str, bool],
) -> None:
    """Heal enabled-but-missing companions via CE vendor rematerialize."""
    missing_companions = _collect_missing_companions(
        project,
        components=components,
        summary=summary,
        bundle_options=bundle_options,
    )
    if not missing_companions:
        return

    try:
        heal = rematerialize_fn(project, names=tuple(missing_companions))
    except Exception as error:  # noqa: BLE001
        heal = {
            "status": "failed",
            "error": f"{type(error).__name__}: {error}",
            "names": missing_companions,
        }
    if heal.get("status") in {"healed", "healthy"}:
        _apply_companion_heal_success(
            components, summary, project, missing_companions
        )
        return
    _apply_companion_heal_failure(
        result, components, summary, project, missing_companions, heal
    )


def _doctor_heal_bundle_tools(
    result: dict[str, object],
    project: Path,
    *,
    components: dict[str, object],
    summary: dict[str, object],
    runner,
    repair: Callable[..., dict[str, object]] | None,
    bundle_options: dict[str, bool],
) -> None:
    """Heal enabled bundle tools via official install / repair_component."""
    for name in BUNDLE_TOOL_ITEMS:
        if not _bundle_enabled(project, name, bundle=bundle_options):
            summary["skipped"].append(name)  # type: ignore[index]
            continue
        item = components.get(name)
        if not _component_needs_heal(item if isinstance(item, dict) else None):
            continue
        heal = heal_bundle_tool(project, name, runner=runner, repair=repair)
        if heal.get("status") == "healed":
            clear_official_self_heal_handoff(project)
            if isinstance(item, dict):
                item["status"] = "healthy"
                item["detail"] = "healed-via-official-install"
                item["officialCommand"] = official_command_for(name)
                item.pop("fixNext", None)
            summary["healed"].append(name)  # type: ignore[index]
            continue

        detail = str(heal.get("error") or "official-install-failed")
        handoff = write_official_self_heal_handoff(
            project, item=name, detail=detail
        )
        prompt = handoff_prompt(name)
        if isinstance(item, dict):
            item["fixNext"] = (
                f"Complete the agent heal using {OFFICIAL_SELF_HEAL_HANDOFF_RELATIVE}. "
                f"Official command: `{official_command_for(name)}`."
            )
            item["agentPrompt"] = prompt
            item["handoff"] = OFFICIAL_SELF_HEAL_HANDOFF_RELATIVE
            item["officialCommand"] = official_command_for(name)
        summary["failed"].append(name)  # type: ignore[index]
        _mark_recovery_required(result)
        summary["officialSelfHealHandoff"] = str(handoff)


def _doctor_note_missing_gh(components: dict[str, object]) -> None:
    if shutil.which("gh") is not None:
        return
    components.setdefault(
        "gh",
        {
            "status": "absent",
            "taskImpact": "advisory",
            "detail": "gh-cli-missing",
            "officialCommand": official_command_for("gh"),
            "fixNext": (
                f"Install GitHub CLI via official installer: "
                f"`{official_command_for('gh')}`."
            ),
        },
    )


def apply_doctor_official_self_heal(
    result: dict[str, object],
    project: Path,
    *,
    runner=None,
    rematerialize: Callable[..., dict[str, object]] | None = None,
    repair: Callable[..., dict[str, object]] | None = None,
    bundle: dict[str, bool] | None = None,
) -> dict[str, object]:
    """
    Heal missing enabled third parties via official commands once during doctor.

    Returns a summary of heal attempts. Updates `result` components / status /
    handoffs in place.
    """
    project = Path(project).resolve()
    components = result.get("components")
    if not isinstance(components, dict):
        components = {}
        result["components"] = components

    summary: dict[str, object] = {"healed": [], "failed": [], "skipped": []}
    rematerialize_fn = rematerialize or rematerialize_companions
    bundle_options = bundle if bundle is not None else read_bundle_enabled(project)

    _doctor_heal_companions(
        result,
        project,
        components=components,
        summary=summary,
        rematerialize_fn=rematerialize_fn,
        bundle_options=bundle_options,
    )
    _doctor_heal_bundle_tools(
        result,
        project,
        components=components,
        summary=summary,
        runner=runner,
        repair=repair,
        bundle_options=bundle_options,
    )
    _doctor_note_missing_gh(components)

    result["officialSelfHeal"] = summary
    return summary


def apply_official_self_heal_fix_next(project: Path, components: object) -> None:
    """Point doctor fix-next at official-self-heal / companion handoffs."""
    if not isinstance(components, dict):
        return
    companion = Path(project) / COMPANION_HANDOFF_RELATIVE
    if companion.is_file() and not companion.is_symlink():
        message = (
            f"Complete the agent heal using {COMPANION_HANDOFF_RELATIVE}, "
            "then rerun doctor. Do not invent an alternate companion installer."
        )
        for key, item in components.items():
            if not isinstance(item, dict):
                continue
            if str(key).startswith("companion-") and item.get("status") != "healthy":
                item["fixNext"] = message
    handoff = Path(project) / OFFICIAL_SELF_HEAL_HANDOFF_RELATIVE
    if handoff.is_file() and not handoff.is_symlink():
        message = (
            f"Complete the agent heal using {OFFICIAL_SELF_HEAL_HANDOFF_RELATIVE}, "
            "run the official install command listed there, then rerun doctor."
        )
        for name in BUNDLE_TOOL_ITEMS:
            item = components.get(name)
            if isinstance(item, dict) and item.get("status") != "healthy":
                item["fixNext"] = message

