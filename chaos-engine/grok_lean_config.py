"""Installer-owned lean Grok user config (#5802/#5803/#5805).

Merges ChaosEngine-owned spans into ``~/.grok/config.toml`` (or ``$GROK_HOME``)
without clobbering foreign keys. Uninstall removes only the CE marker span.
"""

from __future__ import annotations

import os
import re
import shutil
from pathlib import Path

LEAN_START = "# CHAOSENGINE-GROK-LEAN:START"
LEAN_END = "# CHAOSENGINE-GROK-LEAN:END"
SKILLS_START = "# CHAOSENGINE-GROK-SKILLS:START"
SKILLS_END = "# CHAOSENGINE-GROK-SKILLS:END"

COMPAT_FALSE_KEYS = (
    "hooks",
    "rules",
    "agents",
    "mcps",
    "skills",
    "sessions",
)

# First-pass lean skill disable set (#5805). Office/resume-* deferred.
LEAN_GROK_SKILL_DISABLE = (
    "game-animation-frames",
    "game-asset-core",
    "game-character-consistency",
    "game-tilesets",
    "game-ui-icons",
    "imagine",
)

_TABLE_HEADER = re.compile(r"^\[([^\]]+)\]\s*$")


def grok_home(home: Path | None = None) -> Path:
    """Return Grok home directory (``GROK_HOME`` or ``~/.grok``)."""
    env = os.environ.get("GROK_HOME")
    if env:
        return Path(env).expanduser()
    root = Path.home() if home is None else home
    return root / ".grok"


def grok_user_config_path(home: Path | None = None) -> Path:
    return grok_home(home) / "config.toml"


def grok_detected(*, which=shutil.which) -> bool:
    return which("grok") is not None


def project_has_grok_hooks(project: Path) -> bool:
    path = project / ".grok" / "hooks" / "lifecycle.json"
    return path.is_file()


def should_apply_grok_lean(project: Path, *, which=shutil.which) -> bool:
    """Apply lean compat when Grok CLI exists or project already has `.grok/hooks`."""
    return grok_detected(which=which) or project_has_grok_hooks(project)


def lean_compat_block() -> str:
    lines = [LEAN_START]
    for section in ("compat.claude", "compat.cursor"):
        lines.append(f"[{section}]")
        for key in COMPAT_FALSE_KEYS:
            lines.append(f"{key} = false")
        lines.append("")
    lines.append(LEAN_END)
    return "\n".join(lines).rstrip() + "\n"


def lean_skills_block(disabled: tuple[str, ...] = LEAN_GROK_SKILL_DISABLE) -> str:
    items = ", ".join(f'"{name}"' for name in disabled)
    return (
        f"{SKILLS_START}\n"
        f"[skills]\n"
        f"disabled = [{items}]\n"
        f"{SKILLS_END}\n"
    )


def _strip_marker_span(text: str, start: str, end: str) -> str:
    if start not in text or end not in text:
        return text
    out = text
    while start in out and end in out:
        begin = out.index(start)
        finish = out.index(end, begin) + len(end)
        # Drop following newline if present.
        if finish < len(out) and out[finish] == "\n":
            finish += 1
        out = out[:begin] + out[finish:]
    return out


def _strip_toml_tables(text: str, tables: set[str]) -> str:
    """Remove whole ``[name]`` tables (not nested ``[name.x]`` beyond exact name)."""
    lines = text.splitlines(keepends=True)
    kept: list[str] = []
    skipping = False
    for line in lines:
        bare = line.splitlines()[0] if line else ""
        stripped = bare.strip()
        match = _TABLE_HEADER.match(stripped)
        if match is not None:
            name = match.group(1).strip().strip('"')
            skipping = name in tables
            if skipping:
                continue
            kept.append(line)
            continue
        if skipping:
            continue
        kept.append(line)
    return "".join(kept)


def _ensure_trailing_newline(text: str) -> str:
    if not text:
        return ""
    return text if text.endswith("\n") else text + "\n"


def merge_lean_compat(text: str) -> str:
    """Idempotently install lean ``[compat.*]`` inside the CE marker span."""
    body = _strip_marker_span(text, LEAN_START, LEAN_END)
    # Drop outside duplicate lean tables so TOML parsers do not see duplicates.
    body = _strip_toml_tables(body, {"compat.claude", "compat.cursor"})
    body = _ensure_trailing_newline(body.rstrip("\n") + "\n" if body.strip() else "")
    if body and not body.endswith("\n\n"):
        body = body.rstrip("\n") + "\n\n"
    return body + lean_compat_block()


def remove_lean_compat(text: str) -> str:
    """Remove only the CE lean-compat marker span."""
    return _strip_marker_span(text, LEAN_START, LEAN_END)


def lean_compat_applied(text: str) -> bool:
    if LEAN_START not in text or LEAN_END not in text:
        return False
    interior = text.split(LEAN_START, 1)[1].split(LEAN_END, 1)[0]
    for section in ("compat.claude", "compat.cursor"):
        if f"[{section}]" not in interior:
            return False
    for key in COMPAT_FALSE_KEYS:
        if f"{key} = false" not in interior and f"{key}=false" not in interior:
            return False
    return True


def merge_lean_skills(
    text: str, disabled: tuple[str, ...] = LEAN_GROK_SKILL_DISABLE
) -> str:
    """Idempotently install ``[skills] disabled = [...]`` inside CE skills span."""
    body = _strip_marker_span(text, SKILLS_START, SKILLS_END)
    body = _ensure_trailing_newline(body.rstrip("\n") + "\n" if body.strip() else "")
    if body and not body.endswith("\n\n"):
        body = body.rstrip("\n") + "\n\n"
    return body + lean_skills_block(disabled)


def remove_lean_skills(text: str) -> str:
    return _strip_marker_span(text, SKILLS_START, SKILLS_END)


def lean_skills_applied(
    text: str, disabled: tuple[str, ...] = LEAN_GROK_SKILL_DISABLE
) -> bool:
    if SKILLS_START not in text or SKILLS_END not in text:
        return False
    interior = text.split(SKILLS_START, 1)[1].split(SKILLS_END, 1)[0]
    if "[skills]" not in interior:
        return False
    return all(name in interior for name in disabled)


def lean_skills_flag_enabled(
    *,
    cli_flag: bool | None = None,
    env: dict[str, str] | None = None,
) -> bool:
    """Return True when ``--lean-grok-skills`` or ``CHAOS_ENGINE_LEAN_GROK_SKILLS=1``."""
    if cli_flag is True:
        return True
    if cli_flag is False:
        return False
    source = os.environ if env is None else env
    return str(source.get("CHAOS_ENGINE_LEAN_GROK_SKILLS", "")).strip() in {
        "1",
        "true",
        "True",
        "yes",
        "YES",
    }


def apply_lean_compat_file(
    path: Path,
    *,
    dry_run: bool = False,
) -> dict[str, object]:
    """Write lean compat into ``path``; create parent dirs as needed."""
    result: dict[str, object] = {
        "path": str(path),
        "changed": False,
        "status": "healthy",
    }
    before = ""
    if path.is_file():
        before = path.read_text(encoding="utf-8")
    after = merge_lean_compat(before)
    if after == before:
        result["status"] = "healthy"
        result["detail"] = "lean-compat-already-applied"
        return result
    if not dry_run:
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(after, encoding="utf-8")
    result["changed"] = True
    result["detail"] = "lean-compat-applied"
    return result


def remove_lean_compat_file(path: Path, *, dry_run: bool = False) -> dict[str, object]:
    result: dict[str, object] = {
        "path": str(path),
        "changed": False,
        "status": "healthy",
    }
    if not path.is_file():
        result["detail"] = "missing"
        return result
    before = path.read_text(encoding="utf-8")
    after = remove_lean_compat(before)
    if after == before:
        result["detail"] = "lean-compat-absent"
        return result
    if not dry_run:
        if after.strip():
            path.write_text(after, encoding="utf-8")
        else:
            path.write_text("", encoding="utf-8")
    result["changed"] = True
    result["detail"] = "lean-compat-removed"
    return result


def apply_lean_skills_file(
    path: Path,
    *,
    dry_run: bool = False,
    disabled: tuple[str, ...] = LEAN_GROK_SKILL_DISABLE,
) -> dict[str, object]:
    result: dict[str, object] = {
        "path": str(path),
        "changed": False,
        "status": "healthy",
    }
    before = ""
    if path.is_file():
        before = path.read_text(encoding="utf-8")
    after = merge_lean_skills(before, disabled)
    if after == before:
        result["detail"] = "lean-skills-already-applied"
        return result
    if not dry_run:
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(after, encoding="utf-8")
    result["changed"] = True
    result["detail"] = "lean-skills-applied"
    return result


def remove_lean_skills_file(path: Path, *, dry_run: bool = False) -> dict[str, object]:
    result: dict[str, object] = {
        "path": str(path),
        "changed": False,
        "status": "healthy",
    }
    if not path.is_file():
        result["detail"] = "missing"
        return result
    before = path.read_text(encoding="utf-8")
    after = remove_lean_skills(before)
    if after == before:
        result["detail"] = "lean-skills-absent"
        return result
    if not dry_run:
        path.write_text(after, encoding="utf-8")
    result["changed"] = True
    result["detail"] = "lean-skills-removed"
    return result


def sync_project_grok_lean(
    project: Path,
    *,
    home: Path | None = None,
    which=shutil.which,
    lean_skills: bool = False,
    uninstall: bool = False,
) -> dict[str, object]:
    """Apply or remove lean Grok user config for one project activation."""
    report: dict[str, object] = {
        "status": "skipped",
        "reason": "grok-not-detected",
    }
    if uninstall:
        path = grok_user_config_path(home)
        compat = remove_lean_compat_file(path)
        skills = remove_lean_skills_file(path)
        report = {
            "status": "healthy",
            "compat": compat,
            "skills": skills,
            "path": str(path),
        }
        return report
    if not should_apply_grok_lean(project, which=which):
        return report
    path = grok_user_config_path(home)
    compat = apply_lean_compat_file(path)
    skills_result: dict[str, object] = {"status": "skipped", "detail": "flag-off"}
    if lean_skills:
        skills_result = apply_lean_skills_file(path)
    report = {
        "status": "healthy",
        "compat": compat,
        "skills": skills_result,
        "path": str(path),
    }
    return report


def doctor_lean_compat(
    project: Path,
    *,
    home: Path | None = None,
    which=shutil.which,
    heal: bool = True,
) -> dict[str, object]:
    """Heal lean compat when Grok is in play; else sync-advisory (never recovery-required)."""
    if not should_apply_grok_lean(project, which=which):
        return {"status": "skipped", "detail": "grok-not-in-play"}
    path = grok_user_config_path(home)
    text = path.read_text(encoding="utf-8") if path.is_file() else ""
    if lean_compat_applied(text):
        return {
            "status": "healthy",
            "detail": "lean-compat-ok",
            "path": str(path),
        }
    if heal:
        try:
            applied = apply_lean_compat_file(path)
            return {
                "status": "healthy",
                "detail": "lean-compat-healed",
                "path": str(path),
                "changed": applied.get("changed"),
            }
        except OSError as error:
            return {
                "status": "sync-advisory",
                "detail": f"lean-compat-heal-failed: {error}",
                "path": str(path),
                "fixNext": (
                    f"Ensure write access to {path}, then rerun doctor "
                    "or re-run install/activate so ChaosEngine can merge lean "
                    "[compat.claude]/[compat.cursor]."
                ),
            }
    return {
        "status": "sync-advisory",
        "detail": "lean-compat-missing",
        "path": str(path),
        "fixNext": (
            "Rerun install/activate or doctor to merge lean Grok compat "
            f"into {path}."
        ),
    }


def doctor_lean_skills_tip(
    project: Path,
    *,
    home: Path | None = None,
    which=shutil.which,
    env: dict[str, str] | None = None,
) -> dict[str, object]:
    """Sync-advisory tip when Grok is present and lean skills flag unused."""
    if not should_apply_grok_lean(project, which=which):
        return {"status": "skipped", "detail": "grok-not-in-play"}
    if lean_skills_flag_enabled(env=env):
        path = grok_user_config_path(home)
        text = path.read_text(encoding="utf-8") if path.is_file() else ""
        if lean_skills_applied(text):
            return {"status": "healthy", "detail": "lean-skills-ok", "path": str(path)}
        return {
            "status": "sync-advisory",
            "detail": "lean-skills-flag-set-but-not-applied",
            "fixNext": (
                "Rerun install with --lean-grok-skills or set "
                "CHAOS_ENGINE_LEAN_GROK_SKILLS=1 during activate."
            ),
        }
    return {
        "status": "sync-advisory",
        "detail": "lean-skills-recommended",
        "fixNext": (
            "Recommended for ChaosEngine: reinstall/activate with "
            "--lean-grok-skills (or CHAOS_ENGINE_LEAN_GROK_SKILLS=1) to disable "
            "game-* and imagine bundled skills. Undo by removing the "
            f"{SKILLS_START} span from ~/.grok/config.toml."
        ),
    }


def duplicate_chaos_engine_skills_from_inspect(
    payload: object,
) -> list[str]:
    """Best-effort: list duplicate chaos-engine skill paths from ``grok inspect`` JSON."""
    if not isinstance(payload, dict):
        return []
    skills = payload.get("skills")
    paths: list[str] = []
    if isinstance(skills, list):
        for item in skills:
            if not isinstance(item, dict):
                continue
            name = str(item.get("name") or item.get("id") or "").casefold()
            if name not in {"chaos-engine", "chaosengine"}:
                # Also accept path-based detection.
                target = str(item.get("path") or item.get("source") or item.get("target") or "")
                if "chaos-engine" not in target.replace("\\", "/").casefold():
                    continue
            target = str(
                item.get("path")
                or item.get("source")
                or item.get("target")
                or item.get("location")
                or name
            )
            paths.append(target)
    # Alternate shapes: skills.entries / bundledSkills
    if not paths:
        for key in ("bundledSkills", "projectSkills", "skillEntries"):
            entries = payload.get(key)
            if not isinstance(entries, list):
                continue
            for item in entries:
                if not isinstance(item, dict):
                    continue
                blob = str(item).casefold()
                if "chaos-engine" in blob:
                    paths.append(
                        str(
                            item.get("path")
                            or item.get("name")
                            or item.get("id")
                            or "chaos-engine"
                        )
                    )
    # Deduplicate while preserving order; report only when 2+ distinct.
    seen: list[str] = []
    for path in paths:
        normalized = path.replace("\\", "/")
        if normalized not in seen:
            seen.append(normalized)
    return seen if len(seen) >= 2 else []


def expected_chaos_engine_pointer_pair(paths: list[str]) -> bool:
    """True when grok lists only the repo `.agents` + `plugins/` pointer stubs."""
    if len(paths) != 2:
        return False
    norms = [path.replace("\\", "/").casefold() for path in paths]
    has_agents = any(".agents/skills/chaos-engine" in path for path in norms)
    has_plugin = any("plugins/chaos-engine/skills/chaos-engine" in path for path in norms)
    return has_agents and has_plugin


def doctor_grok_skill_dedupe(
    project: Path,
    *,
    executable: str | None = None,
    runner=None,
) -> dict[str, object]:
    """Sync-advisory when grok inspect shows duplicate chaos-engine skills."""
    import json
    import subprocess

    command = executable or shutil.which("grok")
    if not command:
        return {"status": "skipped", "detail": "grok-not-detected"}
    if not project_has_grok_hooks(project) and executable is None:
        # Still probe when CLI exists; cheap advisory.
        pass
    run = subprocess.run if runner is None else runner
    try:
        completed = run(
            [command, "inspect", "--json"],
            cwd=project.resolve(),
            capture_output=True,
            text=True,
            timeout=30,
            check=False,
        )
        payload = json.loads(completed.stdout or "{}") if completed.returncode == 0 else {}
    except (OSError, subprocess.SubprocessError, ValueError):
        return {"status": "skipped", "detail": "grok-inspect-unavailable"}
    dupes = duplicate_chaos_engine_skills_from_inspect(payload)
    if not dupes:
        return {"status": "healthy", "detail": "no-duplicate-chaos-engine-skills"}
    if expected_chaos_engine_pointer_pair(dupes):
        return {
            "status": "healthy",
            "detail": "expected-pointer-adapters",
            "paths": dupes,
        }
    return {
        "status": "sync-advisory",
        "detail": "duplicate-chaos-engine-skills",
        "paths": dupes,
        "fixNext": (
            "Grok listed multiple chaos-engine skills. Prefer AGENTS.md + "
            ".grok/hooks and pointer adapters (.agents/skills + plugins skill "
            "stubs). Ensure lean [compat.claude] skills=false so Claude-compat "
            "does not load a second full body."
        ),
    }
