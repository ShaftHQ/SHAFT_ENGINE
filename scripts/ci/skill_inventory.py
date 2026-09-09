"""Inventory SKILL.md trees; fail on orphans and divergent ChaosEngine bodies (#5713)."""

from __future__ import annotations

from pathlib import Path


def issue(code: str, path: str, message: str) -> dict[str, str]:
    return {"code": code, "path": path, "message": message}


HOST_SKILL_GLOBS = (
    ".agents/skills/*/SKILL.md",
    ".claude/skills/*/SKILL.md",
    ".gemini/skills/*/SKILL.md",
    ".github/skills/*/SKILL.md",
    ".codex/skills/*/SKILL.md",
)

PLUGIN_SKILL_GLOBS = (
    "plugins/*/skills/*/SKILL.md",
)

SOURCE_SKILL_GLOBS = (
    "chaos-engine/skills/*/SKILL.md",
    "chaos-engine/vendor/*/skills/*/SKILL.md",
    "chaos-engine/profiles/*/references/playbooks/*.md",
)

PRODUCT_SKILL_GLOB = "shaft-skills/**/SKILL.md"


def _glob_files(root: Path, pattern: str) -> list[Path]:
    return sorted(path for path in root.glob(pattern) if path.is_file())


def _skill_name(path: Path) -> str:
    if path.name == "SKILL.md":
        return path.parent.name
    return path.stem


def canonical_skill_names(root: Path) -> set[str]:
    names: set[str] = set()
    for pattern in SOURCE_SKILL_GLOBS:
        for path in _glob_files(root, pattern):
            names.add(_skill_name(path))
    return names


def _source_skill_bytes(root: Path, name: str) -> bytes | None:
    candidates = [
        root / "chaos-engine/skills" / name / "SKILL.md",
        *sorted((root / "chaos-engine/vendor").glob(f"*/skills/{name}/SKILL.md")),
    ]
    for path in candidates:
        if path.is_file():
            return path.read_bytes()
    return None


def validate_skill_inventory(root: Path) -> list[dict[str, str]]:
    errors: list[dict[str, str]] = []
    names = canonical_skill_names(root)
    overlay = root / ".chaos-engine"
    source = root / "chaos-engine"
    if overlay.is_dir() and source.is_dir():
        for skill in overlay.rglob("SKILL.md"):
            relative = skill.relative_to(overlay).as_posix()
            counterpart = source / relative
            if counterpart.is_file() and counterpart.read_bytes() != skill.read_bytes():
                errors.append(
                    issue(
                        "skill-body-divergent",
                        f".chaos-engine/{relative}",
                        "byte-divergent from chaos-engine/ source",
                    )
                )
    for pattern in HOST_SKILL_GLOBS:
        for path in _glob_files(root, pattern):
            name = _skill_name(path)
            if name not in names:
                errors.append(
                    issue(
                        "skill-orphan",
                        path.relative_to(root).as_posix(),
                        f"host skill {name!r} has no chaos-engine source",
                    )
                )
    for pattern in PLUGIN_SKILL_GLOBS:
        for path in _glob_files(root, pattern):
            name = _skill_name(path)
            if name not in names:
                errors.append(
                    issue(
                        "skill-orphan",
                        path.relative_to(root).as_posix(),
                        f"plugin skill {name!r} has no chaos-engine source",
                    )
                )
                continue
            expected = _source_skill_bytes(root, name)
            if expected is not None and path.read_bytes() != expected:
                errors.append(
                    issue(
                        "skill-body-divergent",
                        path.relative_to(root).as_posix(),
                        f"byte-divergent from chaos-engine skill {name!r}",
                    )
                )
    if (root / "chaos-engine/shaft-skills").exists() or (
        root / "chaos-engine/skills/shaft-developer"
    ).exists():
        errors.append(
            issue(
                "skill-product-pack",
                "shaft-skills",
                "shaft-skills must stay a separate product pack",
            )
        )
    for path in _glob_files(root, PRODUCT_SKILL_GLOB):
        if "chaos-engine" in path.parts:
            errors.append(
                issue(
                    "skill-product-pack",
                    path.relative_to(root).as_posix(),
                    "shaft-skills must stay a separate product pack",
                )
            )
    return errors
