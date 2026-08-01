"""Check or deploy the source-controlled user harness without touching secrets."""

import json
import os
import sys
from pathlib import Path

MANIFEST = ("CLAUDE.md", "settings.json")
RETIRED_OWNED_SETTINGS_PATHS = (
    ("model",),
    ("effortLevel",),
    ("statusLine",),
    ("permissions", "defaultMode"),
    ("extraKnownMarketplaces", "mempalace"),
    ("env", "MEMPALACE_EMBEDDING_MODEL"),
)


def repo_root() -> Path:
    return Path(__file__).resolve().parent.parent.parent


def user_claude_dir() -> Path:
    override = os.environ.get("SHAFT_USER_CLAUDE_DIR")
    return Path(override) if override else Path.home() / ".claude"


def user_agents_dir(claude_dir: Path) -> Path:
    override = os.environ.get("SHAFT_USER_AGENTS_DIR")
    return Path(override) if override else claude_dir.parent / ".agents"


def normalize(data: bytes) -> bytes:
    return data.replace(b"\r\n", b"\n")


def merge_owned(existing: object, owned: object) -> object:
    """Overlay recursively-owned settings while preserving unowned keys."""
    if not isinstance(existing, dict) or not isinstance(owned, dict):
        return owned
    merged = dict(existing)
    for key, value in owned.items():
        merged[key] = merge_owned(existing.get(key), value)
    return merged


def remove_retired_owned_path(settings: dict, path: tuple[str, ...]) -> None:
    """Remove one formerly-owned key and empty retired-only parent mappings."""
    key = path[0]
    if len(path) == 1:
        settings.pop(key, None)
        return
    child = settings.get(key)
    if not isinstance(child, dict):
        return
    remove_retired_owned_path(child, path[1:])
    if not child:
        settings.pop(key, None)


def sources(root: Path, claude_dir: Path, agents_dir: Path) -> dict[str, tuple[Path, Path]]:
    harness = root / ".claude/user-harness"
    result = {name: (harness / name, claude_dir / name) for name in MANIFEST}
    for source in sorted((root / ".claude/agents").glob("*.md")):
        result[f"agents/{source.name}"] = (source, claude_dir / "agents" / source.name)

    adapter = root / ".claude/skills/act-as-mohab/SKILL.md"
    result["skills/act-as-mohab/SKILL.md"] = (
        adapter,
        claude_dir / "skills/act-as-mohab/SKILL.md",
    )
    canonical = root / ".agents/skills/act-as-mohab"
    for source in sorted(path for path in canonical.rglob("*") if path.is_file()):
        relative = source.relative_to(canonical)
        label = f"../.agents/skills/act-as-mohab/{relative.as_posix()}"
        result[label] = (source, agents_dir / "skills/act-as-mohab" / relative)
    return result


def main() -> int:
    apply_mode = "--apply" in sys.argv[1:]
    root = repo_root()
    claude_dir = user_claude_dir()
    manifest = sources(root, claude_dir, user_agents_dir(claude_dir))

    missing_sources = [str(source) for source, _ in manifest.values() if not source.is_file()]
    if missing_sources:
        print("ERROR: manifest source file(s) missing: " + ", ".join(missing_sources))
        return 2

    all_in_sync = True
    hard_failure = False
    for label, (source, target) in manifest.items():
        source_bytes = source.read_bytes()
        if not target.is_file():
            print(f"MISSING  {label}  (target absent: {target})")
            all_in_sync = False
            if apply_mode:
                target.parent.mkdir(parents=True, exist_ok=True)
                target.write_bytes(source_bytes)
                print(f"  -> deployed {target}")
            continue

        target_bytes = target.read_bytes()
        desired_bytes = source_bytes
        if label == "settings.json":
            try:
                owned = json.loads(source_bytes)
                existing = json.loads(target_bytes)
            except (UnicodeDecodeError, json.JSONDecodeError):
                print(f"INVALID  {label}  (target is not valid JSON: {target})")
                all_in_sync = False
                hard_failure = True
                continue
            desired = merge_owned(existing, owned)
            for retired_path in RETIRED_OWNED_SETTINGS_PATHS:
                remove_retired_owned_path(desired, retired_path)
            desired_bytes = (json.dumps(desired, indent=2) + "\n").encode("utf-8")

        if normalize(desired_bytes) == normalize(target_bytes):
            print(f"IN-SYNC  {label}")
            continue

        all_in_sync = False
        print(f"DRIFTED  {label}  (differs from {target})")
        if apply_mode:
            backup = target.with_name(target.name + ".bak")
            backup.write_bytes(target_bytes)
            print(f"  -> backed up to {backup}")
            target.write_bytes(desired_bytes)
            print(f"  -> deployed {target}")

    if hard_failure:
        return 2
    return 0 if apply_mode or all_in_sync else 1


if __name__ == "__main__":
    raise SystemExit(main())
