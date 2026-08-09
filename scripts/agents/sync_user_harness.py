"""Check or deploy the source-controlled user harness without touching secrets."""

import hashlib
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
CODEX_AGENT_PREFIX = "../.codex/agents/"
CODEX_AGENTS_LABEL = "../.codex/AGENTS.md"
MANAGED_CODEX_AGENT_MARKER = b"Managed by the SHAFT user harness"
LEGACY_CODEX_TARGET_HASHES = {
    "../.codex/AGENTS.md": {
        "58488d3c6f860afec44c078d060c280ef887b69d37bd82e6b2be713371268865",
        "eae7709a4c36efd170327c4bfbdbbed626f14915c8c46b8d3f3eb9947b9fbb25",
    },
    "../.codex/agents/chaos-engine.toml": {
        "e50c90620d99eaa2701cacedc2a5f2b1f0f01e0607c04548aa7a80d8e5126915"
    },
    "../.codex/agents/coder.toml": {
        "3c0dc2baa41a29c5049d68a815e9a41333432725850b92a7c964a3a1c28814f6"
    },
    "../.codex/agents/reviewer.toml": {
        "95b6f4d87be43a7b33bf173fddefd8589fb0950853e98bf9fca8e9645c386e99"
    },
    "../.codex/agents/tester.toml": {
        "f08a90fcb3e8a595f3572559bbadcff2ced310adbd534f7de491555abe3d35ea"
    },
}


def repo_root() -> Path:
    return Path(__file__).resolve().parent.parent.parent


def user_claude_dir() -> Path:
    override = os.environ.get("SHAFT_USER_CLAUDE_DIR")
    return Path(override) if override else Path.home() / ".claude"


def user_agents_dir(claude_dir: Path) -> Path:
    override = os.environ.get("SHAFT_USER_AGENTS_DIR")
    return Path(override) if override else claude_dir.parent / ".agents"


def user_codex_dir(claude_dir: Path) -> Path:
    override = os.environ.get("SHAFT_USER_CODEX_DIR")
    return Path(override) if override else claude_dir.parent / ".codex"


def normalize(data: bytes) -> bytes:
    return data.replace(b"\r\n", b"\n")


def is_owned_codex_target(label: str, target: bytes) -> bool:
    """Recognize current and legacy SHAFT-owned Codex guidance targets."""
    if not (label.startswith(CODEX_AGENT_PREFIX) or label == CODEX_AGENTS_LABEL):
        return True
    if MANAGED_CODEX_AGENT_MARKER in target:
        return True
    digest = hashlib.sha256(normalize(target)).hexdigest()
    return digest in LEGACY_CODEX_TARGET_HASHES.get(label, set())


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


def sources(
    root: Path, claude_dir: Path, agents_dir: Path, codex_dir: Path
) -> dict[str, tuple[Path, Path]]:
    harness = root / ".claude/user-harness"
    result = {name: (harness / name, claude_dir / name) for name in MANIFEST}
    for source in sorted((root / ".claude/agents").glob("*.md")):
        result[f"agents/{source.name}"] = (source, claude_dir / "agents" / source.name)
    result["../.codex/AGENTS.md"] = (harness / "CLAUDE.md", codex_dir / "AGENTS.md")
    for source in sorted((root / ".codex/agents").glob("*.toml")):
        result[f"../.codex/agents/{source.name}"] = (
            source,
            codex_dir / "agents" / source.name,
        )

    # Deploy every canonical skill and its host adapter, not just the router:
    # the router links sibling skills, so a partial deploy leaves dead links.
    for adapter in sorted((root / ".claude/skills").glob("*/SKILL.md")):
        relative = adapter.relative_to(root / ".claude/skills")
        result[f"skills/{relative.as_posix()}"] = (adapter, claude_dir / "skills" / relative)
    canonical_root = root / ".agents/skills"
    for source in sorted(path for path in canonical_root.rglob("*") if path.is_file()):
        relative = source.relative_to(canonical_root)
        label = f"../.agents/skills/{relative.as_posix()}"
        result[label] = (source, agents_dir / "skills" / relative)
    return result


def main() -> int:
    apply_mode = "--apply" in sys.argv[1:]
    json_mode = "--json" in sys.argv[1:]
    root = repo_root()
    claude_dir = user_claude_dir()
    manifest = sources(
        root,
        claude_dir,
        user_agents_dir(claude_dir),
        user_codex_dir(claude_dir),
    )

    entries: list[dict[str, object]] = []

    def record(
        state: str, label: str, source: Path, target: Path, **details: str
    ) -> None:
        entries.append(
            {"state": state, "label": label, "source": str(source), "target": str(target), **details}
        )

    def finish(code: int) -> int:
        if json_mode:
            print(json.dumps({"entries": entries, "exit_code": code}))
        return code

    missing_sources = [
        (label, source, target)
        for label, (source, target) in manifest.items()
        if not source.is_file()
    ]
    if missing_sources:
        for label, source, target in missing_sources:
            record("ERROR", label, source, target)
        if not json_mode:
            print("ERROR: manifest source file(s) missing: " + ", ".join(str(source) for _, source, _ in missing_sources))
        return finish(2)

    all_in_sync = True
    hard_failure = False
    for label, (source, target) in manifest.items():
        source_bytes = source.read_bytes()
        if not target.is_file():
            details = {}
            if not json_mode:
                print(f"MISSING  {label}  (target absent: {target})")
            all_in_sync = False
            if apply_mode:
                target.parent.mkdir(parents=True, exist_ok=True)
                target.write_bytes(source_bytes)
                if not json_mode:
                    print(f"  -> deployed {target}")
                details["deployed"] = str(target)
            record("MISSING", label, source, target, **details)
            continue

        target_bytes = target.read_bytes()
        desired_bytes = source_bytes
        if label == "settings.json":
            try:
                owned = json.loads(source_bytes)
                existing = json.loads(target_bytes)
            except (UnicodeDecodeError, json.JSONDecodeError):
                if not json_mode:
                    print(f"INVALID  {label}  (target is not valid JSON: {target})")
                record("INVALID", label, source, target)
                all_in_sync = False
                hard_failure = True
                continue
            desired = merge_owned(existing, owned)
            for retired_path in RETIRED_OWNED_SETTINGS_PATHS:
                remove_retired_owned_path(desired, retired_path)
            desired_bytes = (json.dumps(desired, indent=2) + "\n").encode("utf-8")

        if normalize(desired_bytes) == normalize(target_bytes):
            if not json_mode:
                print(f"IN-SYNC  {label}")
            record("IN-SYNC", label, source, target)
            continue

        all_in_sync = False
        if not is_owned_codex_target(label, target_bytes):
            if not json_mode:
                print(f"CONFLICT  {label}  (unowned target exists: {target})")
            record("CONFLICT", label, source, target)
            hard_failure = True
            continue
        if not json_mode:
            print(f"DRIFTED  {label}  (differs from {target})")
        details = {}
        if apply_mode:
            backup = target.with_name(target.name + ".bak")
            if backup.exists():
                if not json_mode:
                    print(f"  -> preserved existing backup {backup}")
            else:
                backup.write_bytes(target_bytes)
                if not json_mode:
                    print(f"  -> backed up to {backup}")
            details["backup"] = str(backup)
            target.write_bytes(desired_bytes)
            if not json_mode:
                print(f"  -> deployed {target}")
            details["deployed"] = str(target)
        record("DRIFTED", label, source, target, **details)

    if hard_failure:
        return finish(2)
    return finish(0 if apply_mode or all_in_sync else 1)


if __name__ == "__main__":
    raise SystemExit(main())
