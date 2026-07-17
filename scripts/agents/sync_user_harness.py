"""Sync canonical user-harness files from the repo to ~/.claude; --check reports drift, --apply deploys."""
import os
import sys
from pathlib import Path

MANIFEST = ("CLAUDE.md", "settings.json", "statusline-command.sh")


def repo_root() -> Path:
    """Return the repo root, resolved from this script's own location (script sits in scripts/agents/)."""
    return Path(__file__).resolve().parent.parent.parent


def user_claude_dir() -> Path:
    """Return the target ~/.claude dir, honoring SHAFT_USER_CLAUDE_DIR for tests."""
    override = os.environ.get("SHAFT_USER_CLAUDE_DIR")
    return Path(override) if override else Path.home() / ".claude"


def normalize(data: bytes) -> bytes:
    """Normalize CRLF to LF so autocrlf checkouts don't register as drift."""
    return data.replace(b"\r\n", b"\n")


def main() -> int:
    apply_mode = "--apply" in sys.argv[1:]
    src_dir = repo_root() / ".claude" / "user-harness"
    dst_dir = user_claude_dir()

    sources = {name: src_dir / name for name in MANIFEST}
    missing_sources = [str(p) for p in sources.values() if not p.is_file()]
    if missing_sources:
        print("ERROR: manifest source file(s) missing: " + ", ".join(missing_sources))
        return 2

    all_in_sync = True
    for name in MANIFEST:
        src = sources[name]
        dst = dst_dir / name
        src_bytes = src.read_bytes()

        if not dst.is_file():
            print(f"MISSING  {name}  (target absent: {dst})")
            all_in_sync = False
            if apply_mode:
                dst.parent.mkdir(parents=True, exist_ok=True)
                dst.write_bytes(src_bytes)
                print(f"  -> deployed {dst}")
            continue

        dst_bytes = dst.read_bytes()
        in_sync = normalize(src_bytes) == normalize(dst_bytes)
        if in_sync:
            print(f"IN-SYNC  {name}")
            continue

        all_in_sync = False
        print(f"DRIFTED  {name}  (differs from {dst})")
        if apply_mode:
            backup = dst.with_name(dst.name + ".bak")
            backup.write_bytes(dst_bytes)
            print(f"  -> backed up to {backup}")
            dst.write_bytes(src_bytes)
            print(f"  -> deployed {dst}")

    if apply_mode:
        return 0
    return 0 if all_in_sync else 1


if __name__ == "__main__":
    sys.exit(main())
