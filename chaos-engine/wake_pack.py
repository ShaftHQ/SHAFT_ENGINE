#!/usr/bin/env python3
"""Owner-curated L0/L1 wake pack — MemPalace may draft only (#5624)."""

from __future__ import annotations

import argparse
import json
from pathlib import Path

WAKE_RELATIVE = Path(".chaos-engine-state") / "wake-pack.md"
# ~120 tokens ≈ 480 chars soft budget for SessionStart-safe packs.
MAX_CHARS = 480
DRAFT_RELATIVE = Path(".chaos-engine-state") / "wake-pack.mempalace-draft.md"


def project_root(start: Path | None = None) -> Path:
    here = (start or Path.cwd()).resolve()
    for candidate in (here, *here.parents):
        if (candidate / ".chaos-engine" / "install.py").is_file() or (
            candidate / "chaos-engine" / "install.py"
        ).is_file():
            return candidate
    return here


def wake_path(project: Path | None = None) -> Path:
    return project_root(project) / WAKE_RELATIVE


def draft_path(project: Path | None = None) -> Path:
    return project_root(project) / DRAFT_RELATIVE


def read_wake_pack(project: Path | None = None) -> str | None:
    path = wake_path(project)
    if not path.is_file():
        return None
    try:
        text = path.read_text(encoding="utf-8").strip()
    except OSError:
        return None
    return text or None


def write_wake_pack(
    text: str,
    *,
    project: Path | None = None,
    force: bool = False,
    source: str = "owner",
) -> dict[str, object]:
    """Write owner wake pack. MemPalace drafts never silently overwrite."""
    path = wake_path(project)
    path.parent.mkdir(parents=True, exist_ok=True)
    cleaned = " ".join(str(text).split())
    if len(cleaned) > MAX_CHARS:
        raise ValueError(f"wake pack exceeds {MAX_CHARS} characters (~120 tokens)")
    if source != "owner" and path.is_file() and not force:
        draft = draft_path(project)
        draft.write_text(cleaned + "\n", encoding="utf-8")
        return {
            "status": "drafted",
            "path": str(draft.relative_to(project_root(project))),
            "chars": len(cleaned),
            "note": "MemPalace draft only; owner must promote (never silent overwrite).",
        }
    path.write_text(cleaned + "\n", encoding="utf-8")
    return {
        "status": "written",
        "path": str(path.relative_to(project_root(project))),
        "chars": len(cleaned),
        "source": source,
    }


def session_start_locator(project: Path | None = None) -> str:
    """Locator only — never inject wake pack prose into SessionStart."""
    if read_wake_pack(project) is None:
        return "Wake pack: absent (owner may curate .chaos-engine-state/wake-pack.md)."
    return "Wake pack: .chaos-engine-state/wake-pack.md (owner-curated; no prose dump)."


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    sub = parser.add_subparsers(dest="command", required=True)
    show = sub.add_parser("show")
    show.add_argument("--project", type=Path, default=None)
    write = sub.add_parser("write")
    write.add_argument("--text", required=True)
    write.add_argument("--project", type=Path, default=None)
    write.add_argument("--force", action="store_true")
    write.add_argument("--source", choices=("owner", "mempalace"), default="owner")
    loc = sub.add_parser("locator")
    loc.add_argument("--project", type=Path, default=None)
    args = parser.parse_args(argv)
    if args.command == "show":
        text = read_wake_pack(args.project)
        print(text or "")
        return 0 if text else 1
    if args.command == "write":
        result = write_wake_pack(
            args.text, project=args.project, force=args.force, source=args.source
        )
        print(json.dumps(result, sort_keys=True))
        return 0
    print(session_start_locator(args.project))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
