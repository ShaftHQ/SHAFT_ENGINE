#!/usr/bin/env python3
"""Keep ChaosEngine loaded in every checkout a host can start in (#6178).

The overlay (`.chaos-engine/`) and generated host files are untracked, so a
linked git worktree or a fresh clone starts without them. This module:

* `verify_ce_loads(cwd)` reports, per supported host, whether a session that
  starts in `cwd` (or any subdirectory) would load ChaosEngine.
* `materialize(primary, session)` copies the overlay and missing untracked
  host files from the primary checkout into a linked worktree. It never
  overwrites a file that already exists in the session.
* `linked_worktree_reason(path)` returns a one-line refusal for installers
  run inside a linked worktree, naming the primary checkout.

Standard library only; runs on Windows, macOS, and Linux.
"""

from __future__ import annotations

import argparse
import json
import shutil
import subprocess  # nosec B404 - fixed git arguments only.
import sys
from pathlib import Path

TREE = ".chaos-engine"
ROUTER = f"{TREE}/skills/chaos-engine/SKILL.md"
GUARD = f"{TREE}/hooks/guard.py"
HOOK_HOSTS = ("claude", "codex", "copilot", "gemini", "grok")
INSTRUCTION_ONLY_HOSTS = ("opencode", "cursor", "grok-bot")
HOSTS = (*HOOK_HOSTS, *INSTRUCTION_ONLY_HOSTS)
HOST_FILES = {
    "claude": (".claude/settings.json", ".claude/skills/chaos-engine/SKILL.md", "CLAUDE.md"),
    "codex": (".codex/config.toml", ".codex/hooks.json", ".agents/skills/chaos-engine/SKILL.md"),
    "copilot": (".github/hooks/chaos-engine.json", ".github/skills/chaos-engine/SKILL.md"),
    "gemini": (".gemini/settings.json", ".gemini/skills/chaos-engine/SKILL.md", "GEMINI.md"),
    "grok": (".grok/hooks",),
    "opencode": ("AGENTS.md",),
    "cursor": ("AGENTS.md",),
    "grok-bot": ("AGENTS.md",),
}
SHARED_FILES = ("AGENTS.md", ".mcp.json", ".claude/agents", ".codex/agents")
SKIP_PARTS = {"__pycache__", ".pytest_cache"}


def _git(cwd: Path, *arguments: str) -> str | None:
    git = shutil.which("git")
    if git is None:
        return None
    try:
        result = subprocess.run(  # nosec B603 - resolved git executable, fixed arguments.
            [git, *arguments], cwd=cwd, capture_output=True, text=True, timeout=15, check=False
        )
    except (OSError, subprocess.TimeoutExpired):
        return None
    return result.stdout.strip() if result.returncode == 0 else None


def checkout_root(cwd: Path) -> Path:
    """Top of the checkout that contains `cwd`; `cwd` itself outside git."""
    cwd = Path(cwd).resolve()
    top = _git(cwd, "rev-parse", "--show-toplevel")
    return Path(top).resolve() if top else cwd


def primary_checkout(path: Path) -> Path | None:
    """Primary checkout for a linked worktree, or None when `path` is primary or not git."""
    path = Path(path).resolve()
    output = _git(path, "rev-parse", "--path-format=absolute", "--git-dir", "--git-common-dir")
    if not output:
        return None
    lines = output.splitlines()
    if len(lines) != 2:
        return None
    git_dir, common = (Path(line).resolve() for line in lines)
    if git_dir == common:
        return None
    return common.parent if common.name == ".git" else common


def linked_worktree_reason(path: Path) -> str | None:
    """One-line refusal for installing into a linked worktree, or None."""
    primary = primary_checkout(path)
    if primary is None:
        return None
    return (
        f"linked git worktree: install ChaosEngine in the primary checkout {primary}, then run "
        f"`python3 {primary / TREE / 'worktree_overlay.py'} materialize --primary {primary} "
        f"--session {Path(path).resolve()}`"
    )


def _host_reason(root: Path, host: str, primary: Path | None) -> str:
    if not (root / ROUTER).is_file():
        return "missing-overlay"
    if host in HOOK_HOSTS and not (root / GUARD).is_file():
        return "missing-guard"
    if not (root / "AGENTS.md").is_file() and host in INSTRUCTION_ONLY_HOSTS:
        return "missing-agents-md"
    if primary is not None:
        for relative in HOST_FILES[host]:
            if (primary / relative).exists() and not (root / relative).exists():
                return f"missing-host-file:{relative}"
    return "ok"


def verify_ce_loads(cwd: Path) -> dict[str, str]:
    """Per host: "ok" or the first reason a session started at `cwd` would miss CE."""
    root = checkout_root(Path(cwd))
    primary = primary_checkout(root)
    return {host: _host_reason(root, host, primary) for host in HOSTS}


def _copy(source: Path, destination: Path) -> list[str]:
    copied: list[str] = []
    if source.is_dir():
        for item in sorted(source.rglob("*")):
            if item.is_dir() or SKIP_PARTS & set(item.parts) or item.suffix == ".pyc":
                continue
            target = destination / item.relative_to(source)
            if target.exists():
                continue
            target.parent.mkdir(parents=True, exist_ok=True)
            shutil.copy2(item, target)
            copied.append(target.as_posix())
    elif source.is_file() and not destination.exists():
        destination.parent.mkdir(parents=True, exist_ok=True)
        shutil.copy2(source, destination)
        copied.append(destination.as_posix())
    return copied


def materialize(primary: Path, session: Path) -> list[str]:
    """Copy the overlay and missing host files from `primary` into `session`."""
    primary, session = Path(primary).resolve(), Path(session).resolve()
    if not (primary / ROUTER).is_file():
        raise FileNotFoundError(f"primary checkout has no ChaosEngine overlay: {primary}")
    relatives = [TREE, *SHARED_FILES]
    for files in HOST_FILES.values():
        relatives.extend(files)
    copied: list[str] = []
    for relative in dict.fromkeys(relatives):
        copied.extend(_copy(primary / relative, session / relative))
    return copied


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    commands = parser.add_subparsers(dest="command", required=True)
    verify = commands.add_parser("verify", help="Report per-host CE load status for a directory.")
    verify.add_argument("--cwd", type=Path, default=Path.cwd())
    make = commands.add_parser("materialize", help="Copy the overlay into a linked worktree.")
    make.add_argument("--primary", type=Path, required=True)
    make.add_argument("--session", type=Path, required=True)
    args = parser.parse_args(argv)
    if args.command == "verify":
        report = verify_ce_loads(args.cwd)
        failing = {host: reason for host, reason in report.items() if reason != "ok"}
        print("ChaosEngine loads on every host" if not failing else json.dumps(failing, sort_keys=True))
        return 0 if not failing else 1
    copied = materialize(args.primary, args.session)
    print(f"materialized {len(copied)} file(s)")
    return 0


if __name__ == "__main__":
    sys.exit(main())
