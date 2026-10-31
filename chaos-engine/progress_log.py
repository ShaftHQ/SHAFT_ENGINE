"""Append-only task progress log."""
from __future__ import annotations

import os
from datetime import datetime, timezone
from pathlib import Path

ENV_LOG = "CHAOS_PROGRESS_LOG"


def resolve_log_path(
    task: str,
    *,
    environ: dict[str, str] | None = None,
    checkpoints_dir: Path | str | None = None,
) -> Path:
    """Return the progress log path for ``task``."""
    env = environ if environ is not None else os.environ
    override = str(env.get(ENV_LOG, "")).strip()
    if override:
        return Path(override)
    if checkpoints_dir is not None:
        base = Path(checkpoints_dir)
    else:
        configured = str(env.get("CHAOS_CHECKPOINTS_DIR", "")).strip()
        base = Path(configured) if configured else Path.cwd() / "checkpoints"
    safe = "".join(ch if ch.isalnum() or ch in "-_" else "-" for ch in task.strip()) or "task"
    return base / f"{safe}-progress.md"


def append_step(
    task: str,
    step: str,
    *,
    next_action: str = "",
    environ: dict[str, str] | None = None,
    checkpoints_dir: Path | str | None = None,
    clock: datetime | None = None,
) -> Path:
    """Append one step line; create the file with a header if missing."""
    path = resolve_log_path(task, environ=environ, checkpoints_dir=checkpoints_dir)
    path.parent.mkdir(parents=True, exist_ok=True)
    now = clock or datetime.now(timezone.utc)
    stamp = now.astimezone().strftime("%Y-%m-%d %H:%M %Z")
    if not path.is_file():
        path.write_text(f"# Progress: {task}\n\n", encoding="utf-8")
    line = f"- **{stamp}** — {step.strip()}"
    if next_action.strip():
        line += f" → next: {next_action.strip()}"
    with path.open("a", encoding="utf-8") as handle:
        handle.write(line + "\n")
    return path
