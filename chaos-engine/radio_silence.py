"""Radio-silence SendToUser guard."""
from __future__ import annotations

import os
from pathlib import Path

ENV_FLAG = "CHAOS_RADIO_SILENCE"
STATE_FILE = Path(".chaos-engine-state") / "radio-silence"
TRUTHY = frozenset({"1", "true", "yes", "on", "full"})


def user_send_blocked(
    *,
    environ: dict[str, str] | None = None,
    project: Path | str | None = None,
) -> bool:
    """True when SendToUser / owner-chat status must not run."""
    env = environ if environ is not None else os.environ
    raw = str(env.get(ENV_FLAG, "")).strip().lower()
    if raw in TRUTHY:
        return True
    root = Path(project) if project is not None else Path.cwd()
    flag = root / STATE_FILE
    if flag.is_file():
        body = flag.read_text(encoding="utf-8").strip().lower()
        if body in TRUTHY or body == "":
            return True
    return False


def status_channel(*, blocked: bool | None = None) -> str:
    """Return the only allowed status plane label."""
    if blocked is None:
        blocked = user_send_blocked()
    return "orchestrator-only" if blocked else "owner-or-orchestrator"
