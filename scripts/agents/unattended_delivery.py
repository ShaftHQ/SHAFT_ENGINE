"""One live unattended pull-request watch: resume text and delivery claims."""

from __future__ import annotations

import json
from pathlib import Path

_WATCH_KEYS = ("watchTaskId", "pullRequest", "repository", "headSha")


def _complete(raw: object) -> dict | None:
    if not isinstance(raw, dict):
        return None
    if any(raw.get(key) in (None, "") for key in _WATCH_KEYS):
        return None
    return raw


def checkpoint_from_event(event: dict) -> dict | None:
    """Live watch carried on the hook event or in the checkout state file."""
    found = _complete(event.get("unattendedWatch") or event.get("unattended_watch"))
    if found is not None:
        return found
    cwd = event.get("cwd")
    if not cwd:
        return None
    path = Path(str(cwd)) / ".chaos-engine-state" / "unattended-watch.json"
    try:
        payload = json.loads(path.read_text(encoding="utf-8"))
    except (OSError, UnicodeError, json.JSONDecodeError):
        return None
    return _complete(payload)


def resume_prompt(checkpoint: dict) -> str:
    """Compaction resume while a watch task is still pending."""
    task = checkpoint["watchTaskId"]
    pull = checkpoint["pullRequest"]
    repository = checkpoint["repository"]
    head = checkpoint["headSha"]
    return (
        f"Watch task {task} is still pending for pull request {pull} "
        f"in {repository} at {head}. The work is not done until merged, "
        "or until a red result is fixed and pushed. Wait on that same task id. "
        "A host line that you will be notified does not end the turn. "
        "Do not start another poller. "
        "Compaction restores the watch task id and the not-done-until-merged bit."
    )


def delivery_claim_rejected(text: str, *, follow_up_open: bool) -> bool:
    """Reject a final answer that is only a pending watch or an early merge."""
    folded = (text or "").casefold()
    watch_pending = (
        ("watch" in folded and "running" in folded)
        or "watch stays up" in folded
        or "watch is pending" in folded
    )
    claims_merged = "merged" in folded or "delivered" in folded
    has_merged_at = "mergedat" in folded
    if follow_up_open and claims_merged:
        return True
    return watch_pending and not has_merged_at
