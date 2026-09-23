"""One live unattended pull-request watch: resume text and delivery claims."""

from __future__ import annotations


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
    watch_running = "watch" in folded and "running" in folded
    claims_merged = "merged" in folded or "delivered" in folded
    has_merged_at = "mergedat" in folded
    if follow_up_open and claims_merged:
        return True
    return watch_running and not has_merged_at
