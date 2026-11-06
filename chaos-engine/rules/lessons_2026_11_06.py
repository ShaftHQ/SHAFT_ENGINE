"""
Fail-closed gates for the 2026-11-06 ChaosEngine lessons cycle (#7766, #7767, #7726).

Extends the 2026-11-02 evaluators. Each evaluate_* returns
{"ok": bool, "hits": list, "error": str|None}.
"""

from __future__ import annotations

import importlib.util
import re
from pathlib import Path
from typing import Any


def _result(hits: list[str], error: str) -> dict[str, Any]:
    return {"ok": not hits, "hits": hits, "error": None if not hits else error}


def _load_sibling(name: str):
    path = Path(__file__).resolve().parent / f"{name}.py"
    spec = importlib.util.spec_from_file_location(f"ce_{name}_1106", path)
    if spec is None or spec.loader is None:
        raise ImportError(f"cannot load sibling module {name!r} from {path}")
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


_SECRET = re.compile(
    r"\b(?:ghp_[A-Za-z0-9]{8,}|github_pat_[A-Za-z0-9_]{8,}|AKIA[0-9A-Z]{16}|xox[baprs]-[A-Za-z0-9-]{8,})\b"
)
_PASSWORD = re.compile(r"\bpassword\s*[:=]\s*\S+", re.I)
_RETRIEVE = re.compile(
    r"retrieve:\s*(?:used|skipped\([^)\n]+\)|exempt\([^)\n]+\))",
    re.I,
)
_FOUNDER = re.compile(r"\b(?:co-)?founder\b|\bceo\b", re.I)


# ----- #7767 delegation cycle -----
def evaluate_delegation_cycle(record: dict[str, Any]) -> dict[str, Any]:
    """Shared instruction file, radio silence, one writer, lessons mega-PR cycle."""
    if not isinstance(record, dict):
        return _result(["record"], "#7767 delegation-cycle gate failed")
    hits: list[str] = []
    if record.get("multi_agent_order"):
        if not record.get("shared_instruction_file"):
            hits.append("missing-shared-instruction-file")
        message = str(record.get("peer_message") or "")
        if record.get("peer_message") is not None and record.get("shared_instruction_file"):
            path = str(record.get("shared_instruction_file"))
            if path and path not in message:
                hits.append("peer-message-missing-file-pointer")
            if len(message) > 400 and record.get("essay_in_chat"):
                hits.append("details-in-chat")
    if record.get("owner_status_requested") and not record.get("fresh_poll_complete"):
        hits.append("missing-fresh-poll")
    if record.get("radio_silence") and record.get("messaged_human") and not record.get("human_asked"):
        hits.append("broke-radio-silence")
    if record.get("lessons_cycle"):
        if not record.get("one_ce_pr"):
            hits.append("lessons-not-one-ce-pr")
        if record.get("pr_merged") and not record.get("installed_before_cleanup"):
            hits.append("cleanup-before-install")
        if record.get("cleanup_started") and not record.get("router_cleanup_signal"):
            hits.append("cleanup-without-router-signal")
        if record.get("sent_done") and not record.get("every_cleanup_reported"):
            hits.append("done-before-cleanup-reports")
    writers = record.get("writers_per_file") or {}
    if isinstance(writers, dict):
        for path, names in writers.items():
            if isinstance(names, (list, tuple, set)) and len(set(names)) > 1:
                hits.append(f"two-writers:{path}")
    if record.get("same_file_parallel_edit"):
        hits.append("two-writers")
    return _result(hits, "#7767 delegation-cycle gate failed")


# ----- #7726 docs hygiene -----
_DOC_STATUS = frozenset({"done", "in progress", "not started"})


def evaluate_docs_hygiene(record: dict[str, Any]) -> dict[str, Any]:
    if not isinstance(record, dict):
        return _result(["record"], "#7726 docs-hygiene gate failed")
    hits: list[str] = []
    sources = record.get("source_of_truth_files")
    if record.get("require_sources"):
        if not isinstance(sources, (list, tuple)) or len(sources) != 3:
            hits.append("sources-not-three")
    for row in record.get("status_rows") or []:
        if not isinstance(row, dict):
            continue
        label = str(row.get("status") or "").strip().casefold()
        if label and label not in _DOC_STATUS:
            hits.append(f"bad-status-vocabulary:{row.get('status')}")
    if record.get("hours_tracked"):
        if record.get("done_hours") != record.get("planned_hours"):
            hits.append("done-hours-ne-planned")
        if not record.get("live_formulas"):
            hits.append("hours-not-live-formulas")
    if record.get("journey_maps"):
        if not record.get("journey_maps_agree"):
            hits.append("journey-maps-disagree")
    if record.get("partial_journey") and record.get("merged_as_full"):
        hits.append("partial-journey-as-full")
    if record.get("delete_guard_change") and not record.get("upstream_branch_landed"):
        hits.append("delete-guard-without-upstream")
    if record.get("review_claimed") and not (
        record.get("review_sha") and record.get("head_sha") and record.get("review_sha") == record.get("head_sha")
    ):
        hits.append("review-not-sha-bound")
    return _result(hits, "#7726 docs-hygiene gate failed")


# ----- external contribution tracker (rule only; no upstream edit) -----
def evaluate_external_contribution(record: dict[str, Any]) -> dict[str, Any]:
    if not isinstance(record, dict):
        return _result(["record"], "external-contribution gate failed")
    hits: list[str] = []
    if record.get("closing"):
        if not record.get("upstream_merged"):
            hits.append("close-before-upstream-merge")
        if not record.get("upstream_released"):
            hits.append("close-before-upstream-release")
        if not record.get("product_adopted"):
            hits.append("close-before-product-adoption")
    if record.get("stale") or record.get("changes_requested"):
        if not record.get("stale_check_recorded"):
            hits.append("missing-stale-or-changes-requested-check")
    if record.get("fork_pr") and not record.get("fork_label_asked"):
        hits.append("missing-fork-label-ask")
    text = str(record.get("upstream_text") or "")
    if text and not text.isascii():
        hits.append("upstream-text-not-english")
    return _result(hits, "external-contribution gate failed")


# ----- job-apply record (abstract; no profile data) -----
def evaluate_job_apply(record: dict[str, Any]) -> dict[str, Any]:
    if not isinstance(record, dict):
        return _result(["record"], "job-apply gate failed")
    hits: list[str] = []
    if record.get("confirm_before_send") and record.get("standing_order_recorded"):
        if record.get("within_named_scope") and record.get("blocked_on_confirm"):
            hits.append("standing-order-outranked")
    contacts = record.get("contacts") or []
    if isinstance(contacts, list) and len(contacts) > 1 and record.get("single_application"):
        hits.append("distinct-contacts-collapsed")
    if record.get("remote_role"):
        subject = str(record.get("subject") or "")
        body = str(record.get("body") or "")
        if "remote" not in subject.casefold() or "remote" not in body.casefold():
            hits.append("remote-ask-missing")
    if record.get("sending"):
        reviewers = record.get("reviewers") or []
        if not isinstance(reviewers, list) or len(set(reviewers)) < 2:
            hits.append("missing-two-reviewers")
        if not record.get("sent_readback"):
            hits.append("missing-sent-readback")
        if record.get("signature_count") not in (None, 1) and int(record.get("signature_count") or 0) != 1:
            hits.append("signature-not-single")
    if record.get("public_by_link"):
        if not record.get("share_dialog"):
            hits.append("public-by-link-without-share-dialog")
        if not record.get("confirmed_this_turn"):
            hits.append("share-not-confirmed-this-turn")
    if record.get("ats_identical_errors", 0) >= 2 and not record.get("switched_channel"):
        hits.append("ats-errors-without-next-channel")
    if record.get("tracker_write") and not record.get("tracker_readback"):
        hits.append("tracker-write-without-readback")
    if record.get("treated_ledger_as_source"):
        hits.append("ledger-is-not-source")
    return _result(hits, "job-apply gate failed")


def evaluate_email_resend(record: dict[str, Any]) -> dict[str, Any]:
    if not isinstance(record, dict):
        return _result(["record"], "email-resend gate failed")
    if record.get("resend") and not record.get("sent_folder_searched"):
        return _result(["missing-sent-search"], "email-resend gate failed")
    return _result([], "email-resend gate failed")


def evaluate_cover_letter(
    text: str,
    *,
    sourced_tools: list[str] | None = None,
    claimed_tools: list[str] | None = None,
    client_names: list[str] | None = None,
) -> dict[str, Any]:
    """Founder/CEO titles, caller-supplied client names, unsourced tool claims."""
    hits: list[str] = []
    body = text or ""
    if _FOUNDER.search(body):
        hits.append("founder-or-ceo-title")
    folded = body.casefold()
    for name in client_names or []:
        token = str(name or "").strip()
        if token and token.casefold() in folded:
            hits.append("client-name")
    sourced = {str(tool).casefold() for tool in (sourced_tools or [])}
    for tool in claimed_tools or []:
        if str(tool).casefold() not in sourced:
            hits.append(f"unsourced-tool:{tool}")
    return _result(hits, "cover-letter gate failed")


# ----- secrets, force-push, relayed delete, private remotes -----
def evaluate_secrets_text(text: str, *, kind: str = "lessons") -> dict[str, Any]:
    hits: list[str] = []
    if _SECRET.search(text or ""):
        hits.append(f"secret-in-{kind}")
    if _PASSWORD.search(text or ""):
        hits.append(f"password-in-{kind}")
    return _result(hits, "secrets gate failed")


def evaluate_force_push(command: str, *, approved: bool = False) -> dict[str, Any]:
    text = command or ""
    if "--force" in text or "--force-with-lease" in text:
        if not approved:
            return _result(["force-push-without-approval"], "force-push gate failed")
    return _result([], "force-push gate failed")


def evaluate_private_remote(record: dict[str, Any]) -> dict[str, Any]:
    if not isinstance(record, dict):
        return _result(["record"], "private-remote gate failed")
    if record.get("private_working_copy") and record.get("adding_remote"):
        return _result(["remote-on-private-copy"], "private-remote gate failed")
    return _result([], "private-remote gate failed")


def evaluate_relayed_delete(record: dict[str, Any]) -> dict[str, Any]:
    if not isinstance(record, dict):
        return _result(["record"], "relayed-delete gate failed")
    if record.get("delete_requested") and record.get("code_relayed_by_agent") and not record.get("human_typed_code"):
        return _result(["relayed-delete-code"], "relayed-delete gate failed")
    return _result([], "relayed-delete gate failed")


def evaluate_public_issue_text(text: str, *, reviewed: bool = False) -> dict[str, Any]:
    hits: list[str] = []
    if not reviewed:
        hits.append("missing-leak-review")
    hits.extend(evaluate_secrets_text(text, kind="issue")["hits"])
    return _result(hits, "public-issue leak-review gate failed")


# ----- long tasks, PR body, bot tests, plugin e2e, seeded gate -----
def evaluate_long_task_watchdog(record: dict[str, Any]) -> dict[str, Any]:
    if not isinstance(record, dict):
        return _result(["record"], "watchdog gate failed")
    minutes = record.get("expected_minutes")
    try:
        long = minutes is not None and float(minutes) > 30
    except (TypeError, ValueError):
        long = False
    if not long:
        return _result([], "watchdog gate failed")
    hits: list[str] = []
    if not record.get("hourly_watchdog"):
        hits.append("missing-hourly-watchdog")
    if not record.get("progress_file"):
        hits.append("missing-progress-file")
    if record.get("closed") and not record.get("watchdog_removed"):
        hits.append("watchdog-left-after-close")
    return _result(hits, "watchdog gate failed")


def evaluate_pr_body(body: str) -> dict[str, Any]:
    hits: list[str] = []
    if not _RETRIEVE.search(body or ""):
        hits.append("missing-retrieve-receipt")
    return _result(hits, "pr-body receipt gate failed")


def evaluate_bot_test_run(record: dict[str, Any]) -> dict[str, Any]:
    if not isinstance(record, dict):
        return _result(["record"], "bot-test-defaults gate failed")
    hits: list[str] = []
    if record.get("allure_auto_open") or record.get("allure_serve"):
        hits.append("allure-auto-open")
    if record.get("headless") is False:
        hits.append("not-headless")
    return _result(hits, "bot-test-defaults gate failed")


def evaluate_plugin_delivery(record: dict[str, Any]) -> dict[str, Any]:
    if not isinstance(record, dict):
        return _result(["record"], "plugin-ide-e2e gate failed")
    if not record.get("plugin_or_ide"):
        return _result([], "plugin-ide-e2e gate failed")
    hits: list[str] = []
    if not record.get("real_ide"):
        hits.append("missing-real-ide")
    if not record.get("headless_display"):
        hits.append("missing-headless-display")
    if not record.get("evidence_folder"):
        hits.append("missing-evidence-folder")
    return _result(hits, "plugin-ide-e2e gate failed")


def evaluate_gate_seeded_defect(record: dict[str, Any]) -> dict[str, Any]:
    if not isinstance(record, dict):
        return _result(["record"], "seeded-defect gate failed")
    if record.get("new_gate") and not record.get("seeded_defect_failed"):
        return _result(["gate-not-proven-red"], "seeded-defect gate failed")
    return _result([], "seeded-defect gate failed")


def evaluate_cleanup_safety(record: dict[str, Any]) -> dict[str, Any]:
    if not isinstance(record, dict):
        return _result(["record"], "cleanup-safety gate failed")
    hits: list[str] = []
    created = record.get("created_paths") or []
    registered = set(record.get("registered_paths") or [])
    for path in created:
        if path not in registered:
            hits.append(f"unregistered-artifact:{path}")
    if record.get("delete_worktree") and (record.get("dirty") or record.get("unpushed")):
        hits.append("refuse-dirty-or-unpushed-worktree")
    return _result(hits, "cleanup-safety gate failed")
