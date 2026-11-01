"""Fail-closed gates for CE 2026.11.01 batch (L98–L101, H1–H6, C1–C6, E1–E4, Carl L5–L8).

Carl L1–L4/L9 and Cleo stop-all prose dedupe to L90/L93/L96 (already shipped).
Each evaluate_* returns {"ok": bool, "hits": list, "error": str|None}.
"""

from __future__ import annotations

import re
from pathlib import Path
from typing import Any


def _result(hits: list[str], error: str) -> dict[str, Any]:
    return {"ok": not hits, "hits": hits, "error": None if not hits else error}


# ----- L98 worker budget / resumable batches -----
def evaluate_worker_budget(plan: dict[str, Any]) -> dict[str, Any]:
    hits: list[str] = []
    if not isinstance(plan, dict):
        return _result(["plan"], "L98 worker-budget gate failed")
    if not plan.get("resumable_batches") and not plan.get("batch_plan"):
        hits.append("missing-resumable-batches")
    if not plan.get("per_session_hit_target") and not plan.get("hit_line_target"):
        hits.append("missing-hit-line-target")
    if not plan.get("handoff_file") and not plan.get("auto_handoff"):
        hits.append("missing-handoff-file")
    return _result(hits, "L98 worker-budget gate failed")


# ----- L99 review-before-enforced -----
_RULE_LINK = re.compile(r"rules/[\w./-]+\.py|evaluate_\w+")


def evaluate_enforcement_claim(card_body: str, *, claims_enforced: bool = True) -> dict[str, Any]:
    hits: list[str] = []
    if claims_enforced and not _RULE_LINK.search(card_body or ""):
        hits.append("prose-only-enforcement-claim")
    return _result(hits, "L99 review-before-enforced gate failed")


# ----- L100 path-filter -----
def evaluate_installer_path_filter(workflow_text: str) -> dict[str, Any]:
    hits: list[str] = []
    text = workflow_text or ""
    if "chaos_installer:" not in text:
        hits.append("missing-chaos-installer-filter")
    if "chaos-engine/packs/**" not in text and "chaos-engine/packs/java/**" not in text:
        hits.append("missing-packs-glob")
    return _result(hits, "L100 path-filter gate failed")


# ----- L101 interrupt-resume -----
def evaluate_interrupt_resume(report: str) -> dict[str, Any]:
    hits: list[str] = []
    text = report or ""
    if not re.search(r"\b(live\s+(branch|PR|state|CI)|gh\s+pr\s+view|head\s+SHA|check(s)?\s+status)\b", text, re.I):
        hits.append("missing-live-state-first")
    return _result(hits, "L101 interrupt-resume gate failed")


# ----- Carl L5 alias-gaming -----
def evaluate_alias_audit(aliases: list[dict[str, Any]] | None, *, aliases_md: str = "") -> dict[str, Any]:
    hits: list[str] = []
    if not aliases_md and not aliases:
        hits.append("missing-aliases-md")
        return _result(hits, "Carl L5 alias-audit gate failed")
    for row in aliases or []:
        kind = str(row.get("kind") or row.get("type") or "").lower()
        if kind and kind not in {"en-ar-transliteration", "ar-en-transliteration", "transliteration"}:
            hits.append(f"non-transliteration:{row.get('from') or row.get('src')}")
        if row.get("wer_bypass") is True or row.get("games_wer") is True:
            hits.append(f"wer-bypass:{row.get('from') or row.get('src')}")
    if aliases_md and "transliteration" not in aliases_md.lower():
        hits.append("aliases-md-missing-transliteration-policy")
    return _result(hits, "Carl L5 alias-audit gate failed")


# ----- Carl L6 script-delta re-ack -----
def evaluate_script_delta_reack(record: dict[str, Any]) -> dict[str, Any]:
    hits: list[str] = []
    if not isinstance(record, dict):
        return _result(["record"], "Carl L6 script-delta gate failed")
    if record.get("script_changed_after_accept") and not (
        record.get("coach_reack") or record.get("script_delta_acked")
    ):
        hits.append("missing-coach-reack")
    return _result(hits, "Carl L6 script-delta gate failed")


# ----- Carl L7 resumable short steps -----
def evaluate_resumable_steps(plan: dict[str, Any]) -> dict[str, Any]:
    hits: list[str] = []
    if not isinstance(plan, dict):
        return _result(["plan"], "Carl L7 resumable-steps gate failed")
    minutes = plan.get("step_minutes") or plan.get("max_step_minutes")
    try:
        minutes_f = float(minutes) if minutes is not None else None
    except (TypeError, ValueError):
        minutes_f = None
        hits.append("bad-step-minutes")
    if minutes_f is None or minutes_f > 30:
        hits.append("step-over-30min")
    if not plan.get("commit_each_step") and not plan.get("commit_per_step"):
        hits.append("missing-commit-each-step")
    if not plan.get("resume_from_git"):
        hits.append("missing-resume-from-git")
    return _result(hits, "Carl L7 resumable-steps gate failed")


# ----- E1 stop-scope (extends L90) -----
def evaluate_stop_scope(payload: dict[str, Any]) -> dict[str, Any]:
    hits: list[str] = []
    if not isinstance(payload, dict):
        return _result(["payload"], "E1 stop-scope gate failed")
    all_flag = payload.get("all")
    if all_flag is True or str(all_flag).lower() in {"true", "1", "yes"}:
        if not payload.get("peer_safe_ack") and not payload.get("allow_cancel_peers"):
            hits.append("stop-all-without-peer-ack")
    return _result(hits, "E1 stop-scope gate failed")


# ----- E2 post-stop live-state -----
def evaluate_post_stop_live_state(record: dict[str, Any]) -> dict[str, Any]:
    hits: list[str] = []
    if not isinstance(record, dict):
        return _result(["record"], "E2 post-stop live-state gate failed")
    if record.get("stop_received") and not record.get("live_state_checked"):
        hits.append("missing-live-state-check")
    if record.get("keep_edit") and not record.get("matches_owner_words"):
        hits.append("keep-edit-without-owner-match")
    return _result(hits, "E2 post-stop live-state gate failed")


# ----- E3 YouTube metadata lint -----

def _yt_tag_len(tags: Any) -> int:
    if isinstance(tags, str):
        return len(tags)
    return sum(len(str(t)) + 1 for t in (tags or []))


def _yt_chapter_hits(chapters: list[Any]) -> list[str]:
    hits: list[str] = []
    if not chapters:
        return hits
    first = chapters[0]
    start0 = first.get("start") if isinstance(first, dict) else None
    if start0 not in (0, "0", "0:00", "00:00", "0:00:00"):
        hits.append("first-chapter-not-zero")
    prev = None
    for ch in chapters:
        if not isinstance(ch, dict):
            continue
        start = ch.get("start_seconds")
        if start is None and isinstance(ch.get("start"), (int, float)):
            start = float(ch["start"])
        dur = ch.get("duration_seconds")
        if dur is not None and float(dur) < 10:
            hits.append(f"chapter-under-10s:{ch.get('title') or start}")
        if prev is not None and start is not None and float(start) - float(prev) < 10:
            hits.append(f"chapter-gap-under-10s:{ch.get('title') or start}")
        if start is not None:
            prev = start
    return hits


def _yt_fact_hits(meta: dict[str, Any], title: str) -> list[str]:
    hits: list[str] = []
    facts = meta.get("facts") or meta.get("required_facts") or []
    wording = str(meta.get("description") or "") + " " + title
    lower = wording.lower()
    for fact in facts:
        if str(fact).lower() not in lower:
            hits.append(f"missing-fact:{fact}")
    return hits


def evaluate_yt_metadata(meta: dict[str, Any]) -> dict[str, Any]:
    hits: list[str] = []
    if not isinstance(meta, dict):
        return _result(["meta"], "E3 yt-metadata gate failed")
    title = str(meta.get("title") or "")
    if len(title) > 70:
        hits.append("title-over-70")
    if _yt_tag_len(meta.get("tags") or []) >= 500:
        hits.append("tags-over-500")
    hits.extend(_yt_chapter_hits(list(meta.get("chapters") or [])))
    hits.extend(_yt_fact_hits(meta, title))
    return _result(hits, "E3 yt-metadata gate failed")


# ----- E4 status template delta -----
_STATUS_KEYS = ("DONE", "INPROG", "NEXT", "BLOCKED")


def evaluate_status_template(report: str, *, delta_mode: bool = False) -> dict[str, Any]:
    hits: list[str] = []
    text = report or ""
    for key in _STATUS_KEYS:
        if not re.search(rf"\b{key}\b", text, re.I):
            hits.append(f"missing-{key.lower()}")
    if not re.search(r"\bdropped\b", text, re.I):
        hits.append("missing-dropped")
    if delta_mode and not re.search(r"\bdelta\b", text, re.I):
        hits.append("missing-delta")
    return _result(hits, "E4 status-template gate failed")


# ----- H1 journal progress -----
def evaluate_test_journal(record: dict[str, Any]) -> dict[str, Any]:
    hits: list[str] = []
    if not isinstance(record, dict):
        return _result(["record"], "H1 journal gate failed")
    if not record.get("progress_file"):
        hits.append("missing-progress-file")
    if record.get("detached_queue") is not True and not record.get("queue_runners"):
        hits.append("missing-detached-queue")
    if not record.get("per_test_result") and not record.get("journaled"):
        hits.append("missing-per-test-journal")
    return _result(hits, "H1 journal gate failed")


# ----- H3 picker by label -----
def evaluate_picker_select(record: dict[str, Any]) -> dict[str, Any]:
    hits: list[str] = []
    if not isinstance(record, dict):
        return _result(["record"], "H3 picker gate failed")
    if record.get("select_by_index") is True:
        hits.append("select-by-index")
    if not record.get("select_by_label") and not record.get("label") and not record.get("probe"):
        hits.append("missing-label-or-probe")
    return _result(hits, "H3 picker gate failed")


# ----- H4 map confirm settle -----
def evaluate_map_confirm(record: dict[str, Any]) -> dict[str, Any]:
    hits: list[str] = []
    if not isinstance(record, dict):
        return _result(["record"], "H4 map-confirm gate failed")
    wait = record.get("settle_wait_s")
    try:
        wait_f = float(wait) if wait is not None else None
    except (TypeError, ValueError):
        wait_f = None
    if wait_f is None or wait_f <= 0 or wait_f > 20:
        hits.append("settle-wait-out-of-range")
    if record.get("gps_location_capped") is False:
        hits.append("gps-uncapped")
    return _result(hits, "H4 map-confirm gate failed")


# ----- H5 no mid-run bump -----
def evaluate_no_midrun_bump(record: dict[str, Any]) -> dict[str, Any]:
    hits: list[str] = []
    if not isinstance(record, dict):
        return _result(["record"], "H5 mid-run bump gate failed")
    if record.get("bumped_mid_run") is True:
        hits.append("bumped-mid-run")
    return _result(hits, "H5 mid-run bump gate failed")


# ----- H6 analyze before locator fix -----
def evaluate_analyze_before_locator_fix(record: dict[str, Any]) -> dict[str, Any]:
    hits: list[str] = []
    if not isinstance(record, dict):
        return _result(["record"], "H6 analyze-before-fix gate failed")
    if record.get("locator_changed") and not record.get("ruled_out_bad_locator"):
        hits.append("locator-fix-without-analysis")
    return _result(hits, "H6 analyze-before-fix gate failed")


# ----- C1 YT Studio GMT -----
def evaluate_yt_schedule_tz(record: dict[str, Any]) -> dict[str, Any]:
    hits: list[str] = []
    if not isinstance(record, dict):
        return _result(["record"], "C1 yt-schedule-tz gate failed")
    if record.get("marked_done") and not (
        record.get("gmt_label_checked") or record.get("timezone_verified")
    ):
        hits.append("schedule-done-without-tz-check")
    return _result(hits, "C1 yt-schedule-tz gate failed")


# ----- C2 gate-fix before-values -----
def evaluate_gate_fix_before_values(record: dict[str, Any]) -> dict[str, Any]:
    hits: list[str] = []
    if not isinstance(record, dict):
        return _result(["record"], "C2 before-values gate failed")
    changed = record.get("changed_items") or []
    before = record.get("before_values") or {}
    if changed and not before:
        hits.append("missing-before-values")
    for item in changed:
        if str(item) not in before and item not in before:
            hits.append(f"missing-before:{item}")
    return _result(hits, "C2 before-values gate failed")


# ----- C3 stop-orders answer only -----
def evaluate_stop_orders(record: dict[str, Any]) -> dict[str, Any]:
    hits: list[str] = []
    if not isinstance(record, dict):
        return _result(["record"], "C3 stop-orders gate failed")
    if record.get("under_stop_order") and record.get("started_new_work"):
        hits.append("new-work-under-stop")
    return _result(hits, "C3 stop-orders gate failed")


# ----- C5 AR MSA forms -----
def evaluate_ar_vo_script(script: str) -> dict[str, Any]:
    hits: list[str] = []
    text = script or ""
    if "ثلاث" in text and "تلات" not in text:
        hits.append("msa-ثلاث")
    if re.search(r"\bذاهب\b", text):
        hits.append("msa-ذاهب")
    return _result(hits, "C5 AR VO MSA gate failed")


# ----- C6 coach pass != publish -----
def evaluate_coach_pass_not_publish(record: dict[str, Any]) -> dict[str, Any]:
    hits: list[str] = []
    if not isinstance(record, dict):
        return _result(["record"], "C6 coach-pass-not-publish gate failed")
    if record.get("published") and not (
        record.get("owner_signoff")
    ):
        hits.append("publish-without-owner-signoff")
    if record.get("coach_pass") and record.get("published") and not (
        record.get("owner_signoff")
    ):
        hits.append("coach-pass-treated-as-publish")
    return _result(hits, "C6 coach-pass-not-publish gate failed")


# ----- H2 guidance probe (mechanizable fragment) -----
def evaluate_swiftui_drag(record: dict[str, Any]) -> dict[str, Any]:
    hits: list[str] = []
    if not isinstance(record, dict):
        return _result(["record"], "H2 swiftui-drag gate failed")
    if record.get("drag") and not record.get("tallest_scrollview_fallback"):
        hits.append("missing-tallest-scrollview-fallback")
    if record.get("row_by_index"):
        hits.append("row-by-index")
    return _result(hits, "H2 swiftui-drag gate failed")


def path_filter_workflow_path() -> Path:
    return Path(".github/workflows/pr-gate.yml")
