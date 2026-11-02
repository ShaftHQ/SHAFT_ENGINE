"""
Fail-closed gates for CE 2026-11-02 mega cleanup lessons (#7692–#7721 subset).

Dedupe: Carl L5 alias-audit, E1/E2 stop-scope, C1–C6, E3 yt-metadata, Carl L6/L7,
public_facts, verify_dropped, and box_disk_guard already shipped — this module
extends or wraps them; it does not re-implement those cores.

Each evaluate_* returns {"ok": bool, "hits": list, "error": str|None}.
"""

from __future__ import annotations

import importlib.util
from pathlib import Path
from typing import Any


def _result(hits: list[str], error: str) -> dict[str, Any]:
    return {"ok": not hits, "hits": hits, "error": None if not hits else error}


def _load_sibling(name: str):
    path = Path(__file__).resolve().parent / f"{name}.py"
    spec = importlib.util.spec_from_file_location(f"ce_{name}", path)
    assert spec is not None and spec.loader is not None
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


# ----- #7692 status board: fresh poll, schema, claim provenance -----
def _status_board_receipt_hits(board: dict[str, Any]) -> list[str]:
    hits: list[str] = []
    if board.get("owner_status_requested") and not board.get("fresh_poll_complete"):
        hits.append("missing-fresh-poll")
    agents = board.get("agent_receipts") or []
    registered = board.get("registered_agents") or []
    if not (board.get("owner_status_requested") and registered):
        return hits
    got = {r.get("agent_id") for r in agents if isinstance(r, dict)}
    for aid in registered:
        if aid not in got:
            hits.append(f"missing-receipt:{aid}")
            continue
        row = next(r for r in agents if isinstance(r, dict) and r.get("agent_id") == aid)
        if row.get("same_turn") is not True and row.get("status") != "unknown":
            hits.append(f"stale-receipt:{aid}")
    return hits


def _status_board_item_hits(item: dict[str, Any]) -> list[str]:
    hits: list[str] = []
    label = item.get("id") or item.get("title")
    if not item.get("owner"):
        hits.append(f"missing-owner:{label}")
    if not item.get("link"):
        hits.append(f"missing-link:{label}")
    if item.get("section") == "open" and item.get("status") in {"done", "delivered", "closed"}:
        hits.append(f"closed-in-open:{item.get('id')}")
    if (item.get("blocker_expired") or item.get("superseded")) and item.get("listed_as_blocker"):
        hits.append(f"stale-blocker:{item.get('id')}")
    claim = item.get("external_claim")
    if claim and item.get("claim_status") not in {"unverified", "confirmed"}:
        hits.append(f"claim-unlabeled:{item.get('id')}")
    if claim and item.get("claim_status") == "confirmed" and not item.get("confirming_receipt"):
        hits.append(f"confirmed-without-receipt:{item.get('id')}")
    return hits


def evaluate_status_board(board: dict[str, Any]) -> dict[str, Any]:
    if not isinstance(board, dict):
        return _result(["board"], "#7692 status-board gate failed")
    hits = _status_board_receipt_hits(board)
    for item in board.get("items") or []:
        if isinstance(item, dict):
            hits.extend(_status_board_item_hits(item))
    return _result(hits, "#7692 status-board gate failed")


# ----- #7693 run-state registry, fan-out, token discipline -----
def evaluate_run_state(payload: dict[str, Any]) -> dict[str, Any]:
    hits: list[str] = []
    if not isinstance(payload, dict):
        return _result(["payload"], "#7693 run-state gate failed")
    state = str(payload.get("run_state") or "").lower()
    if payload.get("priority_dispatch") and state and state != "active":
        if not payload.get("unfreeze_or_resume_order"):
            hits.append(f"dispatch-while-{state or 'unknown'}")
    if payload.get("priority_dispatch") and not payload.get("ack_idempotent"):
        hits.append("missing-idempotent-ack")
    if payload.get("fyi_to_frozen") and payload.get("priority_wake"):
        hits.append("priority-fyi-to-frozen")
    if payload.get("fyi_flipped_run_state"):
        hits.append("fyi-flipped-run-state")
    if payload.get("owner_channel_output"):
        text = str(payload["owner_channel_output"])
        if payload.get("internal_notes_leaked"):
            hits.append("internal-notes-as-owner-output")
        if payload.get("owner_ack_and_result_only") is False:
            hits.append("owner-channel-not-ack-plus-result")
        if "TODO-internal" in text:
            hits.append("internal-notes-as-owner-output")
    return _result(hits, "#7693 run-state gate failed")


# ----- #7694 owner-facing media human gate + pivot escalation -----
def evaluate_owner_media_gate(record: dict[str, Any]) -> dict[str, Any]:
    hits: list[str] = []
    if not isinstance(record, dict):
        return _result(["record"], "#7694 owner-media gate failed")
    if record.get("sent_to_owner"):
        if record.get("leading_silence_or_garbage"):
            hits.append("leading-silence-or-garbage")
        if record.get("dialect_mismatch"):
            hits.append("dialect-mismatch")
        if record.get("human_likeness_pass") is not True:
            hits.append("missing-human-likeness-pass")
    if record.get("pivoted_language_or_stack") and not record.get("owner_options_brief"):
        hits.append("silent-pivot")
    if record.get("abandoned_easier_path") and not record.get("owner_options_brief"):
        hits.append("abandoned-easier-path-without-brief")
    return _result(hits, "#7694 owner-media gate failed")


# ----- #7695 platform scheduler / hashtag / composer identity -----
def _scheduler_receipt_hits(record: dict[str, Any]) -> list[str]:
    hits: list[str] = []
    if not record.get("scheduled"):
        return hits
    if not record.get("picker_iana_zone"):
        hits.append("missing-picker-iana")
    if record.get("used_studio_chrome_offset"):
        hits.append("used-studio-chrome-offset")
    if not record.get("converted_via_utc"):
        hits.append("missing-utc-conversion")
    if not (record.get("picker_zone") or record.get("picker_iana_zone")):
        hits.append("receipt-missing-picker-zone")
    if not record.get("intended_local"):
        hits.append("receipt-missing-intended-local")
    if not record.get("utc_instant"):
        hits.append("receipt-missing-utc-instant")
    return hits


def _scheduler_identity_hits(record: dict[str, Any]) -> list[str]:
    hits: list[str] = []
    if record.get("used_hashtag_dropdown"):
        hits.append("hashtag-dropdown-used")
    if record.get("description_sha256_required"):
        if not record.get("description_sha256") or not record.get("brief_description_sha256"):
            hits.append("missing-description-sha")
        elif record.get("description_sha256") != record.get("brief_description_sha256"):
            hits.append("description-sha-mismatch")
    if not record.get("posted_or_scheduled"):
        return hits
    acting = record.get("acting_identity")
    brief = record.get("brief_identity")
    if acting and brief and acting != brief:
        hits.append("composer-identity-mismatch")
    if not acting:
        hits.append("missing-acting-identity-readback")
    return hits


def evaluate_platform_scheduler(record: dict[str, Any]) -> dict[str, Any]:
    if not isinstance(record, dict):
        return _result(["record"], "#7695 platform-scheduler gate failed")
    hits = _scheduler_receipt_hits(record)
    hits.extend(_scheduler_identity_hits(record))
    return _result(hits, "#7695 platform-scheduler gate failed")


# ----- #7696 before/after/diff for live changes (extends C2) -----
def evaluate_live_change_diff(record: dict[str, Any]) -> dict[str, Any]:
    hits: list[str] = []
    if not isinstance(record, dict):
        return _result(["record"], "#7696 live-change-diff gate failed")
    prior = _load_sibling("lessons_2026_11_01")
    base = prior.evaluate_gate_fix_before_values(record)
    hits.extend(base["hits"])
    if record.get("mutated") or record.get("changed_items"):
        if not record.get("after_values") and not record.get("after_sha256"):
            hits.append("missing-after-values")
        if not record.get("diff") and not record.get("diff_recorded"):
            hits.append("missing-diff")
    return _result(hits, "#7696 live-change-diff gate failed")


# ----- #7697 FULL HALT / stop-order (extends C3) -----
def evaluate_full_halt(record: dict[str, Any]) -> dict[str, Any]:
    hits: list[str] = []
    if not isinstance(record, dict):
        return _result(["record"], "#7697 full-halt gate failed")
    prior = _load_sibling("lessons_2026_11_01")
    base = prior.evaluate_stop_orders(record)
    hits.extend(base["hits"])
    armed = record.get("full_halt_armed") or record.get("under_stop_order")
    if armed:
        if record.get("started_new_gate") or record.get("started_new_pr") or record.get(
            "started_new_delegate"
        ):
            hits.append("new-work-under-halt")
        if record.get("peer_wake_new_work_accepted"):
            hits.append("accepted-peer-wake-under-halt")
        if record.get("halt_cleared_by_peer_relay") and not record.get("owner_cleared_halt"):
            hits.append("halt-cleared-by-peer-only")
    return _result(hits, "#7697 full-halt gate failed")


# ----- #7698 board-item owner + drop-verify (extends verify_dropped) -----
def evaluate_board_item_owner(record: dict[str, Any]) -> dict[str, Any]:
    hits: list[str] = []
    if not isinstance(record, dict):
        return _result(["record"], "#7698 board-item-owner gate failed")
    if record.get("running_verify"):
        actor = record.get("actor_id")
        owner = record.get("item_owner")
        borrowed = record.get("borrowed_by")
        assigned = record.get("orchestrator_assign")
        if actor and owner and actor != owner and actor not in {borrowed, assigned}:
            hits.append("non-owner-verify")
    return _result(hits, "#7698 board-item-owner gate failed")


def evaluate_drop_verify_before_redo(record: dict[str, Any]) -> dict[str, Any]:
    hits: list[str] = []
    if not isinstance(record, dict):
        return _result(["record"], "#7698 drop-verify gate failed")
    # reuse L86 prose gate when a brief is provided
    if record.get("brief"):
        vd = _load_sibling("verify_dropped")
        hits.extend(vd.evaluate(str(record["brief"]))["hits"])
    if record.get("redo_dropped"):
        if record.get("still_dropped") is not True:
            hits.append("missing-still-dropped")
        evidence = record.get("drop_evidence") or {}
        if not (
            evidence.get("board_sha")
            or evidence.get("timestamp")
            or evidence.get("link")
            or evidence.get("orchestrator_confirmation")
        ):
            hits.append("missing-drop-evidence")
    return _result(hits, "#7698 drop-verify gate failed")


# ----- #7701 Egyptian colloquial VO (extends C5) -----
_DEFAULT_MSA_LEXICON = (
    ("ثلاث", "تلات"),
    ("ذاهب", "رايح"),
)


def evaluate_egyptian_colloquial_vo(
    script: str,
    *,
    dialect: str | None = None,
    lexicon: list[tuple[str, str]] | None = None,
) -> dict[str, Any]:
    hits: list[str] = []
    text = script or ""
    dial = (dialect or "").lower()
    if dial and dial not in {"egyptian_colloquial", "egyptian", "arz", "egy_colloquial"}:
        return _result([], "")  # gate inactive for other dialects
    if not dial:
        # when unlabeled, still run if caller used this evaluator intentionally
        dial = "egyptian_colloquial"
    pairs = lexicon or list(_DEFAULT_MSA_LEXICON)
    prior = _load_sibling("lessons_2026_11_01")
    hits.extend(prior.evaluate_ar_vo_script(text)["hits"])
    for msa, colo in pairs:
        if msa in text and colo not in text:
            code = f"msa-{msa}"
            if code not in hits:
                hits.append(code)
    return _result(hits, "#7701 egyptian-colloquial VO gate failed")


# ----- #7703 L96 residual: fold aliases must not rewrite proper nouns -----
def evaluate_fold_alias_proper_nouns(record: dict[str, Any]) -> dict[str, Any]:
    hits: list[str] = []
    if not isinstance(record, dict):
        return _result(["record"], "#7703 fold-alias gate failed")
    nouns = {str(n).casefold() for n in (record.get("proper_nouns") or [])}
    folds = record.get("fold_aliases") or {}
    if isinstance(folds, list):
        mapping = {}
        for row in folds:
            if isinstance(row, dict):
                mapping[str(row.get("from") or row.get("src") or "")] = str(
                    row.get("to") or row.get("dst") or ""
                )
            elif isinstance(row, str) and "=" in row:
                a, b = row.split("=", 1)
                mapping[a] = b
        folds = mapping
    for src, dst in (folds or {}).items():
        s = str(src).casefold()
        d = str(dst).casefold()
        if not s:
            continue
        if s == d:
            continue  # case-only / identity
        if s in nouns or d in nouns:
            # rewriting an extracted proper noun (either side) is forbidden
            hits.append(f"proper-noun-fold:{src}={dst}")
    return _result(hits, "#7703 fold-alias-proper-noun gate failed")


# ----- #7707 coach PASS ≠ avatar publish (extends C6) -----
def evaluate_avatar_publish(record: dict[str, Any]) -> dict[str, Any]:
    hits: list[str] = []
    if not isinstance(record, dict):
        return _result(["record"], "#7707 avatar-publish gate failed")
    prior = _load_sibling("lessons_2026_11_01")
    base = prior.evaluate_coach_approved_not_publish(record)
    hits.extend(base["hits"])
    if record.get("published") or record.get("external_post"):
        if record.get("owner_avatar_signoff") is not True and not record.get("owner_signoff"):
            hits.append("missing-owner-avatar-signoff")
        if not record.get("artifact_sha") and not record.get("owner_named_artifact_sha"):
            hits.append("missing-artifact-sha-ack")
        if record.get("ai_labeled") is False:
            hits.append("missing-ai-generated-label")
        if record.get("l56_blind_pass") is not True:
            hits.append("missing-l56-blind-pass")
    return _result(hits, "#7707 avatar-publish gate failed")


# ----- #7708 voice clone gate -----
def _voice_asr_hits(record: dict[str, Any], lang: str) -> list[str]:
    hits: list[str] = []
    wer = record.get("wer")
    cer = record.get("char_error")
    try:
        if lang.startswith("en") and (wer is None or float(wer) > 0.15):
            hits.append("asr-wer-over-15")
    except (TypeError, ValueError):
        hits.append("bad-wer")
    if not lang.startswith("en"):
        try:
            if cer is None or float(cer) > 0.15:
                hits.append("asr-cer-over-15")
        except (TypeError, ValueError):
            hits.append("bad-cer")
    unk = record.get("unk_frac")
    try:
        if unk is not None and float(unk) > 0.05:
            hits.append("unk-frac-over-5pct")
    except (TypeError, ValueError):
        hits.append("bad-unk-frac")
    return hits


def evaluate_voice_clone(record: dict[str, Any]) -> dict[str, Any]:
    hits: list[str] = []
    if not isinstance(record, dict):
        return _result(["record"], "#7708 voice-clone gate failed")
    if record.get("speaker_pass") is not True:
        hits.append("speaker-fail")
    lang = str(record.get("language") or "en").lower()
    hits.extend(_voice_asr_hits(record, lang))
    if record.get("joint_speaker_asr_ckpt") is not True:
        hits.append("missing-joint-ckpt")
    if record.get("language_id_required") and not record.get("language_id"):
        hits.append("missing-language-id")
    for noun in record.get("proper_noun_asr_misses") or []:
        hits.append(f"proper-noun-asr-miss:{noun}")
    return _result(hits, "#7708 voice-clone gate failed")


# ----- #7709 adversarial PASS SHA-bound -----
def evaluate_adversarial_pass_sha(record: dict[str, Any]) -> dict[str, Any]:
    hits: list[str] = []
    if not isinstance(record, dict):
        return _result(["record"], "#7709 adversarial-pass-sha gate failed")
    if record.get("merge_or_enqueue"):
        pass_sha = record.get("adversarial_pass_sha")
        head = record.get("head_sha")
        if not pass_sha:
            hits.append("missing-pass-sha")
        elif head and pass_sha != head:
            if not record.get("reack_after_commit"):
                hits.append("pass-sha-stale-needs-reack")
    if record.get("commits_after_pass") and not record.get("reack_after_commit"):
        hits.append("post-pass-commits-without-reack")
    return _result(hits, "#7709 adversarial-pass-sha gate failed")


# ----- #7710 clean-start leading audio -----
def evaluate_clean_start(record: dict[str, Any]) -> dict[str, Any]:
    hits: list[str] = []
    if not isinstance(record, dict):
        return _result(["record"], "#7710 clean-start gate failed")
    if record.get("leading_garbage") or record.get("leading_silence_then_noise"):
        hits.append("leading-garbage")
    onset_ms = record.get("onset_skew_ms")
    budget = record.get("onset_budget_ms", 150)
    try:
        if onset_ms is not None and float(onset_ms) > float(budget):
            hits.append("onset-skew-over-budget")
    except (TypeError, ValueError):
        hits.append("bad-onset-skew")
    if record.get("first_words_asr"):
        script_first = str(record.get("script_first_words") or "").casefold().split()
        asr_first = str(record.get("first_words_asr") or "").casefold().split()
        n = min(len(script_first), len(asr_first), int(record.get("first_n") or 3))
        if n and script_first[:n] != asr_first[:n]:
            hits.append("first-words-mismatch")
    return _result(hits, "#7710 clean-start gate failed")


# ----- #7711 target-dialect acceptance -----
def evaluate_target_dialect(record: dict[str, Any]) -> dict[str, Any]:
    hits: list[str] = []
    if not isinstance(record, dict):
        return _result(["record"], "#7711 target-dialect gate failed")
    if not record.get("target_dialect"):
        hits.append("missing-target-dialect")
    if record.get("register_mismatch"):
        hits.append("register-mismatch")
    if record.get("method_names_target_dialect") is False:
        hits.append("method-missing-target-dialect")
    return _result(hits, "#7711 target-dialect gate failed")


# ----- #7712 public_facts founded-year (delegates + identity pack) -----
def evaluate_founded_year(obs: dict[str, Any] | None) -> dict[str, Any]:
    pf = _load_sibling("public_facts")
    base = pf.evaluate(obs)
    hits = list(base.get("hits") or [])
    if obs is None or not isinstance(obs, dict):
        return _result(hits or ["missing-input"], "#7712 founded-year gate failed")
    # founded-year checks live in public_facts after extension; surface them here too
    year = obs.get("founded_year") or obs.get("founding_year")
    text = str(obs.get("text") or obs.get("copy") or obs.get("body") or "")
    if hasattr(pf, "FOUNDED_YEAR_PUBLIC"):
        if year is not None and str(year) != str(pf.FOUNDED_YEAR_PUBLIC):
            if str(year) in getattr(pf, "SUPERSEDED_FOUNDED_YEARS", frozenset()):
                hits.append("superseded-founded-year")
            elif obs.get("assert_founded_year") is True:
                hits.append("founded-year-mismatch")
        if text and hasattr(pf, "founded_year_hits"):
            hits.extend(pf.founded_year_hits(text))
    if obs.get("assert_identity_pack") is True:
        pack = pf.evaluate({**obs, "assert_public_trainee_figure": True})
        hits.extend(pack.get("hits") or [])
        if obs.get("assert_founded_year") is not True and year is None:
            hits.append("identity-pack-missing-founded-year")
    # unique preserve order
    seen: set[str] = set()
    uniq: list[str] = []
    for h in hits:
        if h not in seen:
            seen.add(h)
            uniq.append(h)
    return _result(uniq, "#7712 founded-year gate failed")


# ----- #7713 ASR alias audit fail-closed (extends Carl L5) -----
def evaluate_alias_audit_fail_closed(
    aliases: list[dict[str, Any]] | None, *, aliases_md: str = "", audited: bool | None = None
) -> dict[str, Any]:
    prior = _load_sibling("lessons_2026_11_01")
    base = prior.evaluate_alias_audit(aliases, aliases_md=aliases_md)
    hits = list(base["hits"])
    if audited is False or (not aliases_md and audited is not True):
        if "missing-aliases-md" not in hits and audited is False:
            hits.append("unaudited-aliases")
    for row in aliases or []:
        kind = str(row.get("kind") or row.get("type") or "").lower()
        if kind in {"content-rewrite", "synonym", "many-to-one"}:
            hits.append(f"content-rewrite-alias:{row.get('from') or row.get('src')}")
        if row.get("maps_wrong_content_word"):
            hits.append(f"wrong-content-word:{row.get('from') or row.get('src')}")
    return _result(hits, "#7713 alias-audit-fail-closed gate failed")


# ----- #7714 script-delta re-ack with sha (extends Carl L6) -----
def evaluate_script_delta_sha(record: dict[str, Any]) -> dict[str, Any]:
    prior = _load_sibling("lessons_2026_11_01")
    base = prior.evaluate_script_delta_reack(record)
    hits = list(base["hits"])
    if not isinstance(record, dict):
        return _result(["record"], "#7714 script-delta-sha gate failed")
    signed = record.get("signed_script_sha")
    working = record.get("script_sha") or record.get("working_script_sha")
    if signed and working and signed != working:
        if not (record.get("coach_reack") or record.get("script_delta_acked")):
            hits.append("script-sha-mismatch-without-reack")
        if not record.get("script_delta_note"):
            hits.append("missing-script-delta-note")
    return _result(hits, "#7714 script-delta-sha gate failed")


# ----- #7715 media durable-job playbook (extends Carl L7) -----
def evaluate_media_durable_job(plan: dict[str, Any]) -> dict[str, Any]:
    prior = _load_sibling("lessons_2026_11_01")
    base = prior.evaluate_resumable_steps(plan)
    hits = list(base["hits"])
    if not isinstance(plan, dict):
        return _result(["plan"], "#7715 media-durable-job gate failed")
    if plan.get("expected_long") or plan.get("gpu_step") or plan.get("media_step"):
        if not (plan.get("job_name") or plan.get("durable_job") or plan.get("lease")):
            hits.append("missing-durable-job")
        if plan.get("bare_nohup") or plan.get("bare_foreground_long"):
            hits.append("bare-long-runner")
        if not plan.get("resume_via_job_status"):
            hits.append("missing-job-resume-path")
    return _result(hits, "#7715 media-durable-job gate failed")


# ----- #7716 freeze hygiene -----
def evaluate_freeze_hygiene(record: dict[str, Any]) -> dict[str, Any]:
    hits: list[str] = []
    if not isinstance(record, dict):
        return _result(["record"], "#7716 freeze-hygiene gate failed")
    if record.get("dispatched_freezable_worker") and not (
        record.get("committed_before_dispatch") or record.get("durable_checkpoint")
    ):
        hits.append("dispatch-without-commit")
    if record.get("freeze_issued"):
        if record.get("broadcast_cancel_peers") and not record.get("owner_global_halt"):
            hits.append("freeze-broadcast-without-global-halt")
        if not record.get("recorded_frozen_head"):
            hits.append("missing-frozen-head")
        if not record.get("recorded_next_step"):
            hits.append("missing-next-step-on-freeze")
    return _result(hits, "#7716 freeze-hygiene gate failed")


# ----- #7718 stop-scope guard + claimed-done verify (extends E1/E2) -----
def evaluate_stop_scope_guard(payload: dict[str, Any]) -> dict[str, Any]:
    prior = _load_sibling("lessons_2026_11_01")
    base = prior.evaluate_stop_scope(payload)
    hits = list(base["hits"])
    if not isinstance(payload, dict):
        return _result(["payload"], "#7718 stop-scope-guard gate failed")
    if payload.get("broadcast") or payload.get("all") is True:
        if not payload.get("explicit_reason"):
            hits.append("broadcast-without-reason")
        if not payload.get("harness_warning_shown"):
            hits.append("broadcast-without-warning")
        if not (
            payload.get("peer_safe_ack")
            or payload.get("allow_cancel_peers")
            or payload.get("CE_ALLOW_BROADCAST_STOP")
        ):
            if "stop-all-without-peer-ack" not in hits:
                hits.append("stop-all-without-peer-ack")
    return _result(hits, "#7718 stop-scope-guard gate failed")


def evaluate_claimed_done_verify(record: dict[str, Any]) -> dict[str, Any]:
    prior = _load_sibling("lessons_2026_11_01")
    base = prior.evaluate_post_stop_live_state(record)
    hits = list(base["hits"])
    if not isinstance(record, dict):
        return _result(["record"], "#7718 claimed-done-verify gate failed")
    if record.get("claiming_done") or record.get("redo_marked_done"):
        if not record.get("live_surface_checked"):
            hits.append("missing-live-surface-check")
        verdict = str(record.get("verify_verdict") or "").upper()
        if verdict not in {"PASS", "SKIP", "REDO"}:
            hits.append("missing-verify-verdict")
    return _result(hits, "#7718 claimed-done-verify gate failed")


# ----- #7719 publish metadata lint (extends E3 + unconfirmed links) -----
def evaluate_publish_metadata(meta: dict[str, Any]) -> dict[str, Any]:
    prior = _load_sibling("lessons_2026_11_01")
    base = prior.evaluate_yt_metadata(meta)
    hits = list(base["hits"])
    if not isinstance(meta, dict):
        return _result(["meta"], "#7719 publish-metadata gate failed")
    link = meta.get("link") or meta.get("url")
    if meta.get("deep_link_unconfirmed"):
        if link and not meta.get("used_site_root"):
            # deep link published while unconfirmed
            if not str(link).rstrip("/").count("/") <= 2:  # crude: root-ish
                hits.append("unconfirmed-deep-link")
        if meta.get("used_site_root") and not meta.get("todo_upgrade_link"):
            hits.append("missing-link-upgrade-todo")
    return _result(hits, "#7719 publish-metadata gate failed")


# ----- #7720 BLOCKED-INFRA apt (delegates to box_disk_guard) -----
def evaluate_blocked_infra_apt(record: dict[str, Any]) -> dict[str, Any]:
    hits: list[str] = []
    if not isinstance(record, dict):
        return _result(["record"], "#7720 blocked-infra-apt gate failed")
    # Prefer live helper when injectable probe fields present
    root = Path(__file__).resolve().parents[2]
    import sys

    if str(root) not in sys.path:
        sys.path.insert(0, str(root))
    try:
        from scripts.ci import box_disk_guard as bdg
    except Exception:
        bdg = None
    if bdg is not None:
        reasons = bdg.blocked_infra_reasons(
            record.get("path") or ".",
            free=record.get("free"),
            used_percent=record.get("used_percent"),
            swap_kib=record.get("swap_kib", 1),  # default present unless injected
            apt_mirror_ok=record.get("apt_mirror_ok"),
            apt_mirror_5xx=record.get("apt_mirror_5xx"),
            apt_lists_unsigned=record.get("apt_lists_unsigned"),
        )
        for r in reasons:
            if r.startswith("apt-"):
                hits.append(r)
    else:
        if record.get("apt_mirror_5xx"):
            hits.append("apt-mirror-5xx")
        if record.get("apt_lists_unsigned"):
            hits.append("apt-lists-unsigned")
        if record.get("apt_mirror_ok") is False:
            hits.append("apt-mirror-unreachable")
    if hits and record.get("used_force_apt_flag"):
        hits.append("forced-apt-despite-infra-block")
    if record.get("used_force_apt_flag") and (
        record.get("apt_mirror_5xx") or record.get("apt_lists_unsigned")
    ):
        if "forced-apt-despite-infra-block" not in hits:
            hits.append("forced-apt-despite-infra-block")
    return _result(hits, "#7720 blocked-infra-apt gate failed")


# ----- #7721 SEO foundingDate + admin URL + scheduler TZ proof -----
def evaluate_seo_founding_date(record: dict[str, Any]) -> dict[str, Any]:
    hits: list[str] = []
    if not isinstance(record, dict):
        return _result(["record"], "#7721 seo-founding-date gate failed")
    marketing = record.get("founding_year_marketing")
    registry = record.get("founding_year_registry")
    if (
        marketing is not None
        and registry is not None
        and str(marketing) != str(registry)
        and record.get("foundingDate_published")
        and not record.get("owner_product_decision")
    ):
        hits.append("foundingDate-year-mismatch-without-decision")
    return _result(hits, "#7721 seo-founding-date gate failed")


def evaluate_admin_url_shape(record: dict[str, Any]) -> dict[str, Any]:
    hits: list[str] = []
    if not isinstance(record, dict):
        return _result(["record"], "#7721 admin-url-shape gate failed")
    if record.get("slug_admin_failed") and not record.get("used_numeric_org_id"):
        hits.append("slug-admin-without-numeric-fallback")
    if record.get("hardcoded_tenant_id_in_harness"):
        hits.append("hardcoded-tenant-id")
    return _result(hits, "#7721 admin-url-shape gate failed")


def evaluate_scheduler_tz_proof(record: dict[str, Any]) -> dict[str, Any]:
    hits: list[str] = []
    if not isinstance(record, dict):
        return _result(["record"], "#7721 scheduler-tz-proof gate failed")
    if record.get("scheduled"):
        for key in ("owner_tz", "picker_tz", "utc_instant"):
            if not record.get(key):
                hits.append(f"missing-{key}")
        if not record.get("proof_path") and not record.get("proof"):
            hits.append("missing-proof-path")
        if not record.get("converted_via_utc"):
            hits.append("missing-utc-conversion")
        if record.get("typed_owner_local_into_wrong_zone_picker"):
            hits.append("owner-local-into-wrong-zone-picker")
    return _result(hits, "#7721 scheduler-tz-proof gate failed")


# Convenience map for SPEC / tests
ISSUE_EVALUATORS = {
    7692: "evaluate_status_board",
    7693: "evaluate_run_state",
    7694: "evaluate_owner_media_gate",
    7695: "evaluate_platform_scheduler",
    7696: "evaluate_live_change_diff",
    7697: "evaluate_full_halt",
    7698: ("evaluate_board_item_owner", "evaluate_drop_verify_before_redo"),
    7701: "evaluate_egyptian_colloquial_vo",
    7703: "evaluate_fold_alias_proper_nouns",
    7707: "evaluate_avatar_publish",
    7708: "evaluate_voice_clone",
    7709: "evaluate_adversarial_pass_sha",
    7710: "evaluate_clean_start",
    7711: "evaluate_target_dialect",
    7712: "evaluate_founded_year",
    7713: "evaluate_alias_audit_fail_closed",
    7714: "evaluate_script_delta_sha",
    7715: "evaluate_media_durable_job",
    7716: "evaluate_freeze_hygiene",
    7718: ("evaluate_stop_scope_guard", "evaluate_claimed_done_verify"),
    7719: "evaluate_publish_metadata",
    7720: "evaluate_blocked_infra_apt",
    7721: (
        "evaluate_seo_founding_date",
        "evaluate_admin_url_shape",
        "evaluate_scheduler_tz_proof",
    ),
}
