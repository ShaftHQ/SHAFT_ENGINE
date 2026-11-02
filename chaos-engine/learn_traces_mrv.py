#!/usr/bin/env python3
"""Map-reduce-verify file contract for portable learn (#5847).

Isolated from collect so host TUI workflows are never vendored here.
"""

from __future__ import annotations

import json
import re
from collections.abc import Callable
from datetime import datetime, timezone
from pathlib import Path

BATCH_SIZE = 8
FORBIDDEN_TARGET_MARKERS = (
    "~/.grok/skills",
    "/.grok/skills/",
    "GROK_HOME/skills",
    "\\grok\\skills\\",
)
OVERLAY_TARGET_PREFIXES = (
    "chaos-engine/",
    ".chaos-engine/",
    "patches/",
)
PHRASE_TOKEN = re.compile(r"[A-Za-z][A-Za-z0-9_./-]{2,}")
STOPWORDS = frozenset(
    {
        "the", "and", "for", "that", "with", "this", "from", "have", "were", "been",
        "will", "your", "into", "about", "there", "their", "what", "when", "where",
        "which", "while", "would", "could", "should", "please", "thanks", "hello",
        "just", "like", "want", "need", "make", "sure", "also", "then", "than",
        "them", "they", "you", "are", "was", "not", "but", "all", "any", "can",
        "our", "out", "use", "using", "used", "run", "running", "file", "files",
        "path", "code", "test", "tests", "okay", "yes", "no",
    }
)

MAP_PROMPT = """# Map batch (isolated)

Read only the session JSON files listed in this batch. Extract:
- repeated human phrases worth encoding as overlay policy
- stale skill lines / unused loaded surfaces
- gaps (jobs humans re-explained)

Cite session `id` values. Invent nothing. Write JSON to the batch path.
Do not edit ~/.grok/skills. Do not open other batches.
"""

REDUCE_PROMPT = """# Reduce (isolated)

Fold every map/batch-*.json note into one synthesis.json.
Open candidate overlay files under chaos-engine/ (or installed .chaos-engine/
source pointers) as context only. Invent nothing.
Targets must be git-tracked overlay paths (chaos-engine/... or patches/*.diff).
Never propose ~/.grok/skills/... as the primary write.
"""

VERIFY_PROMPT = """# Verify skeptics (isolated)

Three independent checks: phrases, stale lines, deletes.
Keep or drop candidates from reduce/synthesis.json. Add nothing.
Fail closed when evidence (session ids) is missing.
First-ever run: step/curate deletes — never auto-delete.
"""


def load_manifest(run_dir: Path) -> dict[str, object]:
    """Load and validate run-dir manifest.json."""
    path = run_dir / "manifest.json"
    if not path.is_file():
        raise FileNotFoundError(f"missing manifest.json under {run_dir}")
    payload = json.loads(path.read_text(encoding="utf-8"))
    if not isinstance(payload, dict):
        raise ValueError("manifest.json must be an object")
    return payload


def load_session(run_dir: Path, relative: str) -> dict[str, object]:
    """Load one redacted session JSON relative to the run dir."""
    path = run_dir / relative
    payload = json.loads(path.read_text(encoding="utf-8"))
    if not isinstance(payload, dict):
        raise ValueError(f"session {relative} must be an object")
    return payload


def _row_role(row: dict[str, object]) -> str:
    for key in ("role", "type", "kind"):
        value = row.get(key)
        if isinstance(value, str) and value.strip():
            return value.strip().casefold()
    message = row.get("message")
    if isinstance(message, dict):
        role = message.get("role")
        if isinstance(role, str):
            return role.strip().casefold()
    return ""


def _append_str(blobs: list[str], value: object) -> None:
    if isinstance(value, str) and value:
        blobs.append(value)


def _append_from_sequence(blobs: list[str], value: list[object]) -> None:
    for block in value:
        if isinstance(block, str):
            blobs.append(block)
        elif isinstance(block, dict):
            _append_str(blobs, block.get("text"))


def _append_from_mapping(blobs: list[str], value: dict[str, object]) -> None:
    for nested in ("content", "text", "role"):
        _append_str(blobs, value.get(nested))
    content = value.get("content")
    if isinstance(content, list):
        _append_from_sequence(blobs, content)


def _text_blobs(row: dict[str, object]) -> list[str]:
    blobs: list[str] = []
    for key in ("content", "text", "message", "prompt", "input"):
        value = row.get(key)
        if isinstance(value, str):
            blobs.append(value)
        elif isinstance(value, dict):
            _append_from_mapping(blobs, value)
        elif isinstance(value, list):
            _append_from_sequence(blobs, value)
    return blobs


def session_user_text(session: dict[str, object]) -> str:
    """Concatenate human/user message text from a session payload."""
    messages = session.get("messages")
    blobs: list[str] = []
    if not isinstance(messages, list):
        return ""
    for row in messages:
        if not isinstance(row, dict):
            continue
        role = _row_role(row)
        row_type = str(row.get("type", "")).casefold()
        if role in {"user", "human", "prompt", "user_message"} or row_type == "user":
            blobs.extend(_text_blobs(row))
        payload = row.get("payload")
        if isinstance(payload, dict):
            ptype = str(payload.get("type") or payload.get("role") or "").casefold()
            if ptype in {"user_message", "user", "message"}:
                message = payload.get("message")
                if isinstance(message, str):
                    blobs.append(message)
                for key in ("text", "content"):
                    value = payload.get(key)
                    if isinstance(value, str):
                        blobs.append(value)
    return "\n".join(blobs)


def phrase_counts(text: str) -> dict[str, int]:
    """Count unigram/bigram tokens for offline map heuristics."""
    counts: dict[str, int] = {}
    tokens = [
        tok.casefold()
        for tok in PHRASE_TOKEN.findall(text)
        if tok.casefold() not in STOPWORDS and not tok.isdigit()
    ]
    for index, token in enumerate(tokens):
        counts[token] = counts.get(token, 0) + 1
        if index + 1 < len(tokens):
            bigram = f"{token} {tokens[index + 1]}"
            counts[bigram] = counts.get(bigram, 0) + 1
    return counts


def is_forbidden_target(target: str) -> bool:
    """True when target points at ~/.grok skills or equivalents."""
    normalized = target.strip().replace("\\", "/")
    lowered = normalized.casefold()
    if any(marker.casefold() in lowered for marker in FORBIDDEN_TARGET_MARKERS):
        return True
    return lowered.startswith("~/.grok/") or "/.grok/skills" in lowered


def is_git_tracked_overlay_target(target: str) -> bool:
    """True for chaos-engine/ .chaos-engine/ or patches/ write targets."""
    normalized = target.strip().replace("\\", "/").lstrip("./")
    if is_forbidden_target(normalized):
        return False
    return any(normalized.startswith(prefix) for prefix in OVERLAY_TARGET_PREFIXES)


def _write_json(path: Path, payload: object) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(payload, indent=2) + "\n", encoding="utf-8")


def _session_entries(manifest: dict[str, object]) -> list[dict[str, object]]:
    sessions = manifest.get("sessions")
    if not isinstance(sessions, list):
        raise ValueError("manifest.sessions must be a list")
    return [item for item in sessions if isinstance(item, dict)]


def prepare(run_dir: Path) -> dict[str, object]:
    """Write isolated prompts + empty map/reduce/verify layout for host agents."""
    run_dir = run_dir.resolve()
    manifest = load_manifest(run_dir)
    kept = _session_entries(manifest)
    prompts = run_dir / "prompts"
    for directory in (prompts, run_dir / "map", run_dir / "reduce", run_dir / "verify"):
        directory.mkdir(parents=True, exist_ok=True)
    (prompts / "map.md").write_text(MAP_PROMPT.strip() + "\n", encoding="utf-8")
    (prompts / "reduce.md").write_text(REDUCE_PROMPT.strip() + "\n", encoding="utf-8")
    (prompts / "verify.md").write_text(VERIFY_PROMPT.strip() + "\n", encoding="utf-8")
    batches: list[dict[str, object]] = []
    span = max(len(kept), 1)
    for start in range(0, span, BATCH_SIZE):
        chunk = kept[start : start + BATCH_SIZE]
        batch_id = f"batch-{start // BATCH_SIZE:03d}"
        batch = {
            "id": batch_id,
            "schemaVersion": 1,
            "sessions": [
                {"id": item.get("id"), "path": item.get("path"), "host": item.get("host")}
                for item in chunk
            ],
            "notes": [],
            "status": "pending",
        }
        target = run_dir / "map" / f"{batch_id}.json"
        if not target.is_file():
            _write_json(target, batch)
        batches.append({"id": batch_id, "path": f"map/{batch_id}.json", "size": len(chunk)})
    layout = {
        "schemaVersion": 1,
        "kind": "learn-traces-layout",
        "createdAt": datetime.now(timezone.utc).isoformat(),
        "sessions_kept": int(manifest.get("sessions_kept") or len(kept)),
        "batches": batches,
        "prompts": ["prompts/map.md", "prompts/reduce.md", "prompts/verify.md"],
        "next": (
            "Fill map/batch-*.json via isolated subagents (or `learn --offline`), "
            "then reduce/verify; finalize writes report.md + actions.json. "
            "Never write ~/.grok/skills."
        ),
    }
    _write_json(run_dir / "layout.json", layout)
    return layout


def _collect_phrase_sessions(run_dir: Path, sessions_meta: list[dict[str, object]]) -> dict[str, set[str]]:
    phrase_sessions: dict[str, set[str]] = {}
    for item in sessions_meta:
        relative = str(item.get("path") or "")
        sid = str(item.get("id") or "")
        if not relative or not sid:
            continue
        try:
            session = load_session(run_dir, relative)
        except (OSError, json.JSONDecodeError, ValueError):
            continue
        for phrase in phrase_counts(session_user_text(session)):
            phrase_sessions.setdefault(phrase, set()).add(sid)
    return phrase_sessions


def _rank_repeated_phrases(phrase_sessions: dict[str, set[str]]) -> list[tuple[str, list[str]]]:
    repeated = [
        (phrase, sorted(sids))
        for phrase, sids in phrase_sessions.items()
        if len(sids) >= 2 and " " in phrase
    ]
    repeated.sort(key=lambda item: (-len(item[1]), item[0]))
    if repeated:
        return repeated[:12]
    fallback = [
        (phrase, sorted(sids))
        for phrase, sids in phrase_sessions.items()
        if len(sids) >= 1 and " " not in phrase and len(phrase) >= 6
    ]
    fallback.sort(key=lambda item: (-len(item[1]), item[0]))
    return fallback[:12]


def _notes_for_batch(batch_sids: set[str], repeated: list[tuple[str, list[str]]]) -> list[dict[str, object]]:
    notes: list[dict[str, object]] = []
    for phrase, sids in repeated:
        hit = sorted(batch_sids & set(sids))
        if not hit:
            continue
        notes.append(
            {
                "kind": "repeated_phrase",
                "phrase": phrase,
                "sessions": hit,
                "proposedTarget": "chaos-engine/references/harness-learn.md",
                "summary": f"Encode repeated guidance: {phrase}",
            }
        )
    if notes or not batch_sids:
        return notes
    return [
        {
            "kind": "coverage_ack",
            "phrase": "",
            "sessions": sorted(batch_sids),
            "proposedTarget": "chaos-engine/references/learn-traces.md",
            "summary": "No repeated phrase; keep portable learn contract documented",
        }
    ]


def _write_map_batches(run_dir: Path, repeated: list[tuple[str, list[str]]]) -> None:
    map_dir = run_dir / "map"
    map_dir.mkdir(exist_ok=True)
    batch_files = sorted(map_dir.glob("batch-*.json"))
    if not batch_files:
        prepare(run_dir)
        batch_files = sorted(map_dir.glob("batch-*.json"))
    for batch_path in batch_files:
        batch = json.loads(batch_path.read_text(encoding="utf-8"))
        batch_sids = {
            str(entry.get("id"))
            for entry in (batch.get("sessions") or [])
            if isinstance(entry, dict)
        }
        batch["notes"] = _notes_for_batch(batch_sids, repeated)
        batch["status"] = "mapped"
        _write_json(batch_path, batch)


def _gather_map_notes(run_dir: Path) -> tuple[list[dict[str, object]], set[str]]:
    notes: list[dict[str, object]] = []
    cited: set[str] = set()
    for batch_path in sorted((run_dir / "map").glob("batch-*.json")):
        batch = json.loads(batch_path.read_text(encoding="utf-8"))
        for note in batch.get("notes") or []:
            if not isinstance(note, dict):
                continue
            notes.append(note)
            for sid in note.get("sessions") or []:
                cited.add(str(sid))
    return notes, cited


def _merge_or_append_candidate(
    candidates: list[dict[str, object]],
    *,
    kind: object,
    target: str,
    phrase: object,
    summary: object,
    sessions: list[str],
) -> None:
    for existing in candidates:
        if (
            existing.get("kind") == kind
            and existing.get("target") == target
            and existing.get("phrase") == phrase
        ):
            merged = set(str(s) for s in (existing.get("evidenceSessions") or []))
            merged.update(sessions)
            existing["evidenceSessions"] = sorted(merged)
            return
    candidates.append(
        {
            "id": f"cand-{len(candidates) + 1:03d}",
            "kind": "overlay-edit" if kind != "delete" else "curate-delete",
            "target": target,
            "phrase": phrase or "",
            "summary": summary or f"Overlay edit for {target}",
            "evidenceSessions": sessions,
            "delivery": "git-pr",
            "autoApply": False,
            "autoDelete": False,
        }
    )


def _candidates_from_notes(notes: list[dict[str, object]]) -> list[dict[str, object]]:
    candidates: list[dict[str, object]] = []
    seen: set[str] = set()
    for note in notes:
        target = str(note.get("proposedTarget") or "").strip()
        if not target or is_forbidden_target(target):
            continue
        if not is_git_tracked_overlay_target(target):
            target = "patches/learn-overlay.diff"
        key = f"{note.get('kind')}:{target}:{note.get('phrase')}"
        sessions = sorted(str(s) for s in (note.get("sessions") or []))
        if key in seen:
            _merge_or_append_candidate(
                candidates,
                kind=note.get("kind"),
                target=target,
                phrase=note.get("phrase"),
                summary=note.get("summary"),
                sessions=sessions,
            )
            continue
        seen.add(key)
        _merge_or_append_candidate(
            candidates,
            kind=note.get("kind"),
            target=target,
            phrase=note.get("phrase"),
            summary=note.get("summary"),
            sessions=sessions,
        )
    return candidates


def _write_reduce(run_dir: Path, cited: set[str], candidates: list[dict[str, object]]) -> dict[str, object]:
    synthesis = {
        "schemaVersion": 1,
        "kind": "learn-traces-reduce",
        "sessionsCited": sorted(cited),
        "candidates": candidates,
        "policy": {
            "forbidHomeSkills": True,
            "firstRunCurateOnly": True,
            "delivery": "git-tracked PR or patches/*.diff",
        },
    }
    _write_json(run_dir / "reduce" / "synthesis.json", synthesis)
    return synthesis


def _verify_candidate(candidate: dict[str, object]) -> str | None:
    evidence = candidate.get("evidenceSessions") or []
    target = str(candidate.get("target") or "")
    if not evidence:
        return "missing_evidence"
    if is_forbidden_target(target):
        return "forbidden_home_skill"
    if not is_git_tracked_overlay_target(target):
        return "not_git_tracked_overlay"
    return None


def _write_verify(
    run_dir: Path, candidates: list[dict[str, object]], cited: set[str]
) -> dict[str, object]:
    kept: list[dict[str, object]] = []
    dropped: list[dict[str, object]] = []
    for candidate in candidates:
        reason = _verify_candidate(candidate)
        if reason:
            dropped.append({**candidate, "dropReason": reason})
            continue
        action = dict(candidate)
        if action.get("kind") == "curate-delete":
            action["autoDelete"] = False
            action["summary"] = (
                str(action.get("summary") or "")
                + " (first-run: curate only, no auto-delete)"
            ).strip()
        kept.append(action)
    # First-run: deletes never survive as auto actions.
    kept = [item for item in kept if item.get("kind") != "curate-delete"]
    phrases = {
        "schemaVersion": 1,
        "skeptic": "phrases",
        "kept": [item for item in kept if item.get("phrase")],
        "dropped": [item for item in dropped if item.get("phrase")],
    }
    stale = {
        "schemaVersion": 1,
        "skeptic": "stale_lines",
        "kept": [item for item in kept if item.get("kind") == "overlay-edit"],
        "dropped": [item for item in dropped if item.get("kind") == "overlay-edit"],
    }
    deletes = {
        "schemaVersion": 1,
        "skeptic": "deletes",
        "kept": [],
        "dropped": [item for item in dropped if item.get("kind") == "curate-delete"],
        "policy": "first-ever run is step/curate, never auto-delete",
    }
    verify_dir = run_dir / "verify"
    _write_json(verify_dir / "phrases.json", phrases)
    _write_json(verify_dir / "stale.json", stale)
    _write_json(verify_dir / "deletes.json", deletes)
    verdict = {
        "schemaVersion": 1,
        "kind": "learn-traces-verify",
        "kept": kept,
        "dropped": dropped,
        "sessionsCited": sorted(cited),
    }
    _write_json(verify_dir / "verdict.json", verdict)
    return verdict


def offline_map_reduce_verify(run_dir: Path) -> dict[str, object]:
    """Deterministic zero-LLM fill of the map/reduce/verify file contract."""
    run_dir = run_dir.resolve()
    manifest = load_manifest(run_dir)
    if not (run_dir / "layout.json").is_file():
        prepare(run_dir)
    sessions_meta = _session_entries(manifest)
    repeated = _rank_repeated_phrases(_collect_phrase_sessions(run_dir, sessions_meta))
    _write_map_batches(run_dir, repeated)
    notes, cited = _gather_map_notes(run_dir)
    candidates = _candidates_from_notes(notes)
    _write_reduce(run_dir, cited, candidates)
    return _write_verify(run_dir, candidates, cited)


def _require_verify_artifacts(run_dir: Path) -> dict[str, object]:
    verdict_path = run_dir / "verify" / "verdict.json"
    if verdict_path.is_file():
        payload = json.loads(verdict_path.read_text(encoding="utf-8"))
        if isinstance(payload, dict):
            return payload
    kept: list[dict[str, object]] = []
    dropped: list[dict[str, object]] = []
    cited: set[str] = set()
    for name in ("phrases.json", "stale.json", "deletes.json"):
        path = run_dir / "verify" / name
        if not path.is_file():
            continue
        payload = json.loads(path.read_text(encoding="utf-8"))
        if not isinstance(payload, dict):
            continue
        for item in payload.get("kept") or []:
            if isinstance(item, dict):
                kept.append(item)
                for sid in item.get("evidenceSessions") or item.get("sessions") or []:
                    cited.add(str(sid))
        for item in payload.get("dropped") or []:
            if isinstance(item, dict):
                dropped.append(item)
    verify_dir = run_dir / "verify"
    if not verify_dir.is_dir():
        raise FileNotFoundError("verify/ missing; run offline map-reduce or host skeptics first")
    present = any((verify_dir / name).is_file() for name in (
        "verdict.json", "phrases.json", "stale.json", "deletes.json"
    ))
    if not present:
        raise FileNotFoundError("verify artifacts missing; fail closed")
    return {
        "schemaVersion": 1,
        "kind": "learn-traces-verify",
        "kept": kept,
        "dropped": dropped,
        "sessionsCited": sorted(cited),
    }


def _normalize_action(item: dict[str, object], index: int) -> dict[str, object] | None:
    target = str(item.get("target") or "").strip()
    evidence = [str(s) for s in (item.get("evidenceSessions") or [])]
    if not evidence:
        return None
    if is_forbidden_target(target) or not is_git_tracked_overlay_target(target):
        return None
    return {
        "id": item.get("id") or f"act-{index:03d}",
        "kind": item.get("kind") or "overlay-edit",
        "target": target,
        "summary": item.get("summary") or "",
        "phrase": item.get("phrase") or "",
        "evidenceSessions": evidence,
        "delivery": "git-pr",
        "autoApply": False,
        "autoDelete": False,
    }


def _reject_reason(item: dict[str, object]) -> str:
    evidence = item.get("evidenceSessions") or []
    if not evidence:
        return "missing_evidence"
    return "non_git_tracked_or_home"



def _write_report(
    run_dir: Path,
    *,
    kept_count: int,
    cited_count: int,
    actions: list[dict[str, object]],
    rejected_count: int,
    coverage_line: str,
) -> None:
    lines = [
        "# Learn traces report",
        "",
        coverage_line,
        "",
        "## Overview",
        "",
        f"- Sessions kept: {kept_count}",
        f"- Sessions cited: {cited_count}",
        f"- Actions proposed: {len(actions)}",
        f"- Rejected (fail-closed): {rejected_count}",
        "",
        "## Actions (git-tracked overlay only)",
        "",
    ]
    if not actions:
        lines.extend(["_No overlay actions survived verify._", ""])
    else:
        lines.extend(
            [
                "| id | target | summary | evidence |",
                "| --- | --- | --- | --- |",
            ]
        )
        for action in actions:
            evidence = ", ".join(action.get("evidenceSessions") or [])  # type: ignore[arg-type]
            lines.append(
                f"| {action.get('id')} | `{action.get('target')}` | "
                f"{action.get('summary')} | {evidence} |"
            )
        lines.append("")
    lines.extend(
        [
            "## Policy",
            "",
            "- Targets are git-tracked `chaos-engine/` / `.chaos-engine/` source PR paths or `patches/*.diff`.",
            "- Never write learned skills under `~/.grok/skills`.",
            "- First-ever run is step/curate, never auto-delete.",
            "- Host TUI `/learn` is not required; this runner owns the portable contract.",
            "",
        ]
    )
    (run_dir / "report.md").write_text("\n".join(lines), encoding="utf-8")


def finalize(run_dir: Path) -> dict[str, object]:
    """Fail-closed report.md + actions.json from verify artifacts."""
    run_dir = run_dir.resolve()
    manifest = load_manifest(run_dir)
    kept_count = int(manifest.get("sessions_kept") or 0)
    verdict = _require_verify_artifacts(run_dir)
    raw_actions = [item for item in (verdict.get("kept") or []) if isinstance(item, dict)]
    actions: list[dict[str, object]] = []
    rejected: list[dict[str, object]] = []
    cited: set[str] = set(str(s) for s in (verdict.get("sessionsCited") or []))
    for item in raw_actions:
        cited.update(str(s) for s in (item.get("evidenceSessions") or []))
        normalized = _normalize_action(item, len(actions) + 1)
        if normalized is None:
            rejected.append({**item, "rejectReason": _reject_reason(item)})
            continue
        actions.append(normalized)
    coverage_line = (
        f"Coverage: {kept_count} sessions kept in manifest; "
        f"{len(cited)} cited in map-reduce-verify."
    )
    actions_doc = {
        "schemaVersion": 1,
        "kind": "learn-traces-actions",
        "createdAt": datetime.now(timezone.utc).isoformat(),
        "sessions_kept": kept_count,
        "sessions_cited": sorted(cited),
        "coverageLine": coverage_line,
        "actions": actions,
        "rejected": rejected,
        "policy": {
            "forbidHomeSkills": True,
            "forbiddenPrefixes": list(FORBIDDEN_TARGET_MARKERS),
            "overlayPrefixes": list(OVERLAY_TARGET_PREFIXES),
            "delivery": "git-tracked overlay PR or patches/*.diff — never ~/.grok/skills",
            "firstRunCurateOnly": True,
        },
    }
    _write_json(run_dir / "actions.json", actions_doc)
    _write_report(
        run_dir,
        kept_count=kept_count,
        cited_count=len(cited),
        actions=actions,
        rejected_count=len(rejected),
        coverage_line=coverage_line,
    )
    return actions_doc


def learn(
    run_dir: Path,
    *,
    collect_fn: Callable[[Path, Path], dict[str, object]],
    home: Path | None = None,
    collect_first: bool = True,
    offline: bool = True,
) -> dict[str, object]:
    """One portable /learn entry: collect → prepare → map-reduce-verify → report."""
    run_dir = run_dir.resolve()
    run_dir.mkdir(parents=True, exist_ok=True)
    if collect_first or not (run_dir / "manifest.json").is_file():
        collect_fn(home or Path.home(), run_dir)
    layout = prepare(run_dir)
    if offline:
        offline_map_reduce_verify(run_dir)
    elif not (run_dir / "verify" / "verdict.json").is_file():
        return {
            "schemaVersion": 1,
            "kind": "learn-traces-learn",
            "run_dir": str(run_dir),
            "layout": layout,
            "status": "awaiting_host_map_reduce_verify",
            "next": (
                "Run isolated subagents per prompts/; then "
                "`python3 .chaos-engine/learn_traces.py finalize --run-dir "
                f"{run_dir}`"
            ),
        }
    actions = finalize(run_dir)
    return {
        "schemaVersion": 1,
        "kind": "learn-traces-learn",
        "run_dir": str(run_dir),
        "status": "complete",
        "sessions_kept": actions.get("sessions_kept"),
        "actions": len(actions.get("actions") or []),
        "report": "report.md",
        "actionsPath": "actions.json",
        "coverageLine": actions.get("coverageLine"),
    }
