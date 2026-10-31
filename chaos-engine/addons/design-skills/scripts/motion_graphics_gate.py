"""L65 + L65 follow-ups + L73: motion-graphics acceptance gate (fail-closed).

Checks (Carl field miss after r1 MG sample passed indep review; r2/r3 onset):
(a) OCR rendered frames vs verbatim transcript; catch AR/EN bidi garble
    (Latin runs isolated with U+2066 FIRST-STRONG ISOLATE / U+2069 PDI).
(b) Kinetic deliverables: FAIL any static text block >3s OR >6 words.
(c) Caption onset ≤150ms measured vs word-level ASR/aligner timings
    (never trust builder-supplied onset_ms_vs_speech / onset_ms alone).
(d) Transcript manually verified against audio (flag required).
(e) Diagrams animate draw-on, not pop/fade.
Plus: caption lines <=32 chars, 1-2 lines split at phrase boundaries;
share only reviewed + owner-approved deliverables (named acceptance when
applicable); no WIP sample shares. Missing observations / ASR → FAIL.

L65 follow-ups / L73 (CE 2026.10.31.7):
1. Require ``asr_path`` artifact + ``asr_sha256`` + ``asr_tool`` provenance;
   reject inline builder ``asr_words`` without path; reject ``asr_source`` in
   {cue-aligned, cue_aligned, from-cues, builder}.
2. Unmatched cue text → FAIL (no nearest-start fallback).
3. Duplicate tokens match sequentially (consume ASR hits in order).
4. Perfect 0 ms max across all cues is suspect without independent proof
   (derived from data: ASR timings must not be cue-synced; builder ``asr_independent_proof`` is ignored).

L81 NO-SELF-CLOCK (CE 2026.10.31.8):
- Never take onset from the cue under test; audio-only ASR onsets only.
- ``onset_method`` in {mid_speech_voiced, self_clock, cue_start, ...} with
  phrase_onset == cue_start → FAIL (hid 360–940 ms errors in cartoon r3).
- No measurable onset → FAIL (never report 0 ms from a missing clock).
- Mutant: one mid-speech cue shifted +500 ms vs committed ASR must FAIL.
"""
from __future__ import annotations

import hashlib
import json
import re
from pathlib import Path
from typing import Any

LRI = "\u2066"  # FIRST-STRONG ISOLATE (Carl: U+2066)
PDI = "\u2069"  # POP DIRECTIONAL ISOLATE
MAX_CAPTION_LINE_CHARS = 32
MAX_CAPTION_LINES = 2
MAX_STATIC_SECONDS = 3.0
MAX_STATIC_WORDS = 6
MAX_ONSET_MS = 150
_LATIN_RUN = re.compile(r"[A-Za-z][A-Za-z0-9._/+#-]{0,80}")
_DRAW_ON = frozenset({"draw-on", "draw_on", "drawon", "none", "n/a", "static"})
_POP_FADE = frozenset({"pop", "fade", "fade-in", "appear"})
_WORD_KEYS = ("word", "w", "t", "text")
_START_KEYS = ("start", "s", "start_s", "t_start")
_ASR_KEYS = ("asr_words", "aligner_words", "asr", "words")


def _words(text: str) -> list[str]:
    return [tok for tok in re.split(r"\s+", (text or "").strip()) if tok]


def latin_runs_isolated(text: str) -> list[str]:
    """Return Latin runs in *text* that are not inside U+2066…U+2069."""
    bare = text or ""
    stripped = re.sub(re.escape(LRI) + r".*?" + re.escape(PDI), " ", bare, flags=re.S)
    return [m.group(0) for m in _LATIN_RUN.finditer(stripped)]


def has_rtl(text: str) -> bool:
    return any("\u0600" <= ch <= "\u06FF" or "\u0750" <= ch <= "\u077F" for ch in (text or ""))


def _norm_text(value: str) -> str:
    return (value or "").replace(LRI, "").replace(PDI, "").strip()


def _norm_tok(token: str) -> str:
    return (token or "").casefold().rstrip(".,:;!?\"'")


def _key(item: dict[str, Any], keys: tuple[str, ...]) -> Any:
    return next((item[k] for k in keys if k in item), None)


def _as_seconds(value: Any, *, assume_ms: bool = False) -> float | None:
    try:
        number = float(value)
    except (TypeError, ValueError):
        return None
    if assume_ms:
        return number / 1000.0
    # Heuristic: absolute times ≥ 1000 with no unit are almost certainly ms.
    # Caption/ASR starts in this gate are seconds; leave large values as-is when
    # the caller already named a seconds key.
    return number


def _word_start_s(row: dict[str, Any]) -> float | None:
    if "start_ms" in row or "s_ms" in row:
        return _as_seconds(row.get("start_ms", row.get("s_ms")), assume_ms=True)
    return _as_seconds(_key(row, _START_KEYS))


def _normalize_asr_words(raw: Any) -> list[dict[str, Any]]:
    """Flatten word-level ASR/aligner rows to {word, start} seconds."""
    found: list[dict[str, Any]] = []

    def walk(node: Any) -> None:
        if isinstance(node, list):
            for value in node:
                walk(value)
            return
        if not isinstance(node, dict):
            return
        word = _key(node, _WORD_KEYS)
        if isinstance(word, str) and word.strip():
            start = _word_start_s(node)
            if start is None:
                return
            found.append({"word": word.strip(), "start": float(start)})
            return
        for value in node.values():
            walk(value)

    walk(raw)
    found.sort(key=lambda row: row["start"])
    return found


_CUE_ALIGNED = frozenset({
    "cue-aligned", "cue_aligned", "from-cues", "from_cues", "builder", "builder-asr",
})

_SELF_CLOCK_METHODS = frozenset({
    "mid_speech_voiced", "self_clock", "self-clock", "cue_start", "cue-start",
    "onset_eq_cue", "cue_as_onset",
})


def _independent_asr_word_match(row: dict[str, Any], obs: dict[str, Any] | None = None) -> bool:
    """True when onset is backed by an ASR word time, not the cue alone.

    Accept explicit flags or an ``asr_words`` / obs-level word whose ``start``
    is within tolerance of the row onset AND whose text matches the cue/caption.
    """
    if row.get("asr_word_matched") is True or row.get("independent_asr_match") is True:
        return True
    if obs and (obs.get("asr_word_matched") is True or obs.get("independent_asr_match") is True):
        return True
    onset_raw = row.get("phrase_onset", row.get("onset", row.get("onset_s")))
    try:
        onset = float(onset_raw)
    except (TypeError, ValueError):
        return False
    words = row.get("asr_words") or row.get("words")
    if not isinstance(words, list) and obs is not None:
        words = obs.get("asr_words") or obs.get("words")
    if not isinstance(words, list):
        return False
    caption = str(
        row.get("caption") or row.get("word") or row.get("text") or row.get("line") or ""
    ).strip().casefold()
    for word in words:
        if not isinstance(word, dict):
            continue
        try:
            start = float(word.get("start", word.get("begin", word.get("t"))))
        except (TypeError, ValueError):
            continue
        if abs(start - onset) > 1e-4:
            continue
        token = str(word.get("word") or word.get("text") or "").strip().casefold()
        if not caption or (token and (token in caption or caption in token)):
            return True
    return False


def self_clock_hits(obs: dict[str, Any], captions: list[Any] | None = None) -> list[str]:
    """L81: reject onset rows whose onset equals the cue without independent ASR proof.

    Detection is by **value** (onset ≈ cue_s), not by method-name allowlist.
    Named self-clock methods remain an extra signal when values are unreadable.
    """
    del captions  # reserved for future caption cross-check
    hits: list[str] = []
    rows = obs.get("onset_rows") or obs.get("gate_rows") or obs.get("cue_onsets")
    if isinstance(rows, list):
        for index, row in enumerate(rows):
            if not isinstance(row, dict):
                continue
            method = str(row.get("onset_method") or row.get("method") or "").strip().casefold()
            try:
                cue_s = float(row.get("cue_s", row.get("cue_start", row.get("start"))))
                onset = float(row.get("phrase_onset", row.get("onset", row.get("onset_s"))))
            except (TypeError, ValueError):
                if method in _SELF_CLOCK_METHODS:
                    hits.append(f"onset_rows[{index}]:self-clock-unreadable")
                continue
            if abs(cue_s - onset) <= 1e-4:
                if not _independent_asr_word_match(row, obs):
                    label = method or "unnamed"
                    hits.append(f"onset_rows[{index}]:self-clock:{label}")
    # Builder flag that admits cue-derived onsets
    if obs.get("onset_from_cue") is True or obs.get("self_clock") is True:
        hits.append("onset-from-cue-flag")
    method = str(obs.get("onset_method") or "").strip().casefold()
    if obs.get("phrase_onset") is not None and obs.get("cue_s", obs.get("cue_start")) is not None:
        try:
            if abs(float(obs["phrase_onset"]) - float(obs.get("cue_s", obs.get("cue_start")))) <= 1e-4:
                rowish = {
                    "phrase_onset": obs.get("phrase_onset"),
                    "asr_word_matched": obs.get("asr_word_matched"),
                    "independent_asr_match": obs.get("independent_asr_match"),
                    "asr_words": obs.get("asr_words"),
                    "caption": obs.get("caption") or obs.get("word"),
                }
                if not _independent_asr_word_match(rowish, obs):
                    label = method or "unnamed"
                    hits.append(f"obs-self-clock:{label}")
        except (TypeError, ValueError):
            if method in _SELF_CLOCK_METHODS:
                hits.append(f"obs-self-clock-unreadable:{method}")
    return hits



def asr_provenance_hits(obs: dict[str, Any]) -> list[str]:
    """L65-fu1 / L73: require asr_path + hash/tool; reject cue-aligned sources."""
    hits: list[str] = []
    src = str(obs.get("asr_source") or obs.get("asr_origin") or "").strip().casefold()
    if src in _CUE_ALIGNED:
        hits.append("asr-source-cue-aligned")
    path_value = obs.get("asr_path") or obs.get("aligner_path")
    if not path_value:
        hits.append("missing-asr-path")
    else:
        path = Path(str(path_value))
        if not path.is_file():
            hits.append("asr-path-missing-file")
    claimed = str(obs.get("asr_sha256") or obs.get("asr_hash") or "").strip().casefold()
    if not claimed:
        hits.append("missing-asr-sha256")
    elif path_value:
        path = Path(str(path_value))
        if path.is_file():
            try:
                digest = hashlib.sha256(path.read_bytes()).hexdigest()
            except OSError:
                hits.append("asr-sha256-unreadable")
            else:
                if claimed != digest.casefold():
                    hits.append("asr-sha256-mismatch")
    if not str(obs.get("asr_tool") or obs.get("aligner_tool") or "").strip():
        hits.append("missing-asr-tool")
    # Inline builder words without a path are rejected (path is SoT).
    if any(k in obs for k in _ASR_KEYS) and not path_value:
        hits.append("inline-asr-words-without-path")
    return hits


def extract_asr_words(obs: dict[str, Any]) -> list[dict[str, Any]] | None:
    """
    Return ASR/aligner words from ``asr_path`` (preferred) or None when absent.

    Inline ``asr_words`` alone are insufficient (L65-fu1). Builder onset fields
    alone are not ASR.
    """
    path_value = obs.get("asr_path") or obs.get("aligner_path")
    if path_value:
        path = Path(str(path_value))
        if not path.is_file():
            return []
        try:
            data = json.loads(path.read_text(encoding="utf-8"))
        except (OSError, json.JSONDecodeError):
            return []
        return _normalize_asr_words(data)
    return None


def _cue_start_s(cue: dict[str, Any]) -> float | None:
    if "start_ms" in cue or "t_start_ms" in cue:
        return _as_seconds(cue.get("start_ms", cue.get("t_start_ms")), assume_ms=True)
    return _as_seconds(_key(cue, ("start_s", "start", "s", "t_start")))


def _cue_text_tokens(cue: dict[str, Any]) -> list[str]:
    lines = cue.get("lines")
    if lines is None and cue.get("text"):
        lines = str(cue.get("text")).splitlines() or [str(cue.get("text"))]
    if not isinstance(lines, list):
        return []
    return [_norm_tok(tok) for line in lines for tok in _words(str(line)) if _norm_tok(tok)]


def matched_word_start_s(
    cue: dict[str, Any],
    asr_words: list[dict[str, Any]],
    *,
    consumed: set[int] | None = None,
) -> float | None:
    """
    Pick ASR word start for *cue* by sequential token match (no nearest fallback).

    L65-fu2: unmatched cue text → None (FAIL upstream).
    L65-fu3: duplicate tokens consume ASR hits in order via *consumed* indices.
    """
    if not asr_words:
        return None
    tokens = _cue_text_tokens(cue)
    if not tokens:
        return None
    target = tokens[0]
    used = consumed if consumed is not None else set()
    for index, row in enumerate(asr_words):
        if index in used:
            continue
        if _norm_tok(row["word"]) == target:
            used.add(index)
            return float(row["start"])
    return None



def _cue_asr_starts_correlated(
    captions: list[Any],
    asr_words: list[dict[str, Any]],
    *,
    eps_s: float = 1e-3,
) -> bool:
    """True when most cue starts equal an ASR word start (cue-derived timings)."""
    if not captions or not asr_words:
        return False
    asr_starts: list[float] = []
    for word in asr_words:
        try:
            asr_starts.append(float(word.get("start", word.get("start_s", 0))))
        except (TypeError, ValueError):
            continue
    if not asr_starts:
        return False
    matched = 0
    considered = 0
    for cue in captions:
        if not isinstance(cue, dict):
            continue
        try:
            cue_start = float(cue.get("start", cue.get("start_s", 0)))
        except (TypeError, ValueError):
            continue
        considered += 1
        if any(abs(cue_start - s) <= eps_s for s in asr_starts):
            matched += 1
    return considered > 0 and matched == considered


def measured_onset_ms(
    cue: dict[str, Any],
    asr_words: list[dict[str, Any]],
    *,
    consumed: set[int] | None = None,
) -> float | None:
    """|cue_start − matched ASR word start| in ms; None when not measurable."""
    cue_start = _cue_start_s(cue)
    word_start = matched_word_start_s(cue, asr_words, consumed=consumed)
    if cue_start is None or word_start is None:
        return None
    return abs(float(cue_start) - float(word_start)) * 1000.0


def _check_share(obs: dict[str, Any], hits: list[str]) -> None:
    shared = obs.get("shared_with_owner") is True or obs.get("deliverable_shared") is True
    if obs.get("wip_sample_shared") is True:
        hits.append("wip-sample-shared")
    if not shared:
        return
    if obs.get("reviewed") is not True:
        hits.append("shared-without-review")
    if obs.get("owner_approved") is not True:
        hits.append("shared-without-owner-approval")
    if obs.get("coach_acceptance_applicable") is True and obs.get("coach_accepted") is not True:
        hits.append("shared-without-coach-acceptance")


def _check_ocr(frames: Any, hits: list[str]) -> None:
    if frames is None:
        hits.append("missing-ocr_frames")
        return
    if not isinstance(frames, list) or not frames:
        hits.append("empty-ocr_frames")
        return
    for index, frame in enumerate(frames):
        if not isinstance(frame, dict):
            hits.append(f"ocr_frames[{index}]:bad-row")
            continue
        ocr = str(frame.get("text") or frame.get("ocr") or "")
        expected = str(frame.get("expected") or frame.get("transcript") or "")
        if expected and _norm_text(ocr) != _norm_text(expected) and ocr.strip() != expected.strip():
            hits.append(f"ocr_frames[{index}]:mismatch")
        if has_rtl(ocr):
            for run in latin_runs_isolated(ocr):
                hits.append(f"ocr_frames[{index}]:bidi-unisolated:{run}")


def _check_static(obs: dict[str, Any], hits: list[str]) -> None:
    kind = str(obs.get("kind") or obs.get("deliverable_kind") or "").casefold()
    if kind not in {"kinetic", "motion-graphics", "mg", "motion"}:
        return
    blocks = obs.get("static_text_blocks")
    if blocks is None:
        hits.append("missing-static_text_blocks")
        return
    if not isinstance(blocks, list):
        return
    for index, block in enumerate(blocks):
        if not isinstance(block, dict):
            continue
        text = str(block.get("text") or "")
        dur = float(block.get("duration_s") or block.get("dur_s") or 0)
        word_count = int(block.get("words") or len(_words(text)))
        if dur > MAX_STATIC_SECONDS + 1e-9:
            hits.append(f"static_text_blocks[{index}]:duration>{MAX_STATIC_SECONDS:g}s")
        if word_count > MAX_STATIC_WORDS:
            hits.append(f"static_text_blocks[{index}]:words>{MAX_STATIC_WORDS}")


def _phrase_split_ok(cue: dict[str, Any], lines: list[Any]) -> bool:
    phrase_ok = cue.get("phrase_split")
    if phrase_ok is False:
        return False
    if phrase_ok is True or len(lines) != 2:
        return True
    left, right = str(lines[0]).rstrip(), str(lines[1]).lstrip()
    if left.endswith("-") or (left.split() and len(left.split()[-1]) == 1):
        return False
    if right.split() and len(right.split()[0]) == 1:
        return False
    return True


def _check_caption_lines(cue: dict[str, Any], index: int, hits: list[str]) -> list[Any] | None:
    lines = cue.get("lines")
    if lines is None and cue.get("text"):
        lines = str(cue.get("text")).splitlines() or [str(cue.get("text"))]
    if not isinstance(lines, list) or not lines:
        hits.append(f"captions[{index}]:missing-lines")
        return None
    if len(lines) > MAX_CAPTION_LINES:
        hits.append(f"captions[{index}]:lines>{MAX_CAPTION_LINES}")
    for li, line in enumerate(lines):
        if len(str(line)) > MAX_CAPTION_LINE_CHARS:
            hits.append(f"captions[{index}]:line[{li}]>{MAX_CAPTION_LINE_CHARS}chars")
    if not _phrase_split_ok(cue, lines):
        hits.append(f"captions[{index}]:mid-phrase-split")
    return lines


def _check_caption_onset(
    cue: dict[str, Any],
    index: int,
    asr_words: list[dict[str, Any]],
    hits: list[str],
    *,
    consumed: set[int],
    measured: list[float],
) -> None:
    """Measure onset vs ASR; ignore builder onset_ms* fields for pass/fail."""
    if _cue_start_s(cue) is None:
        hits.append(f"captions[{index}]:missing-start")
        return
    delta = measured_onset_ms(cue, asr_words, consumed=consumed)
    if delta is None:
        hits.append(f"captions[{index}]:unmatched-asr-word")
        return
    measured.append(float(delta))
    if delta > MAX_ONSET_MS + 1e-9:
        hits.append(f"captions[{index}]:onset>{MAX_ONSET_MS}ms (measured={delta:.1f}ms)")


def _check_captions(
    captions: Any,
    asr_words: list[dict[str, Any]] | None,
    hits: list[str],
    *,
    obs: dict[str, Any] | None = None,
) -> None:
    if asr_words is None:
        hits.append("missing-asr-words")
    elif not asr_words:
        hits.append("empty-asr-words")
    if captions is None:
        hits.append("missing-captions")
        return
    if not isinstance(captions, list) or not captions:
        hits.append("empty-captions")
        return
    words = asr_words or []
    consumed: set[int] = set()
    measured: list[float] = []
    for index, cue in enumerate(captions):
        if not isinstance(cue, dict):
            hits.append(f"captions[{index}]:bad-row")
            continue
        _check_caption_lines(cue, index, hits)
        if asr_words is None or not asr_words:
            continue
        _check_caption_onset(
            cue, index, words, hits, consumed=consumed, measured=measured,
        )
    # L73: perfect 0ms / cue-synced ASR is not independent — ignore builder
    # ``asr_independent_proof`` boolean; derive from measured onsets + correlation.
    if measured and max(measured) <= 1e-9:
        hits.append("suspect-perfect-zero-onset-cue-correlated")
    elif measured and _cue_asr_starts_correlated(captions, words):
        hits.append("asr-timings-cue-correlated")


def _check_diagrams(diagrams: Any, hits: list[str]) -> None:
    if diagrams is None:
        hits.append("missing-diagrams")
        return
    if not isinstance(diagrams, list):
        return
    for index, diagram in enumerate(diagrams):
        if not isinstance(diagram, dict):
            continue
        anim = str(diagram.get("animation") or diagram.get("motion") or "").casefold()
        if not anim:
            hits.append(f"diagrams[{index}]:missing-animation")
        elif anim in _POP_FADE or anim not in _DRAW_ON:
            hits.append(f"diagrams[{index}]:animation={anim} (need draw-on)")


def evaluate(obs: dict[str, Any] | None) -> dict[str, Any]:
    """Fail-closed evaluation of one MG observations receipt."""
    if obs is None or not isinstance(obs, dict):
        return {
            "ok": False,
            "hits": ["missing-input"],
            "error": "observations JSON object required (fail-closed)",
        }
    hits: list[str] = []
    if obs.get("transcript_manually_verified") is not True:
        hits.append("transcript-not-manually-verified")
    _check_share(obs, hits)
    if obs.get("reviewer_brief_lists_mg_checks") is not True:
        hits.append("reviewer-brief-missing-mg-checks")
    hits.extend(asr_provenance_hits(obs))
    hits.extend(self_clock_hits(obs, obs.get("captions") if isinstance(obs.get("captions"), list) else None))
    _check_ocr(obs.get("ocr_frames"), hits)
    _check_static(obs, hits)
    _check_captions(obs.get("captions"), extract_asr_words(obs), hits, obs=obs)
    _check_diagrams(obs.get("diagrams"), hits)
    return {
        "ok": not hits,
        "hits": hits,
        "error": None if not hits else "motion-graphics gate failed",
        "max_caption_line_chars": MAX_CAPTION_LINE_CHARS,
        "max_onset_ms": MAX_ONSET_MS,
        "asr_path": obs.get("asr_path") or obs.get("aligner_path"),
        "asr_tool": obs.get("asr_tool") or obs.get("aligner_tool"),
        "asr_sha256": obs.get("asr_sha256") or obs.get("asr_hash"),
    }


def load_observations(path: Path) -> dict[str, Any] | None:
    if not path.is_file():
        return None
    try:
        data = json.loads(path.read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError):
        return None
    if isinstance(data, dict):
        if isinstance(data.get("observations"), dict):
            return data["observations"]
        return data
    return None
