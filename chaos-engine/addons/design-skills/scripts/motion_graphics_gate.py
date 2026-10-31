"""L65: motion-graphics acceptance gate (fail-closed on observations JSON).

Checks (Carl field miss after r1 MG sample passed indep review):
(a) OCR rendered frames vs verbatim transcript; catch AR/EN bidi garble
    (Latin runs isolated with U+2066 FIRST-STRONG ISOLATE / U+2069 PDI).
(b) Kinetic deliverables: FAIL any static text block >3s OR >6 words.
(c) Caption onset <=150ms vs speech.
(d) Transcript manually verified against audio (flag required).
(e) Diagrams animate draw-on, not pop/fade.
Plus: caption lines <=32 chars, 1-2 lines split at phrase boundaries;
share only reviewed + owner-approved deliverables (Coach when applicable);
no WIP sample shares. Missing observations / required keys → FAIL.
"""
from __future__ import annotations

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


def _check_captions(captions: Any, hits: list[str]) -> None:
    if captions is None:
        hits.append("missing-captions")
        return
    if not isinstance(captions, list) or not captions:
        hits.append("empty-captions")
        return
    for index, cue in enumerate(captions):
        if not isinstance(cue, dict):
            hits.append(f"captions[{index}]:bad-row")
            continue
        onset = cue.get("onset_ms_vs_speech", cue.get("onset_ms"))
        if onset is None:
            hits.append(f"captions[{index}]:missing-onset")
        else:
            try:
                onset_f = float(onset)
            except (TypeError, ValueError):
                hits.append(f"captions[{index}]:bad-onset")
                onset_f = None
            if onset_f is not None and abs(onset_f) > MAX_ONSET_MS + 1e-9:
                hits.append(f"captions[{index}]:onset>{MAX_ONSET_MS}ms")
        lines = cue.get("lines")
        if lines is None and cue.get("text"):
            lines = str(cue.get("text")).splitlines() or [str(cue.get("text"))]
        if not isinstance(lines, list) or not lines:
            hits.append(f"captions[{index}]:missing-lines")
            continue
        if len(lines) > MAX_CAPTION_LINES:
            hits.append(f"captions[{index}]:lines>{MAX_CAPTION_LINES}")
        for li, line in enumerate(lines):
            if len(str(line)) > MAX_CAPTION_LINE_CHARS:
                hits.append(f"captions[{index}]:line[{li}]>{MAX_CAPTION_LINE_CHARS}chars")
        if not _phrase_split_ok(cue, lines):
            hits.append(f"captions[{index}]:mid-phrase-split")


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
    _check_ocr(obs.get("ocr_frames"), hits)
    _check_static(obs, hits)
    _check_captions(obs.get("captions"), hits)
    _check_diagrams(obs.get("diagrams"), hits)
    return {
        "ok": not hits,
        "hits": hits,
        "error": None if not hits else "motion-graphics gate failed",
        "max_caption_line_chars": MAX_CAPTION_LINE_CHARS,
        "max_onset_ms": MAX_ONSET_MS,
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
