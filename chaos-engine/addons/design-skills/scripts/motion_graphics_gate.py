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


def _words(text: str) -> list[str]:
    return [tok for tok in re.split(r"\s+", (text or "").strip()) if tok]


def latin_runs_isolated(text: str) -> list[str]:
    """Return Latin runs in *text* that are not inside U+2066…U+2069."""
    bare = text or ""
    # Strip already-isolated spans, then hunt leftover Latin.
    stripped = re.sub(re.escape(LRI) + r".*?" + re.escape(PDI), " ", bare, flags=re.S)
    return [m.group(0) for m in _LATIN_RUN.finditer(stripped)]


def has_rtl(text: str) -> bool:
    return any("\u0600" <= ch <= "\u06FF" or "\u0750" <= ch <= "\u077F" for ch in (text or ""))


def evaluate(obs: dict[str, Any] | None) -> dict[str, Any]:
    """Fail-closed evaluation of one MG observations receipt."""
    if obs is None or not isinstance(obs, dict):
        return {
            "ok": False,
            "hits": ["missing-input"],
            "error": "observations JSON object required (fail-closed)",
        }
    hits: list[str] = []

    # (d) transcript manually verified — required bool True
    if obs.get("transcript_manually_verified") is not True:
        hits.append("transcript-not-manually-verified")

    # Share only reviewed + owner-approved deliverables (never WIP samples).
    shared = obs.get("shared_with_owner") is True or obs.get("deliverable_shared") is True
    wip_shared = obs.get("wip_sample_shared") is True
    if wip_shared:
        hits.append("wip-sample-shared")
    if shared:
        if obs.get("reviewed") is not True:
            hits.append("shared-without-review")
        if obs.get("owner_approved") is not True:
            hits.append("shared-without-owner-approval")
        if obs.get("coach_acceptance_applicable") is True and obs.get("coach_accepted") is not True:
            hits.append("shared-without-coach-acceptance")

    # Reviewer brief must list MG checks
    if obs.get("reviewer_brief_lists_mg_checks") is not True:
        hits.append("reviewer-brief-missing-mg-checks")

    # (a) OCR vs transcript + bidi
    frames = obs.get("ocr_frames")
    if frames is None:
        hits.append("missing-ocr_frames")
    elif not isinstance(frames, list) or not frames:
        hits.append("empty-ocr_frames")
    else:
        for index, frame in enumerate(frames):
            if not isinstance(frame, dict):
                hits.append(f"ocr_frames[{index}]:bad-row")
                continue
            ocr = str(frame.get("text") or frame.get("ocr") or "")
            expected = str(frame.get("expected") or frame.get("transcript") or "")
            if expected and ocr.replace(LRI, "").replace(PDI, "").strip() != expected.replace(LRI, "").replace(PDI, "").strip():
                # Allow isolate markers to differ only if normalized equal; else FAIL
                if ocr.strip() != expected.strip():
                    hits.append(f"ocr_frames[{index}]:mismatch")
            # Mixed AR/EN: Latin runs must be isolated
            if has_rtl(ocr) and latin_runs_isolated(ocr):
                for run in latin_runs_isolated(ocr):
                    hits.append(f"ocr_frames[{index}]:bidi-unisolated:{run}")

    # (b) kinetic static text
    kind = str(obs.get("kind") or obs.get("deliverable_kind") or "").casefold()
    kinetic = kind in {"kinetic", "motion-graphics", "mg", "motion"}
    blocks = obs.get("static_text_blocks")
    if kinetic:
        if blocks is None:
            hits.append("missing-static_text_blocks")
        elif isinstance(blocks, list):
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

    # (c) caption onset + line shape
    captions = obs.get("captions")
    if captions is None:
        hits.append("missing-captions")
    elif not isinstance(captions, list) or not captions:
        hits.append("empty-captions")
    else:
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
                    hits.append(
                        f"captions[{index}]:line[{li}]>{MAX_CAPTION_LINE_CHARS}chars"
                    )
            # Phrase-boundary split: when 2 lines, must not break mid-token
            if len(lines) == 2:
                phrase_ok = cue.get("phrase_split")
                if phrase_ok is False:
                    hits.append(f"captions[{index}]:mid-phrase-split")
                elif phrase_ok is not True:
                    # Heuristic: line1 should not end with hyphenated broken word
                    # and neither line should start/end with a single letter fragment
                    left, right = str(lines[0]).rstrip(), str(lines[1]).lstrip()
                    if left.endswith("-") or (left.split() and len(left.split()[-1]) == 1):
                        hits.append(f"captions[{index}]:mid-phrase-split")
                    if right.split() and len(right.split()[0]) == 1:
                        hits.append(f"captions[{index}]:mid-phrase-split")

    # (e) diagrams draw-on
    diagrams = obs.get("diagrams")
    if diagrams is None:
        hits.append("missing-diagrams")
    elif isinstance(diagrams, list):
        for index, diagram in enumerate(diagrams):
            if not isinstance(diagram, dict):
                continue
            anim = str(diagram.get("animation") or diagram.get("motion") or "").casefold()
            if anim in {"pop", "fade", "fade-in", "appear"}:
                hits.append(f"diagrams[{index}]:animation={anim} (need draw-on)")
            elif anim and anim not in {"draw-on", "draw_on", "drawon", "none", "n/a"}:
                # unknown non-draw-on → FAIL closed for kinetic safety
                if anim not in {"static"}:
                    hits.append(f"diagrams[{index}]:animation={anim} (need draw-on)")
            elif not anim:
                hits.append(f"diagrams[{index}]:missing-animation")

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
