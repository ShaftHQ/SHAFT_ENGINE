"""L64: uncaptioned speech gate — ASR speech span coverage by human cues.

Port of the designer-workspace coverage helper, project-neutral and fail-closed.
ASR word timestamps (seconds) merge into speech spans; any span >= MIN_SPEECH_S
(300 ms) that is less than MIN_COVERED_RATIO covered by human (non-hall) cues
→ FAIL. Missing cues/asr → FAIL.
"""
from __future__ import annotations

import json
import re
from pathlib import Path
from typing import Any

MIN_SPEECH_S = 0.30  # 300 ms
MIN_COVERED_RATIO = 0.50
DEFAULT_MERGE_GAP_S = 0.35

# Generic non-speech / hall filter tokens (project-neutral; no client names).
_HALL_EXACT = frozenset({
    "[music]", "(music)", "[applause]", "(applause)",
    "[laughter]", "(laughter)", "[silence]", "(silence)",
    "[inaudible]", "(inaudible)", "[noise]", "(noise)",
})
_HALL_RE = re.compile(
    r"(?i)\b("
    r"music|applause|laughter|silence|inaudible|noise|"
    r"subscribe|like and subscribe|thanks for watching|"
    r"copyright|all rights reserved"
    r")\b"
)


def is_hall_text(text: str) -> bool:
    """True when cue/ASR text looks like a non-speech / hall filler."""
    raw = (text or "").strip()
    if not raw:
        return True
    if raw.casefold() in _HALL_EXACT:
        return True
    if _HALL_RE.search(raw):
        return True
    if re.fullmatch(r"[\[\(][^\]\)]+[\]\)]", raw):
        return True
    return False


def _seconds(value: Any) -> float | None:
    """Coerce a timestamp to seconds. Accepts s / start / start_ms."""
    if value is None:
        return None
    if isinstance(value, (int, float)):
        number = float(value)
        # Heuristic: large integers are milliseconds.
        if number >= 1000 and abs(number - round(number)) < 1e-9:
            return number / 1000.0
        return number
    text = str(value).strip()
    if not text:
        return None
    if text.endswith("ms"):
        try:
            return float(text[:-2]) / 1000.0
        except ValueError:
            return None
    if ":" in text:
        parts = text.split(":")
        try:
            nums = [float(p) for p in parts]
        except ValueError:
            return None
        if len(nums) == 3:
            return nums[0] * 3600 + nums[1] * 60 + nums[2]
        if len(nums) == 2:
            return nums[0] * 60 + nums[1]
    try:
        return float(text)
    except ValueError:
        return None


def _span_seconds(item: dict) -> tuple[float, float] | None:
    start = item.get("s", item.get("start", item.get("start_ms", item.get("begin"))))
    end = item.get("e", item.get("end", item.get("end_ms", item.get("finish"))))
    # Prefer explicit *_ms keys when both present without s/e.
    if "start_ms" in item and "s" not in item and "start" not in item:
        start = item.get("start_ms")
        if isinstance(start, (int, float)):
            start = float(start) / 1000.0
    if "end_ms" in item and "e" not in item and "end" not in item:
        end = item.get("end_ms")
        if isinstance(end, (int, float)):
            end = float(end) / 1000.0
    a = _seconds(start)
    if a is None:
        return None
    b = _seconds(end)
    if b is None:
        b = a + 0.12
    if b < a:
        a, b = b, a
    return a, b


def _overlap(a0: float, a1: float, b0: float, b1: float) -> float:
    return max(0.0, min(a1, b1) - max(a0, b0))


def load_asr_words(items: list[dict]) -> list[dict]:
    """Normalize ASR word dicts to {s,e,t} in seconds; drop halls."""
    out: list[dict] = []
    for item in items:
        if not isinstance(item, dict):
            continue
        span = _span_seconds(item)
        if span is None:
            continue
        text = str(
            item.get("t") or item.get("w") or item.get("word") or item.get("text") or ""
        ).strip()
        if not text or is_hall_text(text):
            continue
        out.append({"s": span[0], "e": span[1], "t": text})
    out.sort(key=lambda row: row["s"])
    return out


def merge_asr_spans(
    words: list[dict], *, merge_gap: float = DEFAULT_MERGE_GAP_S,
) -> list[dict]:
    """Merge consecutive ASR words into speech spans when gap ≤ merge_gap."""
    if not words:
        return []
    spans: list[dict] = []
    cur_s = words[0]["s"]
    cur_e = words[0]["e"]
    cur_toks = [words[0]["t"]]
    for word in words[1:]:
        gap = word["s"] - cur_e
        if gap <= merge_gap + 1e-9:
            cur_e = max(cur_e, word["e"])
            cur_toks.append(word["t"])
        else:
            span_text = " ".join(cur_toks)
            if not is_hall_text(span_text):
                spans.append({
                    "s": cur_s, "e": cur_e, "dur": cur_e - cur_s, "text": span_text,
                })
            cur_s, cur_e, cur_toks = word["s"], word["e"], [word["t"]]
    span_text = " ".join(cur_toks)
    if not is_hall_text(span_text):
        spans.append({"s": cur_s, "e": cur_e, "dur": cur_e - cur_s, "text": span_text})
    return spans


def human_cues(cues: list[dict]) -> list[dict]:
    """Return speech cues that count toward coverage.

    Coverage requires JSON ``human: true`` (see ``human_cue.is_human_json_cue``).
    ASS Style/Name ``Human`` is L63-only and does **not** count here.
    """
    # Local import keeps the helper optional for consumers that only ship this file.
    try:
        from human_cue import is_human_json_cue  # type: ignore
    except ImportError:  # pragma: no cover - same-dir load via craft_qc
        import importlib.util
        helper = Path(__file__).resolve().parent / "human_cue.py"
        spec = importlib.util.spec_from_file_location("ce_human_cue", helper)
        mod = importlib.util.module_from_spec(spec)
        assert spec.loader is not None
        spec.loader.exec_module(mod)
        is_human_json_cue = mod.is_human_json_cue

    out: list[dict] = []
    for cue in cues:
        if not isinstance(cue, dict):
            continue
        text = str(cue.get("t") or cue.get("text") or cue.get("content") or "")
        if is_hall_text(text):
            continue
        # L64: JSON human: true only (non-hall text without the flag does not count).
        if not is_human_json_cue(cue):
            continue
        span = _span_seconds(cue)
        if span is None:
            continue
        out.append({"s": span[0], "e": span[1], "t": text[:80]})
    return out


def evaluate(
    cues: list[dict] | None,
    asr: list[dict] | None,
    *,
    min_speech_s: float = MIN_SPEECH_S,
    min_covered_ratio: float = MIN_COVERED_RATIO,
    merge_gap: float = DEFAULT_MERGE_GAP_S,
) -> dict[str, Any]:
    """
    Fail-closed coverage gate.

    Missing cues or asr → FAIL. Each merged ASR speech span >= min_speech_s must
    be covered >= min_covered_ratio by human (non-hall) cues.
    """
    if cues is None or asr is None:
        return {
            "ok": False,
            "hits": ["missing-input"],
            "error": "cues and asr are required (fail-closed)",
            "uncovered": [],
        }
    if not isinstance(cues, list) or not isinstance(asr, list):
        return {
            "ok": False,
            "hits": ["bad-input"],
            "error": "cues and asr must be lists",
            "uncovered": [],
        }
    humans = human_cues(cues)
    words = load_asr_words(asr)
    spans = merge_asr_spans(words, merge_gap=merge_gap)
    uncovered: list[dict[str, Any]] = []
    checked = 0
    for span in spans:
        if span["dur"] + 1e-9 < min_speech_s:
            continue
        checked += 1
        overlap = sum(_overlap(span["s"], span["e"], c["s"], c["e"]) for c in humans)
        overlap = min(overlap, span["dur"])
        ratio = overlap / span["dur"] if span["dur"] > 0 else 1.0
        if ratio + 1e-9 < min_covered_ratio:
            uncovered.append({
                "start_s": round(span["s"], 3),
                "end_s": round(span["e"], 3),
                "dur_s": round(span["dur"], 3),
                "covered_ratio": round(ratio, 4),
                "text": span["text"][:120],
            })
    ok = not uncovered
    return {
        "ok": ok,
        "hits": [
            f"uncaptioned:{u['start_s']}-{u['end_s']}" for u in uncovered
        ] if uncovered else [],
        "uncovered": uncovered,
        "speech_spans": checked,
        "human_cues": len(humans),
        "min_speech_s": min_speech_s,
        "min_covered_ratio": min_covered_ratio,
    }


def load_json_list(path: Path) -> list[dict] | None:
    if not path.is_file():
        return None
    try:
        data = json.loads(path.read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError):
        return None
    if isinstance(data, dict):
        for key in ("cues", "words", "asr_words", "segments", "items", "asr"):
            if isinstance(data.get(key), list):
                return data[key]
        return None
    if isinstance(data, list):
        return data
    return None


# Back-compat aliases used by craft_qc / tests.
MIN_SPEECH_MS = int(MIN_SPEECH_S * 1000)
