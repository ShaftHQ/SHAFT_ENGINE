#!/usr/bin/env python3
"""Objective, zero-LLM checks for the design-skills add-on.

Every sub-command prints one JSON object and exits:
0 pass, 1 fail, 2 usage error, 4 skipped (a required tool is missing).
A skipped check is never a pass. Stdlib only; media checks call ffmpeg/ffprobe.
"""

from __future__ import annotations

import argparse
import difflib
import hashlib
import json
import math
import os
import re
import shutil
import subprocess  # nosec B404 - list-form argv for resolved media tools, never a shell.
import sys
import time
from array import array
from pathlib import Path

PASS, FAIL, USAGE, SKIPPED = 0, 1, 2, 4
STATUS = {PASS: "pass", FAIL: "fail", USAGE: "error", SKIPPED: "skipped"}


class Skip(Exception):
    """A required external tool is unavailable."""


def report(check: str, code: int, **details: object) -> int:
    print(json.dumps({"check": check, "status": STATUS[code], **details}, sort_keys=True))
    return code


def need(tool: str) -> str:
    found = shutil.which(tool)
    if not found:
        raise Skip(f"{tool} not found on PATH")
    return found


def run(command: list[str]) -> subprocess.CompletedProcess[str]:
    return subprocess.run(command, capture_output=True, text=True, check=False)  # nosec B603 - list argv, no shell.


def ffprobe(path: str) -> dict:
    result = run([need("ffprobe"), "-v", "error", "-print_format", "json", "-show_streams", "-show_format", path])
    if result.returncode != 0:
        raise ValueError(f"ffprobe failed: {result.stderr.strip()}")
    return json.loads(result.stdout)


def video_stream(probe: dict) -> dict:
    for stream in probe.get("streams", []):
        if stream.get("codec_type") == "video":
            return stream
    raise ValueError("no video stream")


def decode_raw(path: str, output_args: list[str]) -> bytes:
    """Decode a media file to raw bytes on stdout (pcm or rawvideo)."""
    command = [need("ffmpeg"), "-v", "error", "-nostdin", "-i", path, *output_args, "-"]
    result = subprocess.run(command, capture_output=True, check=False)  # nosec B603 - list argv, no shell.
    if result.returncode != 0:
        raise ValueError(f"ffmpeg decode failed: {result.stderr.decode(errors='replace').strip()[-400:]}")
    return result.stdout


def ffmpeg_filter_log(path: str, filters: str, audio: bool = False) -> str:
    flag = "-af" if audio else "-vf"
    result = run([need("ffmpeg"), "-hide_banner", "-nostats", "-i", path, flag, filters, "-f", "null", "-"])
    if result.returncode != 0:
        raise ValueError(f"ffmpeg failed: {result.stderr.strip()[-400:]}")
    return result.stderr


# ---------------------------------------------------------------- color
def hex_rgb(value: str) -> tuple[float, float, float]:
    text = value.strip().lstrip("#")
    if len(text) == 3:
        text = "".join(ch * 2 for ch in text)
    if not re.fullmatch(r"[0-9a-fA-F]{6}", text):
        raise ValueError(f"not a hex color: {value}")
    return tuple(int(text[i:i + 2], 16) / 255 for i in (0, 2, 4))  # type: ignore[return-value]


def luminance(rgb: tuple[float, float, float]) -> float:
    def channel(c: float) -> float:
        return c / 12.92 if c <= 0.04045 else ((c + 0.055) / 1.055) ** 2.4
    r, g, b = (channel(c) for c in rgb)
    return 0.2126 * r + 0.7152 * g + 0.0722 * b


def contrast_ratio(a: str, b: str) -> float:
    la, lb = sorted((luminance(hex_rgb(a)), luminance(hex_rgb(b))), reverse=True)
    return (la + 0.05) / (lb + 0.05)


def cmd_contrast(args: argparse.Namespace) -> int:
    """Pairs as fg:bg[:kind] where kind is text (4.5), large (3) or ui (3)."""
    minimum = {"text": 4.5, "large": 3.0, "ui": 3.0}
    pairs = list(args.pair)
    if args.tokens:
        data = json.loads(Path(args.tokens).read_text(encoding="utf-8"))
        for item in data.get("contrastPairs", []):
            pairs.append(f"{item['fg']}:{item['bg']}:{item.get('kind', 'text')}")
    if not pairs:
        return report("contrast", USAGE, error="give --pair fg:bg[:kind] or --tokens with contrastPairs")
    results, failed = [], False
    for pair in pairs:
        parts = pair.split(":")
        kind = parts[2] if len(parts) > 2 else "text"
        ratio = round(contrast_ratio(parts[0], parts[1]), 2)
        ok = ratio >= minimum.get(kind, 4.5)
        failed |= not ok
        results.append({"pair": pair, "ratio": ratio, "min": minimum.get(kind, 4.5), "ok": ok})
    return report("contrast", FAIL if failed else PASS, results=results)


# ---------------------------------------------------------------- captions
TIME = re.compile(r"(\d+):(\d\d):(\d\d)[,.](\d{3})")


def seconds(stamp: str) -> float:
    match = TIME.search(stamp)
    if not match:
        raise ValueError(f"bad timestamp: {stamp}")
    h, m, s, ms = (int(x) for x in match.groups())
    return h * 3600 + m * 60 + s + ms / 1000


def parse_cues(text: str) -> list[dict]:
    cues = []
    for block in re.split(r"\n\s*\n", text.replace("\r\n", "\n").strip()):
        lines = [line for line in block.split("\n") if line.strip()]
        timing = next((i for i, line in enumerate(lines) if "-->" in line), None)
        if timing is None:
            continue
        start, end = lines[timing].split("-->")
        body = [re.sub(r"<[^>]+>", "", line) for line in lines[timing + 1:]]
        cues.append({"start": seconds(start), "end": seconds(end.strip().split(" ")[0]), "lines": body})
    return cues


def cmd_captions(args: argparse.Namespace) -> int:
    cues = parse_cues(Path(args.file).read_text(encoding="utf-8"))
    if not cues:
        return report("captions", FAIL, problems=["no cues"])
    frame = 1 / args.fps
    problems = []
    for index, cue in enumerate(cues, 1):
        duration = cue["end"] - cue["start"]
        chars = sum(len(line) for line in cue["lines"])
        if len(cue["lines"]) > args.max_lines:
            problems.append(f"cue {index}: {len(cue['lines'])} lines")
        for line in cue["lines"]:
            if len(line) > args.max_chars:
                problems.append(f"cue {index}: line of {len(line)} chars")
        if duration < args.min_duration - 1e-6 or duration > args.max_duration + 1e-6:
            problems.append(f"cue {index}: duration {duration:.3f}s")
        if duration > 0 and chars / duration > args.max_cps:
            problems.append(f"cue {index}: {chars / duration:.1f} chars/s")
        if index > 1:
            gap = cue["start"] - cues[index - 2]["end"]
            if gap < -1e-6:
                problems.append(f"cue {index}: overlaps previous cue")
            elif 1e-6 < gap < 2 * frame - 1e-6:
                problems.append(f"cue {index}: gap {gap:.3f}s under two frames")
    if args.script:
        spoken = words(Path(args.script).read_text(encoding="utf-8"))
        shown = words(" ".join(" ".join(c["lines"]) for c in cues))
        coverage = 1 - word_error_rate(spoken, shown) if spoken else 1.0
        if coverage < 0.95:
            problems.append(f"script coverage {coverage:.2%}")
    return report("captions", FAIL if problems else PASS, cues=len(cues), problems=problems)


# ---------------------------------------------------------------- words
def words(text: str) -> list[str]:
    return re.findall(r"[a-z0-9']+", text.lower())


def word_error_rate(reference: list[str], hypothesis: list[str]) -> float:
    if not reference:
        return 0.0 if not hypothesis else 1.0
    previous = list(range(len(hypothesis) + 1))
    for i, ref in enumerate(reference, 1):
        current = [i] + [0] * len(hypothesis)
        for j, hyp in enumerate(hypothesis, 1):
            current[j] = min(previous[j] + 1, current[j - 1] + 1, previous[j - 1] + (ref != hyp))
        previous = current
    return previous[-1] / len(reference)


def parse_folds(items: list[str]) -> list[tuple[str, str]]:
    """`heard=meant` pairs: known ASR mishearings of the same spoken word."""
    pairs = []
    for item in items:
        heard, sep, meant = item.partition("=")
        if not sep or not heard.strip():
            raise ValueError(f"fold must be heard=meant: {item}")
        pairs.append((heard.strip().lower(), meant.strip().lower()))
    return pairs


def fold(text: str, folds: list[tuple[str, str]]) -> str:
    folded = text.lower()
    for heard, meant in folds:
        folded = re.sub(r"\b" + re.escape(heard) + r"\b", meant, folded)
    return folded


def cmd_tts(args: argparse.Namespace) -> int:
    """Gate on WER after folding known mishearings; raw WER is advisory."""
    script = Path(args.script).read_text(encoding="utf-8")
    transcript = Path(args.transcript).read_text(encoding="utf-8")
    folds = parse_folds(args.fold)
    reference = words(script)
    raw = word_error_rate(reference, words(transcript))
    wer = word_error_rate(words(fold(script, folds)), words(fold(transcript, folds)))
    problems = [] if wer <= args.max_wer else [f"WER {wer:.2%} above {args.max_wer:.0%}"]
    wpm = None
    if args.duration:
        wpm = round(len(reference) / (args.duration / 60), 1)
        if not args.min_wpm <= wpm <= args.max_wpm:
            problems.append(f"pace {wpm} wpm outside {args.min_wpm}-{args.max_wpm}")
    return report("tts", FAIL if problems else PASS, wer=round(wer, 4), raw_wer=round(raw, 4), wpm=wpm,
                  folds=len(folds), problems=problems)


# ---------------------------------------------------------------- narration text
STOP = frozenset("a an the and or to of in it is for with on so its that this".split())


def load_lines(path: str, spoken: bool = False) -> dict[str, str]:
    """Script lines as id -> text from {"lines": [...]}, a mapping, or plain text (one line per id).

    With `spoken`, a line's optional `say` (its spoken form) replaces `text`.
    """
    raw = Path(path).read_text(encoding="utf-8")
    if not path.endswith(".json"):
        return {f"L{n}": line for n, line in enumerate(raw.splitlines(), 1) if line.strip()}
    data = json.loads(raw)
    if isinstance(data, dict) and isinstance(data.get("lines"), list):
        key = "say" if spoken else "text"
        return {str(item["id"]): str(item.get(key, item["text"])) for item in data["lines"]}
    if not isinstance(data, dict):
        raise ValueError(f"{path}: expected an object")

    def text(value: object) -> str:
        if isinstance(value, dict):
            return str(value.get("transcript", value.get("text", "")))
        return str(value)
    return {str(key): text(value) for key, value in data.items()}


def compare_line(reference: list[str], hypothesis: list[str]) -> tuple[list[str], list[str]]:
    """Dropped content words, and repeats the script does not contain."""
    joined = "".join(hypothesis)

    def heard(word: str) -> bool:
        return word in hypothesis or word in joined or bool(difflib.get_close_matches(word, hypothesis, n=1, cutoff=0.8))
    dropped = [w for w in reference if w not in STOP and len(w) > 1 and not heard(w)]
    scripted = {a for a, b in zip(reference, reference[1:]) if a == b}
    repeated = [b for a, b in zip(hypothesis, hypothesis[1:]) if a == b and a not in STOP and a not in scripted]
    return dropped, repeated


def cmd_vowords(args: argparse.Namespace) -> int:
    """Per-line transcript versus the one source string of that line."""
    script, heard, folds = load_lines(args.script), load_lines(args.transcripts), parse_folds(args.fold)
    lines, failed = [], False
    for line_id, text in script.items():
        if line_id not in heard:
            lines.append({"id": line_id, "dropped": [], "repeated": [], "problem": "no transcript"})
            failed = True
            continue
        dropped, repeated = compare_line(words(fold(text, folds)), words(fold(heard[line_id], folds)))
        failed |= bool(dropped or repeated)
        lines.append({"id": line_id, "dropped": dropped, "repeated": repeated})
    return report("vowords", FAIL if failed else PASS, lines=lines)


CLI_SYNTAX = {
    "flag": r"(?<![\w-])--?[A-Za-z][\w-]*",
    "backtick": r"`",
    "variable": r"\$\{?\w",
    "path": r"\w/\w|~/",
    "pipe": r"\s\|\s",
}
VOWELS = "aeiouyɑɐɒæɔəɘɚɛɜɝɞɨɪʊʉʌʏøœɤɯᵻː"


def phoneme_repeats(text: str, ipa: str) -> list[str]:
    """Word joins where an open final syllable meets the same vowel (after an optional glide)."""
    spoken = [re.sub(r"[ˈˌ.,!?;:]", "", word) for word in ipa.split()]
    written = words(text)
    names = written if len(written) == len(spoken) else spoken
    pairs = []
    for index, (left, right) in enumerate(zip(spoken, spoken[1:])):
        end = re.search(f"[{VOWELS}]+$", left)
        start = re.match(f"[jwh]?([{VOWELS}]+)", right)
        if end and start and end.group(0) == start.group(1):
            pairs.append(f"{names[index]} {names[index + 1]}")
    return pairs


def lint_line(line_id: str, text: str, ipa: str | None, allow: set[str]) -> list[str]:
    hits = [f"{line_id}: cli {kind} in spoken text" for kind, pattern in CLI_SYNTAX.items() if re.search(pattern, text)]
    tokens = words(text)
    hits += [f"{line_id}: repeated word '{a}'" for a, b in zip(tokens, tokens[1:]) if a == b and f"{a} {b}" not in allow]
    if ipa:
        hits += [f"{line_id}: phoneme repeat '{pair}'" for pair in phoneme_repeats(text, ipa) if pair not in allow]
    return hits


def cmd_ttslint(args: argparse.Namespace) -> int:
    """Narration written for the ear: no literal CLI syntax, no merged repeats."""
    lines = load_lines(args.script, spoken=True)
    phonemes = load_lines(args.phonemes) if args.phonemes else {}
    allow = {item.lower() for item in args.allow}
    hits = [hit for line_id, text in lines.items() for hit in lint_line(line_id, text, phonemes.get(line_id), allow)]
    return report("ttslint", FAIL if hits else PASS, lines=len(lines), hits=hits)


def cmd_holds(args: argparse.Namespace) -> int:
    """On-screen text needs words/3 seconds plus 0.5 s of hold time."""
    data = json.loads(Path(args.storyboard).read_text(encoding="utf-8"))
    problems = []
    for scene in data.get("scenes", []):
        text = scene.get("onScreenText", "")
        hold = float(scene.get("textHold", scene.get("duration", 0)))
        needed = len(words(text)) / 3 + 0.5 if text else 0
        if hold + 1e-6 < needed:
            problems.append(f"{scene.get('id', '?')}: hold {hold:.2f}s < {needed:.2f}s")
    return report("holds", FAIL if problems else PASS, problems=problems)


# ---------------------------------------------------------------- brief / manifest
def cmd_brief(args: argparse.Namespace) -> int:
    data = json.loads(Path(args.storyboard).read_text(encoding="utf-8"))
    problems = []
    scenes = data.get("scenes", [])
    if not scenes:
        problems.append("no scenes")
    for scene in scenes:
        for key in ("id", "duration", "visual", "vo", "asset", "source"):
            if not scene.get(key) and scene.get(key) != 0:
                problems.append(f"{scene.get('id', '?')}: missing {key}")
        for claim in scene.get("claims", []):
            if not claim.get("evidence"):
                problems.append(f"{scene.get('id', '?')}: claim without evidence: {claim.get('text', '')[:60]}")
    target = data.get("targetDuration")
    total = sum(float(scene.get("duration", 0)) for scene in scenes)
    if target and abs(total - float(target)) > 0.05 * float(target):
        problems.append(f"total {total:.1f}s not within 5% of {target}s")
    return report("brief", FAIL if problems else PASS, scenes=len(scenes), total=total, problems=problems)


def sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        for chunk in iter(lambda: handle.read(1 << 20), b""):
            digest.update(chunk)
    return digest.hexdigest()


def cmd_manifest(args: argparse.Namespace) -> int:
    path = Path(args.manifest)
    data = json.loads(path.read_text(encoding="utf-8"))
    problems = []
    for asset in data.get("assets", []):
        name = asset.get("path", "?")
        if not asset.get("licence") and not asset.get("license"):
            problems.append(f"{name}: no licence")
        target = (path.parent / name)
        if not target.is_file():
            problems.append(f"{name}: missing file")
        elif asset.get("sha256") and asset["sha256"] != sha256(target):
            problems.append(f"{name}: sha256 mismatch")
        elif not asset.get("sha256"):
            problems.append(f"{name}: no sha256")
    return report("manifest", FAIL if problems else PASS, assets=len(data.get("assets", [])), problems=problems)


# ---------------------------------------------------------------- lint
UNSLOP = {
    "gradient-text": r"background-clip\s*:\s*text",
    "glassmorphism": r"backdrop-filter\s*:\s*blur",
    "over-rounding": r"border-radius\s*:\s*(?:[3-9]\d|\d{3,})px",
    "pure-black-surface": r"background(?:-color)?\s*:\s*(?:#000(?:000)?|black)\b",
    "pure-white-surface": r"background(?:-color)?\s*:\s*(?:#fff(?:fff)?|white)\b",
}
EASING = {
    "linear-easing": r"(?:transition|animation)[^;{}]*\blinear\b|ease\s*:\s*['\"]linear",
    "inline-cubic-bezier": r"cubic-bezier\(",
}
LINT_SUFFIXES = {".css", ".scss", ".html", ".htm", ".js", ".jsx", ".ts", ".tsx", ".vue", ".svelte", ".mdx"}


def lint(check: str, root: str, rules: dict[str, str], allow: list[str]) -> int:
    base = Path(root)
    files = [base] if base.is_file() else [p for p in base.rglob("*") if p.suffix in LINT_SUFFIXES and p.is_file()]
    hits = []
    for file in files:
        for number, line in enumerate(file.read_text(encoding="utf-8", errors="replace").splitlines(), 1):
            if "design-qc: allow" in line:
                continue
            for rule, pattern in rules.items():
                if rule not in allow and re.search(pattern, line, re.IGNORECASE):
                    hits.append(f"{file}:{number} {rule}")
    return report(check, FAIL if hits else PASS, files=len(files), hits=hits)


def cmd_unslop(args: argparse.Namespace) -> int:
    return lint("unslop", args.path, UNSLOP, args.allow)


def cmd_easing(args: argparse.Namespace) -> int:
    return lint("easing", args.path, EASING, args.allow)


# ---------------------------------------------------------------- media
def cmd_loudness(args: argparse.Namespace) -> int:
    log = ffmpeg_filter_log(args.file, "ebur128=peak=true", audio=True)
    summary = log.rsplit("Summary:", 1)[-1]

    def value(label: str) -> float:
        match = re.search(label + r":\s*(-?[\d.]+|-inf)", summary)
        if not match:
            raise ValueError(f"no {label} in ebur128 summary")
        return float(match.group(1))
    integrated, lra, peak = value(r"I"), value(r"LRA"), value(r"Peak")
    problems = []
    if abs(integrated - args.target) > args.tolerance:
        problems.append(f"integrated {integrated} LUFS not {args.target}±{args.tolerance}")
    if peak > args.max_peak:
        problems.append(f"true peak {peak} dBTP above {args.max_peak}")
    if args.max_lra is not None and lra > args.max_lra:
        problems.append(f"LRA {lra} LU above {args.max_lra}")
    return report("loudness", FAIL if problems else PASS, integrated=integrated, lra=lra, true_peak=peak, problems=problems)


def cmd_colortags(args: argparse.Namespace) -> int:
    stream = video_stream(ffprobe(args.file))
    expected = {"color_primaries": "bt709", "color_transfer": "bt709", "color_space": "bt709", "color_range": "tv"}
    problems = [f"{k}={stream.get(k)} (want {v})" for k, v in expected.items() if stream.get(k) != v]
    return report("colortags", FAIL if problems else PASS, problems=problems)


def level_violations(log: str, limit: int = 5) -> list[dict]:
    """First frames outside BT.709 legal luma 16-235, with their timestamps."""
    found = []
    for chunk in log.split("frame:")[1:]:
        stamp = re.search(r"pts_time:([\d.]+)", chunk)
        low = re.search(r"YMIN=([\d.]+)", chunk)
        high = re.search(r"YMAX=([\d.]+)", chunk)
        if not (stamp and low and high):
            continue
        ymin, ymax = float(low.group(1)), float(high.group(1))
        if ymin < 16 or ymax > 235:
            found.append({"t": float(stamp.group(1)), "ymin": ymin, "ymax": ymax})
            if len(found) >= limit:
                break
    return found


def cmd_levels(args: argparse.Namespace) -> int:
    log = ffmpeg_filter_log(args.file, "signalstats,metadata=mode=print")
    lows = [float(x) for x in re.findall(r"YMIN=([\d.]+)", log)]
    highs = [float(x) for x in re.findall(r"YMAX=([\d.]+)", log)]
    if not lows:
        raise ValueError("no signalstats output")
    ymin, ymax = min(lows), max(highs)
    problems = []
    if ymin < 16:
        problems.append(f"YMIN {ymin} below 16")
    if ymax > 235:
        problems.append(f"YMAX {ymax} above 235")
    return report("levels", FAIL if problems else PASS, ymin=ymin, ymax=ymax, problems=problems,
                  violations=level_violations(log) if problems else [])


def ratio(text: str) -> float:
    num, _, den = str(text).partition("/")
    return float(num) / float(den or 1) if float(den or 1) else 0.0


PRESETS = {
    "1080p": (1920, 1080), "2160p": (3840, 2160), "vertical": (1080, 1920),
}


def cmd_delivery(args: argparse.Namespace) -> int:
    probe = ffprobe(args.file)
    video = video_stream(probe)
    audio = [s for s in probe.get("streams", []) if s.get("codec_type") == "audio"]
    width, height = PRESETS[args.preset]
    problems = []
    checks = {
        "codec_name": "h264", "pix_fmt": "yuv420p", "width": width, "height": height,
        "color_primaries": "bt709", "color_transfer": "bt709", "color_space": "bt709",
    }
    for key, want in checks.items():
        if video.get(key) != want:
            problems.append(f"{key}={video.get(key)} (want {want})")
    if str(video.get("profile", "")).lower() not in {"high", "main"}:
        problems.append(f"profile={video.get('profile')} (want High)")
    fps = ratio(video.get("avg_frame_rate", "0/1"))
    if args.fps and abs(fps - args.fps) > 0.01:
        problems.append(f"fps {fps:.3f} (want {args.fps})")
    if not audio:
        problems.append("no audio stream")
    elif audio[0].get("codec_name") != "aac":
        problems.append(f"audio codec {audio[0].get('codec_name')} (want aac)")
    elif int(audio[0].get("sample_rate", 0)) not in {48000, 44100}:
        problems.append(f"audio sample rate {audio[0].get('sample_rate')}")
    return report("delivery", FAIL if problems else PASS, fps=round(fps, 3), problems=problems)


def cmd_blackfreeze(args: argparse.Namespace) -> int:
    log = ffmpeg_filter_log(args.file, f"blackdetect=d={args.min}:pix_th=0.10,freezedetect=n=-60dB:d={args.min}")
    blacks = re.findall(r"black_start:([\d.]+) black_end:([\d.]+)", log)
    freezes = re.findall(r"freeze_start: ([\d.]+)", log)
    allowed = [tuple(float(x) for x in span.split("-")) for span in args.allow]

    def free(start: float) -> bool:
        return not any(lo - 0.05 <= start <= hi + 0.05 for lo, hi in allowed)
    problems = [f"black at {s}s" for s, _ in blacks if free(float(s))]
    problems += [f"freeze at {s}s" for s in freezes if free(float(s))]
    return report("blackfreeze", FAIL if problems else PASS, problems=problems)


def runs(flags: list[bool]) -> list[tuple[int, int]]:
    """Half-open index ranges where flags are true."""
    found, start = [], None
    for index, flag in enumerate([*flags, False]):
        if flag and start is None:
            start = index
        elif not flag and start is not None:
            found.append((start, index))
            start = None
    return found


def static_spans(frames: list[bytes], fps: float, step: float, thresh: float, max_s: float,
                 allow: list[tuple[float, float]]) -> tuple[list[dict], float]:
    """Stretches where no grid cell changes by `thresh` levels against `step` seconds earlier."""
    lag = max(1, round(step * fps))
    still = [i >= lag and max(abs(a - b) for a, b in zip(frames[i], frames[i - lag])) < thresh
             for i in range(len(frames))]
    spans, longest = [], 0.0
    for first, end in runs(still):
        start, stop = max((first - lag) / fps, 0.0), end / fps
        longest = max(longest, stop - start)
        if stop - start > max_s and not any(lo <= start and stop <= hi for lo, hi in allow):
            spans.append({"start": round(start, 2), "end": round(stop, 2), "dur": round(stop - start, 2)})
    return spans, round(longest, 2)


def cmd_static(args: argparse.Namespace) -> int:
    """Visually static stretches: a 32x18 cell grid ignores slow pushes, catches no-content-change holds."""
    grid = "scale='if(gt(iw,ih),32,18)':'if(gt(iw,ih),18,32)':flags=area,format=gray"
    crop = f"crop={args.crop}," if args.crop else ""
    raw = decode_raw(args.file, ["-an", "-vf", f"fps={args.fps},{crop}{grid}", "-f", "rawvideo"])
    frames = [raw[i:i + 576] for i in range(0, len(raw) - 575, 576)]
    if not frames:
        raise ValueError("no decoded frames")
    allow = [tuple(float(x) for x in span.split("-")) for span in args.allow]
    spans, longest = static_spans(frames, args.fps, args.step, args.thresh, args.max, allow)  # type: ignore[arg-type]
    return report("static", FAIL if spans else PASS, spans=spans, longest_static_s=longest, max_s=args.max)


HOP_S, SPEECH_DBFS = 0.02, -45.0


def mask_overlaps(placed: list[tuple[int, list[bool]]], hop: float) -> list[dict]:
    """Time ranges where two or more placed speech masks are active."""
    counts: dict[int, int] = {}
    for start, mask in placed:
        for offset, active in enumerate(mask):
            if active:
                counts[start + offset] = counts.get(start + offset, 0) + 1
    if not counts:
        return []
    flags = [counts.get(i, 0) >= 2 for i in range(max(counts) + 1)]
    return [{"start": round(a * hop, 2), "end": round(b * hop, 2)} for a, b in runs(flags)]


def speech_mask(path: str) -> list[bool]:
    """20 ms frames whose RMS is above -45 dBFS."""
    samples = array("h")
    samples.frombytes(decode_raw(path, ["-ac", "1", "-ar", "16000", "-f", "s16le"]))
    hop = int(16000 * HOP_S)
    mask = []
    for i in range(0, len(samples) - hop + 1, hop):
        power = sum(x * x for x in samples[i:i + hop]) / hop
        mask.append(10 * math.log10(power / 32768 ** 2 + 1e-12) > SPEECH_DBFS)
    return mask


def vo_entries(edl: dict) -> list[dict]:
    audio = edl.get("audio", [])
    flagged = [item for item in audio if item.get("vo")]
    return sorted(flagged or [item for item in audio if not item.get("duck")], key=lambda item: float(item["at"]))


def entry_duration(item: dict, base: Path, edl_only: bool) -> float:
    if "dur" in item:
        return float(item["dur"])
    if edl_only:
        raise ValueError(f"{item.get('src')}: --edl-only needs dur on every VO entry")
    return float(ffprobe(str(base / item["src"]))["format"]["duration"])


def cmd_vooverlap(args: argparse.Namespace) -> int:
    """Narration lines never overlap: edit-list gap plus per-line speech masks."""
    path = Path(args.edl)
    lines = vo_entries(json.loads(path.read_text(encoding="utf-8")))
    durations = [entry_duration(item, path.parent, args.edl_only) for item in lines]
    edl_overlaps = []
    for (a, dur), b in zip(zip(lines, durations), lines[1:]):
        end, start = float(a["at"]) + dur, float(b["at"])
        if start < end + args.gap - 1e-6:
            edl_overlaps.append({"a": a["src"], "a_end": round(end, 3), "b": b["src"], "b_start": round(start, 3),
                                 "overlap_s": round(end - start, 3)})
    audio_overlaps = []
    if not args.edl_only:
        placed = [(round(float(item["at"]) / HOP_S), speech_mask(str(path.parent / item["src"]))) for item in lines]
        audio_overlaps = mask_overlaps(placed, HOP_S)
    failed = bool(edl_overlaps or audio_overlaps)
    return report("vooverlap", FAIL if failed else PASS, lines=len(lines), gap=args.gap, edl_overlaps=edl_overlaps,
                  audio_overlaps=audio_overlaps, audio_checked=not args.edl_only)


def cmd_idle(args: argparse.Namespace) -> int:
    """Run ASR and full QC only on a quiet machine: 1-minute load per CPU at or below the limit."""
    if not hasattr(os, "getloadavg"):
        raise Skip("load average is unavailable on this platform")
    deadline = time.monotonic() + args.wait
    while True:
        load = round(os.getloadavg()[0] / (os.cpu_count() or 1), 2)
        if load <= args.max_load:
            return report("idle", PASS, load_per_cpu=load, max_load=args.max_load)
        if time.monotonic() >= deadline:
            return report("idle", FAIL, load_per_cpu=load, max_load=args.max_load)
        time.sleep(min(5.0, max(deadline - time.monotonic(), 0.0)))


def cmd_silence(args: argparse.Namespace) -> int:
    log = ffmpeg_filter_log(args.file, f"silencedetect=n={args.noise}dB:d={args.max}", audio=True)
    starts = [float(x) for x in re.findall(r"silence_start: (-?[\d.]+)", log)]
    allowed = [tuple(float(x) for x in span.split("-")) for span in args.allow]
    problems = [f"silence at {s:.2f}s" for s in starts if not any(lo - 0.05 <= s <= hi for lo, hi in allowed)]
    return report("silence", FAIL if problems else PASS, problems=problems)


def cmd_ssim(args: argparse.Namespace) -> int:
    result = run([need("ffmpeg"), "-hide_banner", "-nostats", "-i", args.file, "-i", args.reference,
                  "-lavfi", "[0:v][1:v]scale2ref[a][b];[a][b]ssim", "-f", "null", "-"])
    match = re.search(r"All:([\d.]+)", result.stderr)
    if result.returncode != 0 or not match:
        raise ValueError("ssim failed")
    score = float(match.group(1))
    return report("ssim", PASS if score >= args.min else FAIL, ssim=score, min=args.min)


def has_libvmaf() -> bool:
    ffmpeg = shutil.which("ffmpeg")
    if not ffmpeg:
        return False
    return "libvmaf" in run([ffmpeg, "-hide_banner", "-filters"]).stdout


def cmd_vmaf(args: argparse.Namespace) -> int:
    if not has_libvmaf():
        raise Skip("ffmpeg has no libvmaf filter (install a build with libvmaf or the vmaf CLI)")
    result = run([need("ffmpeg"), "-hide_banner", "-nostats", "-i", args.file, "-i", args.reference,
                  "-lavfi", "[0:v][1:v]scale2ref[a][b];[a][b]libvmaf", "-f", "null", "-"])
    match = re.search(r"VMAF score:\s*([\d.]+)", result.stderr)
    if not match:
        raise ValueError("vmaf failed")
    score = float(match.group(1))
    return report("vmaf", PASS if score >= args.min else FAIL, vmaf=score, min=args.min)


def cmd_flash(args: argparse.Namespace) -> int:
    """Approximate WCAG 2.3.1: count large average-luma swings per second."""
    log = ffmpeg_filter_log(args.file, "signalstats,metadata=mode=print")
    times = [float(x) for x in re.findall(r"pts_time:([\d.]+)", log)]
    lumas = [float(x) for x in re.findall(r"YAVG=([\d.]+)", log)]
    events = []
    for i in range(1, min(len(times), len(lumas))):
        if abs(lumas[i] - lumas[i - 1]) >= args.delta:
            events.append(times[i])
    worst = 0
    for i, start in enumerate(events):
        worst = max(worst, sum(1 for t in events[i:] if t < start + 1.0) // 2)
    return report("flash", FAIL if worst > 3 else PASS, max_flashes_per_second=worst)


def cmd_noisefloor(args: argparse.Namespace) -> int:
    def floor(path: str) -> float:
        log = ffmpeg_filter_log(path, "astats=metadata=0:reset=0", audio=True)
        match = re.findall(r"Noise floor dB:\s*(-?[\d.]+|-inf)", log)
        if not match:
            raise ValueError("astats gave no noise floor")
        return float(match[-1]) if match[-1] != "-inf" else -120.0
    before, after = floor(args.before), floor(args.after)
    gain = round(before - after, 2)
    return report("noisefloor", PASS if gain >= args.min_gain else FAIL, before=before, after=after, gain_db=gain)


def cmd_image(args: argparse.Namespace) -> int:
    stream = video_stream(ffprobe(args.file))
    problems = []
    if args.size:
        width, height = (int(x) for x in args.size.lower().split("x"))
        if (stream.get("width"), stream.get("height")) != (width, height):
            problems.append(f"size {stream.get('width')}x{stream.get('height')} (want {args.size})")
    if args.max_bytes and Path(args.file).stat().st_size > args.max_bytes:
        problems.append(f"file {Path(args.file).stat().st_size} bytes above {args.max_bytes}")
    if args.file.endswith(".svg") and "<title" not in Path(args.file).read_text(encoding="utf-8"):
        problems.append("svg has no <title>")
    return report("image", FAIL if problems else PASS, problems=problems)


def cmd_doctor(args: argparse.Namespace) -> int:
    tools = ["ffmpeg", "ffprobe", "node", "npx", "vhs", "ttyd", "manim", "whisper-cli", "tesseract", "pwsh",
             "realesrgan-ncnn-vulkan", "vmaf", "deep-filter"]
    found = {tool: bool(shutil.which(tool)) for tool in tools}
    found["libvmaf"] = has_libvmaf()
    core = found["ffmpeg"] and found["ffprobe"]
    return report("doctor", PASS if core else SKIPPED, tools=found,
                  note="media checks skip without ffmpeg/ffprobe; optional tools unlock their cards")


# ---------------------------------------------------------------- round-3 picture and capture gates
def video_rate(path: str) -> float:
    num, _, den = video_stream(ffprobe(path)).get("r_frame_rate", "30/1").partition("/")
    return float(num) / float(den or 1)


def frame_std(frame: bytes) -> float:
    count = len(frame)
    mean = sum(frame) / count
    return math.sqrt(max(sum(value * value for value in frame) / count - mean * mean, 0.0))


def flat_runs(stds: list[float], fps: float, edge: float, limit: float) -> list[dict]:
    """Runs of solid frames (luma std below `limit`) outside the first and last `edge` seconds."""
    head, tail = round(edge * fps), len(stds) - round(edge * fps)
    flags = [head <= i < tail and std < limit for i, std in enumerate(stds)]
    return [{"start": round(a / fps, 2), "frames": b - a} for a, b in runs(flags)]


def cmd_flatframes(args: argparse.Namespace) -> int:
    """Solid frames at cuts: a fade-to-void dip or an empty card before its first element."""
    fps = video_rate(args.file)
    raw = decode_raw(args.file, ["-an", "-vf", "scale=64:36:flags=area,format=gray", "-f", "rawvideo"])
    stds = [frame_std(raw[i:i + 2304]) for i in range(0, len(raw) - 2303, 2304)]
    if not stds:
        raise ValueError("no decoded frames")
    found = flat_runs(stds, fps, args.edge, args.std)
    return report("flatframes", FAIL if found else PASS, runs=found, frames=len(stds), edge_s=args.edge)


def segment_seconds(segment: dict) -> float:
    if "hold" in segment:
        return float(segment["hold"])
    if "src" in segment:
        return (float(segment["src"][1]) - float(segment["src"][0])) / float(segment.get("speed", 1.0))
    return 0.0


def hold_limit(segment: dict, plain: float, stepped: float) -> float:
    return stepped if len(segment.get("marks", [])) >= 2 or segment.get("scroll") else plain


def cmd_contenthold(args: argparse.Namespace) -> int:
    """EDL holds: plain holds up to --plain s, holds with 2+ stepping marks or a scroll up to --stepped s."""
    edl = json.loads(Path(args.edl).read_text(encoding="utf-8"))
    over, count, clip_start = [], 0, 0.0
    for clip in edl.get("clips", []):
        now = 0.0
        for segment in clip.get("segments", []):
            limit = hold_limit(segment, args.plain, args.stepped)
            count += "hold" in segment
            if "hold" in segment and float(segment["hold"]) > limit + 1e-6:
                over.append({"clip": clip.get("id", "?"), "at": round(clip_start + now, 2),
                             "hold": float(segment["hold"]), "limit": limit})
            now += segment_seconds(segment)
        clip_start += float(clip.get("dur", now))
    return report("contenthold", FAIL if over else PASS, holds=count, over_limit=over)


def screen_text(claim: dict, base: Path, video: str | None) -> str:
    if "screen" in claim:
        return str(claim["screen"])
    if "screenFile" in claim:
        return (base / claim["screenFile"]).read_text(encoding="utf-8")
    if not video:
        raise ValueError(f"{claim.get('id', '?')}: needs screen, screenFile or --video")
    tesseract = need("tesseract")
    frame = Path(video).with_name(f".claim-{claim.get('id', 'x')}.png")
    try:
        run([need("ffmpeg"), "-v", "error", "-y", "-ss", str(claim["t"]), "-i", video, "-frames:v", "1", str(frame)])
        return run([tesseract, str(frame), "stdout"]).stdout
    finally:
        frame.unlink(missing_ok=True)


def check_claim(claim: dict, screen: str) -> dict:
    lines = [line.strip() for line in screen.splitlines()]

    def seen(pattern: str) -> bool:
        return any(re.search(pattern, line) for line in lines)
    missing = [p for p in claim.get("must", []) if not seen(p)]
    unexpected = [p for p in claim.get("mustNot", []) if seen(p)]
    return {"id": claim.get("id", "?"), "t": claim.get("t"), "ok": not missing and not unexpected,
            "missing": missing, "unexpected": unexpected}


def cmd_claims(args: argparse.Namespace) -> int:
    """Screen state at each narration claim word: required and forbidden patterns."""
    path = Path(args.claims)
    data = json.loads(path.read_text(encoding="utf-8"))
    rows = [check_claim(c, screen_text(c, path.parent, args.video)) for c in data.get("claims", [])]
    return report("claims", FAIL if any(not r["ok"] for r in rows) else PASS, claims=rows)


def edge_hits(rows: list, strip: int, delta: int, min_pixels: int) -> list[str]:
    """Sides whose outer `strip` columns hold more than `min_pixels` text-bright pixels."""
    width = len(rows[0])
    center = sorted(v for row in rows[::4] for v in row[width // 10: width - width // 10: 5])
    bright = center[len(center) // 2] + delta
    sides = []
    for side, cut in (("left", slice(0, strip)), ("right", slice(width - strip, width))):
        if sum(1 for row in rows for v in row[cut] if v > bright) > min_pixels:
            sides.append(side)
    return sides


def cmd_edgeclip(args: argparse.Namespace) -> int:
    """Vertical cuts: text touching the frame edge means a cropped, not reflowed, line."""
    stream = video_stream(ffprobe(args.file))
    width, height = int(stream["width"]), int(stream["height"])
    top, _, bottom = (args.region or f"0:{height}").partition(":")
    rows_high = int(bottom) - int(top)
    raw = decode_raw(args.file, ["-an", "-vf", f"fps={args.fps},crop={width}:{rows_high}:0:{top},format=gray",
                                 "-f", "rawvideo"])
    size, clipped = width * rows_high, []
    for index in range(len(raw) // size):
        frame = raw[index * size:(index + 1) * size]
        rows = [frame[y * width:(y + 1) * width] for y in range(0, rows_high, 2)]
        clipped += [{"t": round(index / args.fps, 2), "side": side}
                    for side in edge_hits(rows, args.strip, args.delta, args.min_pixels)]
    return report("edgeclip", FAIL if clipped else PASS, clipped=clipped[:20], clipped_frames=len(clipped))


CSI = re.compile(r"\x1b\[([0-9;?]*)([A-Za-z@`])|\x1b\][^\x07\x1b]*(?:\x07|\x1b\\)|\x1b[()][0-9A-Za-z]|\x1b.")


class StaleScreen:
    """Minimal terminal model: finds rows rewritten from their start with shorter text and no erase."""

    def __init__(self, width: int, height: int = 24):
        self.width, self.height = width, height
        self.rows: dict[int, list[str]] = {}
        self.x = self.y = 0
        self.visit: dict | None = None
        self.hits: list[dict] = []
        self.t = 0.0

    def line(self, y: int) -> str:
        return "".join(self.rows.get(y, [])).rstrip()

    def end(self) -> None:
        visit, self.visit = self.visit, None
        if not visit or visit["erased"]:
            return
        before = visit["before"]
        first = len(before) - len(before.lstrip())
        if not before.strip() or visit["start"] > first or visit["max"] >= len(before) - 1:
            return
        now, cut = self.line(visit["y"]), visit["max"] + 1
        if now[cut:].strip() and now[cut:] == before[cut:]:
            self.hits.append({"t": round(self.t, 3), "line": now.strip()[:90], "stale_tail": before[cut:].strip()[:40]})

    def draw(self, char: str) -> None:
        if self.x >= self.width:
            self.end()
            self.x, self.y = 0, self.y + 1
        if self.visit is None:
            self.visit = {"y": self.y, "before": self.line(self.y), "start": self.x, "max": -1, "erased": False}
        row = self.rows.setdefault(self.y, [])
        row.extend(" " * (self.x + 1 - len(row)))
        row[self.x] = char
        self.visit["max"] = max(self.visit["max"], self.x)
        self.x += 1

    def erase(self, kind: str, mode: int) -> None:
        if self.visit:
            self.visit["erased"] = True
        row = self.rows.setdefault(self.y, [])
        if kind == "K":
            spans = {0: range(self.x, len(row)), 1: range(min(self.x + 1, len(row))), 2: range(len(row))}
            for i in spans.get(mode, range(0)):
                row[i] = " "
        elif mode in (2, 3):
            self.rows.clear()
        else:
            del row[self.x:]
            for y in [y for y in self.rows if y > self.y]:
                del self.rows[y]

    def move(self, final: str, values: list[int]) -> None:
        self.end()
        first = values[0] if values and values[0] else 1
        second = values[1] if len(values) > 1 and values[1] else 1
        top = max(0, max(self.rows, default=0) - self.height + 1)
        x, y = {
            "A": (self.x, max(0, self.y - first)), "B": (self.x, self.y + first), "C": (self.x + first, self.y),
            "D": (max(0, self.x - first), self.y), "E": (0, self.y + first), "F": (0, max(0, self.y - first)),
            "G": (first - 1, self.y), "H": (second - 1, top + first - 1), "f": (second - 1, top + first - 1),
        }[final]
        self.x, self.y = x, y

    def control(self, char: str) -> None:
        if char in "\r\n":
            self.end()
            if char == "\r":
                self.x = 0
            else:
                self.y += 1
        elif char == "\b":
            self.x = max(0, self.x - 1)
        elif char == "\t":
            self.x = min(self.width - 1, (self.x // 8 + 1) * 8)
        elif char >= " ":
            self.draw(char)

    def csi(self, final: str, values: list[int]) -> None:
        if final in "KJ":
            self.erase(final, values[0] if values else 0)
        elif final in "ABCDEFGHf":
            self.move(final, values)

    def feed(self, data: str) -> None:
        position = 0
        for match in CSI.finditer(data):
            for char in data[position:match.start()]:
                self.control(char)
            position = match.end()
            if match.group(2):
                self.csi(match.group(2), [int(v) for v in match.group(1).replace("?", "").split(";") if v.isdigit()])
        for char in data[position:]:
            self.control(char)
        self.end()


def stale_rewrites(events: list[tuple[float, str]], width: int, height: int = 24) -> list[dict]:
    screen = StaleScreen(width, height)
    for t, data in events:
        screen.t = t
        screen.feed(data)
    return screen.hits


def cmd_staletext(args: argparse.Namespace) -> int:
    """Replay an asciicast v2 capture and report stale tails left by shorter rewrites without erase."""
    lines = Path(args.cast).read_text(encoding="utf-8").splitlines()
    header = json.loads(lines[0])
    events = [(float(t), d) for t, kind, d in (json.loads(line) for line in lines[1:] if line.strip()) if kind == "o"]
    hits = stale_rewrites(events, int(header.get("width", 80)), int(header.get("height", 24)))
    return report("staletext", FAIL if hits else PASS, events=len(events), hits=hits[:50], count=len(hits))


PS_PARSE = ("$e=$null;$t=$null;"
            "[void][System.Management.Automation.Language.Parser]::ParseInput($env:CMD,[ref]$t,[ref]$e);"
            "$e | ForEach-Object { $_.Message }")


def ps_commands(text: str) -> list[str]:
    blocks = re.split(r"\n\s*\n", text.replace("\r\n", "\n"))
    return [re.sub(r"(?m)^PS> ?", "", block).rstrip() for block in blocks if block.strip()]


def ps_row(command: str, pwsh: str | None) -> dict:
    errors = []
    if pwsh:
        result = subprocess.run([pwsh, "-NoProfile", "-NonInteractive", "-Command", PS_PARSE],  # nosec B603
                                capture_output=True, text=True, check=False, env={**os.environ, "CMD": command})
        errors = [line for line in result.stdout.splitlines() if line.strip()]
    return {"command": command.replace("\n", " / ")[:200], "parse_errors": errors,
            "backslash_continuation": bool(re.search(r"\\\s*$", command, re.MULTILINE))}


def cmd_psparse(args: argparse.Namespace) -> int:
    """PowerShell on screen parses with pwsh and never ends a line with a bash backslash."""
    pwsh = None if args.static else need("pwsh")
    rows = [ps_row(command, pwsh) for command in ps_commands(Path(args.file).read_text(encoding="utf-8"))]
    failed = not rows or any(r["parse_errors"] or r["backslash_continuation"] for r in rows)
    return report("psparse", FAIL if failed else PASS, commands=rows, parsed=bool(pwsh))


# ---------------------------------------------------------------- explainer arc and description
BEATS = ("hook", "problem", "solution", "proof", "cta")
BANDS = {"exec": (60.0, 120.0), "technical": (180.0, 300.0), "social": (15.0, 60.0)}


def placed_scenes(scenes: list[dict]) -> list[tuple[dict, float, float]]:
    placed, now = [], 0.0
    for scene in scenes:
        start = float(scene.get("start", now))
        now = start + float(scene.get("duration", 0))
        placed.append((scene, start, now))
    return placed


def beat_order_problems(beats: list) -> list[str]:
    problems = [f"missing beat: {beat}" for beat in BEATS if beat not in beats]
    core = [b for b in beats[:-1] if b != "cta"] + beats[-1:]
    ranks = [BEATS.index(b) for b in core if b in BEATS]
    if any(a > b for a, b in zip(ranks, ranks[1:])):
        problems.append("order: beats must run hook, problem, solution, proof, cta")
    return problems


def hook_problems(placed: list[tuple[dict, float, float]], hook_s: float) -> list[str]:
    if not placed or placed[0][0].get("beat") != "hook" or placed[0][1] > 0:
        return ["hook: the first scene must be the hook, starting at 0 s"]
    hook_end = max(end for scene, _, end in placed if scene.get("beat") == "hook")
    return [f"hook ends at {hook_end:.1f}s > {hook_s:.1f}s"] if hook_end > hook_s + 1e-6 else []


def cta_problems(placed: list[tuple[dict, float, float]], total: float) -> list[str]:
    if not placed or placed[-1][0].get("beat") != "cta":
        return ["cta: the last beat must be the call to action"]
    start = placed[-1][1]
    return [f"cta starts at {start:.1f}s, before 80% of {total:.1f}s"] if start < 0.8 * total else []


def proof_problems(scenes: list[dict]) -> list[str]:
    def evidenced(scene: dict) -> bool:
        claims = scene.get("claims", [])
        return bool(claims) and all(c.get("evidence") for c in claims)
    return [f"proof {s.get('id', '?')}: needs claims with evidence" for s in scenes
            if s.get("beat") == "proof" and not evidenced(s)]


def arc_problems(scenes: list[dict], band: tuple[float, float], audience: str, hook_s: float) -> list[str]:
    placed = placed_scenes(scenes)
    total = placed[-1][2] if placed else 0.0
    problems = beat_order_problems([scene.get("beat") for scene in scenes])
    problems += hook_problems(placed, hook_s) + cta_problems(placed, total) + proof_problems(scenes)
    if not band[0] <= total <= band[1]:
        problems.append(f"duration {total:.1f}s outside {audience} band {band[0]:.0f}-{band[1]:.0f}s")
    return problems


def cmd_arc(args: argparse.Namespace) -> int:
    """Explainer arc: hook in the first seconds, problem, solution, evidenced proof, closing CTA, length band."""
    data = json.loads(Path(args.storyboard).read_text(encoding="utf-8"))
    audience = args.audience or data.get("audience", "exec")
    low, high = BANDS.get(audience, BANDS["exec"])
    band = (args.min if args.min is not None else low, args.max if args.max is not None else high)
    problems = arc_problems(data.get("scenes", []), band, audience, args.hook)
    return report("arc", FAIL if problems else PASS, audience=audience, band=list(band), problems=problems)


def description_gap(scene: dict, minimum: float) -> dict | None:
    shown = [w for w in words(scene.get("onScreenText", "")) if w not in STOP and len(w) > 1]
    if not shown or scene.get("description"):
        return None
    spoken = set(words(scene.get("vo", "")))
    missing = [w for w in shown if w not in spoken]
    coverage = round(1 - len(missing) / len(shown), 2)
    return {"id": scene.get("id", "?"), "coverage": coverage, "missing": missing} if coverage < minimum else None


def cmd_describe(args: argparse.Namespace) -> int:
    """WCAG 2.2 1.2.5: on-screen text is spoken in the scene's narration or described."""
    data = json.loads(Path(args.storyboard).read_text(encoding="utf-8"))
    failed = [gap for scene in data.get("scenes", []) if (gap := description_gap(scene, args.min))]
    return report("describe", FAIL if failed else PASS, scenes=failed, min=args.min)


def cmd_all(args: argparse.Namespace) -> int:
    plan = json.loads(Path(args.plan).read_text(encoding="utf-8"))
    worst = PASS
    for step in plan.get("checks", []):
        code = main([step["check"], *[str(a) for a in step.get("args", [])]])
        if code == FAIL or code == USAGE:
            worst = FAIL
        elif code == SKIPPED and worst == PASS:
            worst = SKIPPED
    return report("all", worst, steps=len(plan.get("checks", [])))


def parser() -> argparse.ArgumentParser:
    root = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    sub = root.add_subparsers(dest="command", required=True)

    def add(name: str, func, help_text: str) -> argparse.ArgumentParser:
        command = sub.add_parser(name, help=help_text)
        command.set_defaults(func=func)
        return command

    p = add("contrast", cmd_contrast, "WCAG contrast of color pairs")
    p.add_argument("--pair", action="append", default=[], help="fg:bg[:text|large|ui]")
    p.add_argument("--tokens", help="theme.json with contrastPairs")
    p = add("captions", cmd_captions, "SRT/WebVTT timing and line rules")
    p.add_argument("file")
    p.add_argument("--script")
    p.add_argument("--fps", type=float, default=30.0)
    p.add_argument("--max-chars", type=int, default=42)
    p.add_argument("--max-lines", type=int, default=2)
    p.add_argument("--max-cps", type=float, default=20.0)
    p.add_argument("--min-duration", type=float, default=0.833)
    p.add_argument("--max-duration", type=float, default=7.0)
    p = add("tts", cmd_tts, "script vs transcript WER and pace")
    p.add_argument("script")
    p.add_argument("transcript")
    p.add_argument("--duration", type=float)
    p.add_argument("--max-wer", type=float, default=0.05)
    p.add_argument("--min-wpm", type=float, default=140)
    p.add_argument("--max-wpm", type=float, default=160)
    p.add_argument("--fold", action="append", default=[], help="heard=meant (known ASR mishearing)")
    p = add("vowords", cmd_vowords, "per-line transcript vs script words")
    p.add_argument("script")
    p.add_argument("transcripts")
    p.add_argument("--fold", action="append", default=[], help="heard=meant")
    p = add("ttslint", cmd_ttslint, "spoken-form CLI syntax and repeated phonemes")
    p.add_argument("script")
    p.add_argument("--phonemes", help="JSON of line id to IPA")
    p.add_argument("--allow", action="append", default=[], help="'word1 word2' pair to accept")
    p = add("vooverlap", cmd_vooverlap, "narration lines never overlap")
    p.add_argument("edl")
    p.add_argument("--gap", type=float, default=0.25)
    p.add_argument("--edl-only", action="store_true", help="edit-list method only (needs dur)")
    p = add("static", cmd_static, "visually static stretches")
    p.add_argument("file")
    p.add_argument("--max", type=float, default=3.0)
    p.add_argument("--fps", type=float, default=10.0)
    p.add_argument("--step", type=float, default=0.5)
    p.add_argument("--thresh", type=float, default=3.0)
    p.add_argument("--allow", action="append", default=[], help="start-end seconds")
    p.add_argument("--crop", help="w:h:x:y region to measure (overlay-free picture)")
    p = add("idle", cmd_idle, "machine is quiet enough for ASR or full QC")
    p.add_argument("--max-load", type=float, default=0.5)
    p.add_argument("--wait", type=float, default=0.0)
    p = add("holds", cmd_holds, "on-screen text hold time")
    p.add_argument("storyboard")
    p = add("brief", cmd_brief, "storyboard completeness and evidence")
    p.add_argument("storyboard")
    p = add("manifest", cmd_manifest, "asset licences and checksums")
    p.add_argument("manifest")
    for name, func, text in (("unslop", cmd_unslop, "banned visual patterns"), ("easing", cmd_easing, "linear or inline easing")):
        p = add(name, func, text)
        p.add_argument("path")
        p.add_argument("--allow", action="append", default=[])
    p = add("loudness", cmd_loudness, "EBU R128 integrated loudness and true peak")
    p.add_argument("file")
    p.add_argument("--target", type=float, default=-16.0)
    p.add_argument("--tolerance", type=float, default=1.0)
    p.add_argument("--max-peak", type=float, default=-1.0)
    p.add_argument("--max-lra", type=float, default=11.0)
    for name, func, text in (("colortags", cmd_colortags, "BT.709 tags"), ("levels", cmd_levels, "legal luma range"),
                             ("flash", cmd_flash, "flash rate")):
        p = add(name, func, text)
        p.add_argument("file")
        if name == "flash":
            p.add_argument("--delta", type=float, default=40.0)
    p = add("delivery", cmd_delivery, "encode preset conformance")
    p.add_argument("file")
    p.add_argument("--preset", choices=sorted(PRESETS), default="1080p")
    p.add_argument("--fps", type=float)
    p = add("blackfreeze", cmd_blackfreeze, "unintended black or frozen frames")
    p.add_argument("file")
    p.add_argument("--min", type=float, default=0.5)
    p.add_argument("--allow", action="append", default=[], help="start-end seconds")
    p = add("silence", cmd_silence, "dead air")
    p.add_argument("file")
    p.add_argument("--max", type=float, default=0.7)
    p.add_argument("--noise", type=float, default=-50.0)
    p.add_argument("--allow", action="append", default=[], help="start-end seconds")
    for name, func, default in (("ssim", cmd_ssim, 0.95), ("vmaf", cmd_vmaf, 90.0)):
        p = add(name, func, f"{name} against a reference")
        p.add_argument("file")
        p.add_argument("reference")
        p.add_argument("--min", type=float, default=default)
    p = add("noisefloor", cmd_noisefloor, "noise floor improvement")
    p.add_argument("before")
    p.add_argument("after")
    p.add_argument("--min-gain", type=float, default=10.0)
    p = add("image", cmd_image, "image size, weight and SVG title")
    p.add_argument("file")
    p.add_argument("--size")
    p.add_argument("--max-bytes", type=int)
    p = add("flatframes", cmd_flatframes, "solid frames at cuts")
    p.add_argument("file")
    p.add_argument("--edge", type=float, default=1.0, help="seconds exempt at head and tail")
    p.add_argument("--std", type=float, default=1.0, help="luma standard deviation below which a frame is solid")
    p = add("contenthold", cmd_contenthold, "EDL hold limits")
    p.add_argument("edl")
    p.add_argument("--plain", type=float, default=3.0)
    p.add_argument("--stepped", type=float, default=5.5)
    p = add("claims", cmd_claims, "screen state at narration claims")
    p.add_argument("claims")
    p.add_argument("--video", help="OCR the frame at t when a claim has no screen text")
    p = add("edgeclip", cmd_edgeclip, "text touching the frame edge")
    p.add_argument("file")
    p.add_argument("--fps", type=float, default=2.0)
    p.add_argument("--strip", type=int, default=14)
    p.add_argument("--delta", type=int, default=70)
    p.add_argument("--min-pixels", type=int, default=12)
    p.add_argument("--region", help="y0:y1 rows holding text (default whole frame)")
    p = add("staletext", cmd_staletext, "stale terminal text in a capture")
    p.add_argument("cast")
    p = add("psparse", cmd_psparse, "PowerShell commands parse")
    p.add_argument("file")
    p.add_argument("--static", action="store_true", help="backslash check only, no pwsh")
    p = add("arc", cmd_arc, "explainer beats, hook, CTA and length band")
    p.add_argument("storyboard")
    p.add_argument("--audience", choices=sorted(BANDS))
    p.add_argument("--hook", type=float, default=5.0)
    p.add_argument("--min", type=float)
    p.add_argument("--max", type=float)
    p = add("describe", cmd_describe, "on-screen text spoken or described")
    p.add_argument("storyboard")
    p.add_argument("--min", type=float, default=0.8)
    add("doctor", cmd_doctor, "which tools are present")
    p = add("all", cmd_all, "run a JSON plan of checks")
    p.add_argument("plan")
    return root


def main(argv: list[str] | None = None) -> int:
    args = parser().parse_args(argv)
    try:
        return args.func(args)
    except Skip as skip:
        return report(args.command, SKIPPED, reason=str(skip))
    except (OSError, ValueError, KeyError, json.JSONDecodeError) as error:
        return report(args.command, USAGE, error=str(error))


if __name__ == "__main__":
    sys.exit(main())
