#!/usr/bin/env python3
"""Objective, zero-LLM checks for the design-skills add-on.

Every sub-command prints one JSON object and exits:
0 pass, 1 fail, 2 usage error, 4 skipped (a required tool is missing).
A skipped check is never a pass. Stdlib only; media checks call ffmpeg/ffprobe.
"""

from __future__ import annotations

import argparse
import hashlib
import json
import math
import re
import shutil
import subprocess
import sys
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
    return subprocess.run(command, capture_output=True, text=True, check=False)


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


def cmd_tts(args: argparse.Namespace) -> int:
    reference = words(Path(args.script).read_text(encoding="utf-8"))
    hypothesis = words(Path(args.transcript).read_text(encoding="utf-8"))
    wer = word_error_rate(reference, hypothesis)
    problems = [] if wer <= args.max_wer else [f"WER {wer:.2%} above {args.max_wer:.0%}"]
    wpm = None
    if args.duration:
        wpm = round(len(reference) / (args.duration / 60), 1)
        if not args.min_wpm <= wpm <= args.max_wpm:
            problems.append(f"pace {wpm} wpm outside {args.min_wpm}-{args.max_wpm}")
    return report("tts", FAIL if problems else PASS, wer=round(wer, 4), wpm=wpm, problems=problems)


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
    return report("levels", FAIL if problems else PASS, ymin=ymin, ymax=ymax, problems=problems)


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
    if not shutil.which("ffmpeg"):
        return False
    return "libvmaf" in run(["ffmpeg", "-hide_banner", "-filters"]).stdout


def cmd_vmaf(args: argparse.Namespace) -> int:
    if not has_libvmaf():
        raise Skip("ffmpeg has no libvmaf filter (install a build with libvmaf or the vmaf CLI)")
    result = run(["ffmpeg", "-hide_banner", "-nostats", "-i", args.file, "-i", args.reference,
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
    tools = ["ffmpeg", "ffprobe", "node", "npx", "vhs", "ttyd", "manim", "whisper-cli", "tesseract",
             "realesrgan-ncnn-vulkan", "vmaf", "deep-filter"]
    found = {tool: bool(shutil.which(tool)) for tool in tools}
    found["libvmaf"] = has_libvmaf()
    core = found["ffmpeg"] and found["ffprobe"]
    return report("doctor", PASS if core else SKIPPED, tools=found,
                  note="media checks skip without ffmpeg/ffprobe; optional tools unlock their cards")


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
