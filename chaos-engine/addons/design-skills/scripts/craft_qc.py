"""Craft checks for design_qc.py: motion grammar, copy, layout and edit rhythm.

Sources: references/research/motionmaxxing-map.md (Apache-2.0 ideas, re-expressed)
and the craft cards in references/craft/. Each check is red-then-green tested.
"""

from __future__ import annotations

import json
import math
import re
import statistics
from pathlib import Path

TEXT_SUFFIXES = {".css", ".scss", ".html", ".htm", ".js", ".jsx", ".ts", ".tsx", ".vue", ".svelte", ".mdx",
                 ".md", ".txt", ".json", ".srt", ".vtt"}
CHROME = {
    "hud-counter": r"\b\d{2}\s*[/\u2013\u2014-]\s*[A-Z]{3,}\b",
    "timecode": r"\b\d{2}:\d{2}[:;]\d{2}\b",
    "tracked-caps": r"letter-spacing\s*:\s*(?:0?\.[2-9]\d*|[1-9]\d*(?:\.\d+)?)em",
    "progress-bar": r"class\s*=\s*[\"'][^\"']*\bprogress\b",
    "render-metadata": r"\b\d{2,3}\s?fps\b|\b\d{2,3}\s?bpm\b",
}
COPYSLOP = {
    "unlock": r"\bunlock\b",
    "meet-x": r"\bmeet\s+[A-Z][\w-]+",
    "future-of": r"\bthe future of\b",
    "not-just": r"\bnot just\b[^.]{0,40}\.\s*it'?s\b",
    "one-word-triplet": r"(?:^|[.!?]\s+)[A-Z]\w+\.\s+[A-Z]\w+\.\s+[A-Z]\w+\.",
    "hype": r"\b(?:revolutioni[sz]e|game[- ]chang\w*|supercharge\w*|seamless(?:ly)?|next[- ]level)\b",
}
OVERSHOOT = r"back\.(?:in|out|inout)|elastic|bounce|cubic-bezier\(\s*[\d.]+\s*,\s*(?:-|1\.\d*[1-9])[\d.]*|cubic-bezier\([^)]*,\s*(?:-[\d.]+|1\.\d*[1-9]\d*)\s*,\s*[\d.]+\s*\)"
DURATION = re.compile(r"(?:transition|animation)[^;{}]*?(\d*\.?\d+)(ms|s)\b|duration\s*:\s*(\d*\.?\d+)", re.I)
# Meta Reels ads guide: keep text out of the top 14%, bottom 35% and 6% each side.
# story: 1080x1920 feed story, top ~250 px profile bar, bottom ~340 px link sticker.
SAFE = {"9x16": (0.06, 0.14, 0.06, 0.35), "story": (0.06, 0.13, 0.06, 0.18), "1x1": (0.05, 0.05, 0.05, 0.05), "16x9": (0.05, 0.05, 0.05, 0.10)}

# 1080x1920 app UI map (px): words only inside the band; below it decoration only.
PX_MAP = {"9x16": (168, 1330, 1080)}
BIDI_CONTROLS = re.compile("[\u200e\u200f\u061c\u202a-\u202e\u2066-\u2069]")
ARABIC = re.compile("[\u0600-\u06FF\u0750-\u077F\uFB50-\uFDFF\uFE70-\uFEFF]")


def files_of(root: str, suffixes: set[str]) -> list[Path]:
    base = Path(root)
    return [base] if base.is_file() else sorted(p for p in base.rglob("*") if p.suffix in suffixes and p.is_file())


def scan(qc, check: str, root: str, rules: dict[str, str], allow: list[str], case: bool = False) -> int:
    hits, files = [], files_of(root, TEXT_SUFFIXES)
    flags = 0 if case else re.IGNORECASE
    for file in files:
        for number, line in enumerate(file.read_text(encoding="utf-8", errors="replace").splitlines(), 1):
            if "design-qc: allow" in line:
                continue
            hits += [f"{file}:{number} {rule}" for rule, pat in rules.items() if rule not in allow and re.search(pat, line, flags)]
    return qc.report(check, qc.FAIL if hits else qc.PASS, files=len(files), hits=hits)


def text_of(root: str) -> str:
    return "\n".join(f.read_text(encoding="utf-8", errors="replace") for f in files_of(root, TEXT_SUFFIXES))


def cmd_fadeonly(qc, args) -> int:
    hits = []
    for name, body in re.findall(r"@keyframes\s+([\w-]+)\s*\{((?:[^{}]*\{[^{}]*\})*[^{}]*)\}", text_of(args.path)):
        if "opacity" in body and not re.search(r"transform|translate|scale|rotate|filter|clip-path|inset", body):
            hits.append(name)
    return qc.report("fadeonly", qc.FAIL if hits else qc.PASS, hits=hits)


def cmd_springs(qc, args) -> int:
    found = re.findall(OVERSHOOT, text_of(args.path), re.I)
    soft = [f for f in found if re.fullmatch(r"back\.\w+", f, re.I)]
    # a gentle back ease (amount <= 1.7, a late lean) is not a visible spring
    gentle = len(re.findall(r"back\.\w+\(\s*(?:0?\.\d+|1(?:\.[0-7]\d*)?)\s*\)", text_of(args.path), re.I))
    count = len(found) - min(gentle, len(soft))
    return qc.report("springs", qc.FAIL if count > args.max else qc.PASS, springs=count, max=args.max)


def cmd_motiondur(qc, args) -> int:
    values = []
    for match in DURATION.finditer(text_of(args.path)):
        if match.group(1):
            values.append(float(match.group(1)) / (1000.0 if match.group(2).lower() == "ms" else 1.0))
        else:
            raw = float(match.group(3))
            values.append(raw / 1000.0 if raw > 20 else raw)
    if not values:
        raise qc.Skip("no durations found")
    median = statistics.median(values)
    return qc.report("motiondur", qc.FAIL if median > args.max else qc.PASS, median_s=round(median, 3), n=len(values))


def cmd_measure(qc, args) -> int:
    hits = [f"{v}ch" for v in re.findall(r"max-width\s*:\s*(\d+(?:\.\d+)?)ch", text_of(args.path))
            if not args.min <= float(v) <= args.max]
    return qc.report("measure", qc.FAIL if hits else qc.PASS, hits=hits, band=[args.min, args.max])


def cmd_typescale(qc, args) -> int:
    sizes = sorted({float(v) for v in re.findall(r"font-size\s*:\s*(\d+(?:\.\d+)?)px", text_of(args.path))}, reverse=True)
    if not sizes:
        raise qc.Skip("no px font sizes")
    tight = [f"{a:g}/{b:g}" for a, b in zip(sizes, sizes[1:]) if a / b < args.min_ratio]
    failed = tight or len(sizes) > args.max_sizes
    return qc.report("typescale", qc.FAIL if failed else qc.PASS, sizes=sizes, tight=tight)


def cmd_safezones(qc, args) -> int:
    boxes = json.loads(Path(args.file).read_text(encoding="utf-8"))["boxes"]
    if args.px_map:
        top, floor, width = PX_MAP[args.px_map]
        hits = [b.get("id", "?") for b in boxes if b.get("kind", "text") != "decoration"
                and (b["y"] < top or b["y"] + b["h"] > floor or (args.rail and b["x"] + b["w"] > width - args.rail))]
        return qc.report("safezones", qc.FAIL if hits else qc.PASS, hits=hits, px_map=args.px_map,
                         band=[top, floor], rail=args.rail)
    left, top, right, bottom = SAFE[args.format]
    hits = [b.get("id", "?") for b in boxes
            if b["x"] < left or b["y"] < top or b["x"] + b["w"] > 1 - right or b["y"] + b["h"] > 1 - bottom]
    return qc.report("safezones", qc.FAIL if hits else qc.PASS, hits=hits, format=args.format)


def lumas(qc, path: str) -> tuple[list[bytes], float]:
    fps = qc.video_rate(path)
    raw = qc.decode_raw(path, ["-an", "-vf", "scale=64:36:flags=area,format=gray", "-f", "rawvideo"])
    frames = [raw[i:i + 2304] for i in range(0, len(raw) - 2303, 2304)]
    if len(frames) < 2:
        raise ValueError("too few frames")
    return frames, fps


def diff(a: bytes, b: bytes) -> float:
    return sum(abs(x - y) for x, y in zip(a, b)) / len(a)


def cmd_endhold(qc, args) -> int:
    frames, fps = lumas(qc, args.file)
    last, run = frames[-1], 0
    for frame in reversed(frames[:-1]):
        if diff(frame, last) > args.thresh:
            break
        run += 1
    hold = run / fps
    failed = hold > args.max or hold < args.min
    return qc.report("endhold", qc.FAIL if failed else qc.PASS, hold_s=round(hold, 2), band=[args.min, args.max])


def cmd_motionband(qc, args) -> int:
    frames, _ = lumas(qc, args.file)
    mean = statistics.mean(diff(a, b) for a, b in zip(frames, frames[1:]))
    failed = not args.low <= mean <= args.high
    return qc.report("motionband", qc.FAIL if failed else qc.PASS, mean=round(mean, 2), band=[args.low, args.high])


def cmd_hook(qc, args) -> int:
    frames, fps = lumas(qc, args.file)
    poster = qc.frame_std(frames[0])
    window = frames[:max(2, int(args.window * fps) + 1)]
    change = max(diff(window[0], f) for f in window[1:])
    failed = poster < args.min_std or change < args.min_change
    return qc.report("hook", qc.FAIL if failed else qc.PASS, poster_std=round(poster, 1), change=round(change, 1))


def cmd_cutrhythm(qc, args) -> int:
    frames, fps = lumas(qc, args.file)
    diffs = [diff(a, b) for a, b in zip(frames, frames[1:])]
    floor = max(args.cut, 4 * statistics.median(diffs))  # a cut stands out from the film's own motion
    cuts: list[int] = []
    for i, d in enumerate(diffs):
        if d > floor and (not cuts or i + 1 - cuts[-1] > fps * 0.2):  # a fast move spans frames, a cut is one
            cuts.append(i + 1)
    edges = [0, *cuts, len(frames)]
    shots = [round((b - a) / fps, 2) for a, b in zip(edges, edges[1:]) if b > a]
    longest = max(shots)
    return qc.report("cutrhythm", qc.FAIL if longest > args.max_shot else qc.PASS, shots=shots, max_shot=args.max_shot)

# W02/W04/W05/W01 card rules (#7347). Re-expressed from the cards' own Sources.
LAYOUT_PROPS = r"(?:(?:min-|max-)?(?:width|height)|top|left|right|bottom|margin[\w-]*|padding[\w-]*|inset[\w-]*)"
UIMOTION = {
    "scale-zero": r"scale(?:3d|X|Y)?\(\s*0(?:\.0+)?\s*(?:[,)])|\bscale\s*:\s*0(?:\.0+)?\s*(?:[,;}]|$)",
    "layout-transition": r"transition(?:-property)?\s*:[^;{}]*\b" + LAYOUT_PROPS + r"\b(?![\w-])",
}
REVEAL = re.compile(r"\b(?:opacity\s*:\s*1\b|visibility\s*:\s*visible|display\s*:\s*(?!none)\w)", re.I)
GATED = re.compile(r":hover[^,{]*(?:\s|>)(?:a|button|p|span|h[1-6]|\.[\w-]*(?:cta|link|text|label)[\w-]*)\b", re.I)


def css_rules(text: str):
    """Yield (selector, body, inside_hover_media, line) for each flat rule; nested @media tracked."""
    stack, sel_start = [], 0
    for m in re.finditer(r"[{}]", text):
        if m.group() == "{":
            head = text[sel_start:m.start()].strip()
            stack.append((head, m.end()))
        elif stack:
            head, start = stack.pop()
            if not head.startswith("@"):
                media = any(re.search(r"\(\s*hover\s*:\s*hover\s*\)", h) for h, _ in stack if h.startswith("@media"))
                yield head, text[start:m.start()], media, text.count("\n", 0, start) + 1
        sel_start = m.end()


def cmd_maskprefix(qc, args) -> int:
    hits, files = [], files_of(args.path, {".css", ".scss", ".html", ".htm", ".vue", ".svelte"})
    for file in files:
        text = file.read_text(encoding="utf-8", errors="replace")
        for _selector, body, _media, line in css_rules(text):
            if "design-qc: allow" in body:
                continue
            for prop in ("mask", "mask-image"):
                plain = re.search(rf"(?<![-\w]){prop}\s*:", body)
                webkit = re.search(rf"-webkit-{prop}\s*:", body)
                if plain and not webkit:
                    hits.append(f"{file}:{line} missing -webkit-{prop}")
                elif webkit and not plain:
                    hits.append(f"{file}:{line} missing {prop}")
    return qc.report("maskprefix", qc.FAIL if hits else qc.PASS, files=len(files), hits=hits)


def cmd_hovergate(qc, args) -> int:
    hits, files = [], files_of(args.path, {".css", ".scss", ".html", ".htm", ".vue", ".svelte"})
    for file in files:
        text = file.read_text(encoding="utf-8", errors="replace")
        for selector, body, media, line in css_rules(text):
            if ":hover" not in selector or "design-qc: allow" in body:
                continue
            if GATED.search(selector) and REVEAL.search(body):
                hits.append(f"{file}:{line} hover-gated-content")
            elif not media:
                hits.append(f"{file}:{line} ungated-hover")
    return qc.report("hovergate", qc.FAIL if hits else qc.PASS, files=len(files), hits=hits)


def cmd_uimotion(qc, args) -> int:
    hits, files = [], files_of(args.path, TEXT_SUFFIXES)
    for file in files:
        text = file.read_text(encoding="utf-8", errors="replace")
        for number, line in enumerate(text.splitlines(), 1):
            if "design-qc: allow" not in line:
                hits += [f"{file}:{number} {rule}" for rule, pat in UIMOTION.items() if re.search(pat, line)]
        for m in re.finditer(r"@keyframes\s+([\w-]+)\s*\{((?:[^{}]*\{[^{}]*\})*[^{}]*)\}", text):
            if re.search(r"(?:^|[;{\s])" + LAYOUT_PROPS + r"\s*:", m.group(2)) and "design-qc: allow" not in m.group(0):
                hits.append(f"{file}:{text.count(chr(10), 0, m.start()) + 1} layout-keyframe {m.group(1)}")
    return qc.report("uimotion", qc.FAIL if hits else qc.PASS, files=len(files), hits=hits)


def cmd_svga11y(qc, args) -> int:
    hits, files = [], files_of(args.path, {".svg", ".html", ".htm", ".vue", ".svelte", ".jsx", ".tsx", ".mdx"})
    for file in files:
        text = file.read_text(encoding="utf-8", errors="replace")
        for m in re.finditer(r"<svg\b([^>]*)>(.*?)</svg>", text, re.I | re.S):
            tag, inner = m.group(1), m.group(2)
            if re.search(r"aria-hidden\s*=\s*[\"']?true", tag, re.I) or "design-qc: allow" in tag:
                continue
            missing = [n for n, ok in (("role-img", re.search(r"role\s*=\s*[\"']img", tag, re.I)),
                                       ("title", re.search(r"<title\b", inner, re.I)),
                                       ("desc", re.search(r"<desc\b", inner, re.I))) if not ok]
            if missing:
                hits.append(f"{file}:{text.count(chr(10), 0, m.start()) + 1} missing {'+'.join(missing)}")
    return qc.report("svga11y", qc.FAIL if hits else qc.PASS, files=len(files), hits=hits)


CLOSED = re.compile(r"\b(?:fixed|closed|done|resolved|pass(?:ed)?)\b", re.I)


def cmd_floor(qc, args) -> int:
    hits = []
    for number, line in enumerate(Path(args.file).read_text(encoding="utf-8", errors="replace").splitlines(), 1):
        if re.search(r"\bFloor\b", line) and re.search(r"\bopen\b", line, re.I) and not CLOSED.search(line):
            hits.append(f"{number} open-floor")
        if re.search(r"\bReflex\b", line) and re.search(r"overrul", line, re.I) and not re.search(r"reason\s*:\s*\S", line, re.I):
            hits.append(f"{number} reflex-overrule-without-reason")
    return qc.report("floor", qc.FAIL if hits else qc.PASS, hits=hits)

# Caption and video lessons, batch b (#7351): L02 bidi, L06 caption/video QC.
RTL = re.compile(r"[\u0590-\u08FF\uFB1D-\uFDFF\uFE70-\uFEFF]")
ISOLATE = re.compile(r"[\u2066\u2067\u2068].*?\u2069", re.S)
EDGE = r"(?=$|[\s\u0590-\u08FF\uFB1D-\uFDFF\uFE70-\uFEFF.,،؛؟!?])"
LEAD = r"(?:^|(?<=[\s\u0590-\u08FF\uFB1D-\uFDFF\uFE70-\uFEFF]))"
BIDI = {
    "curly-quote-in-latin": r"[A-Za-z][\u2018\u2019]|[\u2018\u2019][A-Za-z]",
    "double-quote-at-latin": r"[A-Za-z0-9]\"|\"[A-Za-z]",
    "dash-at-latin-edge": LEAD + r"-{1,2}[A-Za-z]|[A-Za-z0-9]-{1,2}" + EDGE,
    "colon-at-latin-edge": r"[A-Za-z0-9]:" + EDGE,
}
PERCENT = re.compile(r"\d\s?[%\u066A]|[%\u066A]\s?\d")
HALLUCINATIONS = ("اشتركوا في القناة", "شكرا للمشاهدة", "شكرا على المشاهدة", "thanks for watching",
                  "thank you for watching", "please subscribe", "bye")
BOUND = re.compile(r"(?:^|\s)(?:[وبلكف]|[\u0600-\u06FF]{1,2}\u0640)\s*$")


WORD_KEYS, START_KEYS, END_KEYS = ("word", "w", "text"), ("start", "s"), ("end", "e")


def _key(item: dict, keys: tuple[str, ...]):
    return next((item[k] for k in keys if k in item), None)


def words_of(data) -> list[dict]:
    """Word-level ASR entries in any common shape: {word|w, start|s, end|e}, top-level or under words/segments."""
    found: list[dict] = []

    def walk(node):
        if isinstance(node, list):
            for value in node:
                walk(value)
        elif isinstance(node, dict):
            word = _key(node, WORD_KEYS[:2])
            if isinstance(word, str):
                start, end = _key(node, START_KEYS), _key(node, END_KEYS)
                found.append({"word": word.strip(), "start": None if start is None else float(start),
                              "end": None if end is None else float(end)})
                return
            for value in node.values():
                walk(value)
    walk(data)
    return found


def strings_of(file: Path) -> list[tuple[str, str]]:
    """(location, text) pairs: JSON string values (hook/label specs), else real file lines; SRT adds the cue number."""
    text = file.read_text(encoding="utf-8", errors="replace")
    if file.suffix == ".json":
        out: list[tuple[str, str]] = []

        def walk(node, path):
            if isinstance(node, str):
                out.extend((f"{path or '$'}", line) for line in node.splitlines())
            elif isinstance(node, dict):
                for key, value in node.items():
                    walk(value, f"{path}.{key}")
            elif isinstance(node, list):
                for index, value in enumerate(node):
                    walk(value, f"{path}[{index}]")
        walk(json.loads(text), "")
        return out
    if file.suffix in {".html", ".htm"}:
        text = re.sub(r"<bdi\b[^>]*>.*?</bdi>", lambda m: "\n" * m.group(0).count("\n"), text, flags=re.S | re.I)
        text = re.sub(r"<[^>]+>", " ", text)
    lines = text.splitlines()
    out, cue = [], 0
    for number, line in enumerate(lines, 1):
        if "-->" in line:
            cue += 1
            continue
        if line.strip().isdigit() and number < len(lines) and "-->" in lines[number]:
            continue
        where = f"{number}" + (f" (cue {cue})" if file.suffix in {".srt", ".vtt"} and cue else "")
        out.append((where, line))
    return out


def cmd_bidi(qc, args) -> int:
    hits, warnings = [], []
    files = files_of(args.path, {".srt", ".vtt", ".txt", ".json", ".html", ".htm", ".md"})
    for file in files:
        for where, line in strings_of(file):
            if not RTL.search(line) or "design-qc: allow" in line:
                continue
            bare = ISOLATE.sub(" ", line)
            hits += [f"{file}:{where} {rule}" for rule, pat in BIDI.items() if re.search(pat, bare)]
            # G8: trailing Latin .!? must sit inside LRI…PDI (outside paints as leading under dir=rtl).
            if re.search(r"\u2069[.!?]", line) or re.search(r"[A-Za-z0-9][.!?]", ISOLATE.sub("", line)):
                hits.append(f"{file}:{where} trailing-latin-punct-outside-isolate")
            if PERCENT.search(line):
                warnings.append(f"{file}:{where} percent-in-rtl (normal RTL order, warn only)")
    return qc.report("bidi", qc.FAIL if hits else qc.PASS, files=len(files), hits=hits, warnings=warnings)


def cmd_cuegaps(qc, args) -> int:
    cues = qc.parse_cues(Path(args.file).read_text(encoding="utf-8"))
    eps = 1e-3
    def covered(at: float) -> bool:
        return any(c["start"] - eps <= at <= c["end"] + eps for c in cues)
    hits = [f"join {join:.3f}s has no caption" for join in args.join
            if not all(covered(at) for at in (join - args.frame, join, join + args.frame))]
    return qc.report("cuegaps", qc.FAIL if hits else qc.PASS, joins=len(args.join), hits=hits)


def cmd_asrhall(qc, args) -> int:
    file = Path(args.file)
    phrases = [p.lower() for p in (*HALLUCINATIONS, *args.phrase)]
    texts = [(where, line) for where, line in strings_of(file) if "design-qc: allow" not in line]
    if file.suffix == ".json":
        words = words_of(json.loads(file.read_text(encoding="utf-8")))
        if words:  # match phrases across consecutive words, not word by word
            texts = []
            for index in range(len(words)):
                run = " ".join(w["word"] for w in words[index:index + 6])
                texts.append((f"word {index + 1} @{words[index]['start']}s", run))
    hits = []
    for where, line in texts:
        low = re.sub(r"\s+", " ", line.lower())
        anchor = "^" if where.startswith("word ") else r"(?<!\w)"  # word runs: phrase starts at this word
        hits += [f"{where} {p}" for p in phrases if re.search(anchor + re.escape(p) + r"(?!\w)", low)]
    return qc.report("asrhall", qc.FAIL if hits else qc.PASS, hits=hits)


def cmd_boundprefix(qc, args) -> int:
    cues = qc.parse_cues(Path(args.file).read_text(encoding="utf-8"))
    hits = [f"cue {i}: line ends on a bound prefix" for i, cue in enumerate(cues, 1)
            for line in cue["lines"] if BOUND.search(ISOLATE.sub("", line).rstrip())]
    return qc.report("boundprefix", qc.FAIL if hits else qc.PASS, cues=len(cues), hits=hits)


# U2 G7: ASR paints English tech terms as Arabic-script transliterations.
# Prefer Latin with the bound article outside the isolate (L02). Placeholder lexicon
# only — project terms arrive via --term AR=LATIN, never shipped here.
TERM_WORDS = {
    "داتايز": "database", "داتابيز": "database", "داتابيس": "database", "داتابايز": "database",
    "تست": "test", "تستنج": "testing", "فريمورك": "framework", "باكند": "backend", "فرونتند": "frontend",
    # Common English loanwords (U9 G15) — generic placeholders only, never brand names.
    "فري": "free", "سولو": "solo", "فاوندر": "founder",
}
TERM_PHRASES = (("فريم ورك", "framework"), ("تست كيس", "test case"), ("تست كيسز", "test cases"))
TERM_PREF = ("وال", "فال", "بال", "لل", "ال", "و", "ف", "ب", "ك")


def _term_lexicon_maps(extra: list[str]) -> tuple[dict[str, str], list[tuple[str, str]]]:
    words, phrases = dict(TERM_WORDS), list(TERM_PHRASES)
    for item in extra:
        if "=" not in item:
            continue
        ar, lat = item.split("=", 1)
        ar, lat = ar.strip(), lat.strip()
        if not ar or not lat:
            continue
        if " " in ar:
            phrases.append((ar, lat))
        else:
            words[ar] = lat
    phrases.sort(key=lambda p: len(p[0]), reverse=True)
    return words, phrases


def term_lexicon_hits(text: str, words: dict[str, str], phrases: list[tuple[str, str]]) -> list[str]:
    """Flag leftover Arabic-script forms of known tech/loan terms (optional bound prefixes)."""
    hits: list[str] = []
    joined = text.replace(" ", " ")
    for ar, lat in phrases:
        if ar in joined:
            hits.append(f"Arabic-script tech transliteration {ar!r} → prefer Latin {lat!r}")
    for tok in joined.split():
        core = tok.rstrip("؟?.!")
        if core in words:
            hits.append(f"Arabic-script tech transliteration {tok!r} → prefer Latin {words[core]!r}")
            continue
        for pref in TERM_PREF:
            if core.startswith(pref) and len(core) > len(pref):
                rest = core[len(pref):]
                if rest.startswith("ـ"):  # tatweel after article
                    rest = rest[1:]
                if rest in words:
                    hits.append(
                        f"Arabic-script tech transliteration {tok!r} → prefer Latin {words[rest]!r} "
                        "(bound article outside)"
                    )
                    break
    return hits


def cmd_termlexicon(qc, args) -> int:
    words, phrases = _term_lexicon_maps(args.term)
    cues = qc.parse_cues(Path(args.file).read_text(encoding="utf-8"))
    hits = [f"cue {i}: {hit}" for i, cue in enumerate(cues, 1)
            for hit in term_lexicon_hits(" ".join(cue["lines"]), words, phrases)]
    return qc.report("termlexicon", qc.FAIL if hits else qc.PASS, cues=len(cues), hits=hits)


def cmd_fadetail(qc, args) -> int:
    data = json.loads(Path(args.words).read_text(encoding="utf-8"))
    ends = [w["end"] for w in words_of(data) if w["end"] is not None]
    if not ends:
        return qc.report("fadetail", qc.FAIL, hits=["no word end times (keys end|e) in words file"])
    last = max(ends)
    hits = []
    if args.end < last + args.pause - 1e-6:
        hits.append(f"span end {args.end:.3f}s before last word {last:.3f}s + pause {args.pause}s")
    if args.fade > 0.15 + 1e-6:
        hits.append(f"tail fade {args.fade}s over 0.15s dims the last word")
    if args.end - args.fade < last - 1e-6:
        hits.append("fade starts before the last word ends")
    return qc.report("fadetail", qc.FAIL if hits else qc.PASS, last_word=last, hits=hits)


def cmd_cuemids(qc, args) -> int:
    cues = qc.parse_cues(Path(args.file).read_text(encoding="utf-8"))
    mids = [round((c["start"] + c["end"]) / 2, 3) for c in cues]
    snapped, problems = [], []
    for probe in args.probe:
        cue = min(cues, key=lambda c: 0 if c["start"] <= probe <= c["end"] else min(abs(probe - c["start"]), abs(probe - c["end"])))
        lo, hi = cue["start"] + 0.25, cue["end"] - 0.25
        if lo > hi:
            problems.append(f"probe {probe}: cue shorter than 0.5 s, use its midpoint")
            snapped.append(round((cue["start"] + cue["end"]) / 2, 3))
        else:
            snapped.append(round(min(max(probe, lo), hi), 3))
    return qc.report("cuemids", qc.PASS if cues else qc.FAIL, midpoints=mids, probes=snapped, notes=problems)


def cue_list(qc, path: str) -> list[dict]:
    return qc.parse_cues(Path(path).read_text(encoding="utf-8"))


def gray_at(qc, path: str, at: float, crop: str | None = None) -> bytes:
    """One grayscale frame at `at` seconds (optionally cropped), scaled to 64 px wide."""
    chain = [f"select=gte(t\\,{at:.3f})"] if at > 0 else []
    chain += [f"crop={crop}"] if crop else []
    chain.append("scale=64:-2:flags=area,format=gray")
    return qc.decode_raw(path, ["-an", "-vf", ",".join(chain), "-frames:v", "1", "-f", "rawvideo"])


def cmd_capluma(qc, args) -> int:
    """Brightness behind each cue's text box picks the blend; plain subtitles over 60 need a text-sized scrim."""
    x, y, w, h = (int(v) for v in args.box.split(","))
    rows = []
    for index, cue in enumerate(cue_list(qc, args.cues), 1):
        frame = gray_at(qc, args.file, (cue["start"] + cue["end"]) / 2, f"{w}:{h}:{x}:{y}")
        if not frame:
            continue
        luma = round(sum(frame) / len(frame), 1)
        blend = "screen" if luma < 60 else "overlay" if luma <= 180 else "normal+opaque"
        rows.append({"cue": index, "luma": luma, "blend": blend})
    hits = [f"cue {r['cue']}: luma {r['luma']} behind plain subtitles needs a 30-40% text-sized scrim"
            for r in rows if args.plain and r["luma"] > 60]
    return qc.report("capluma", qc.FAIL if hits else qc.PASS, cues=rows, hits=hits)


def cmd_firstframe(qc, args) -> int:
    """Frame 0 is the thumbnail: not black, not flat; captions start 0.1-0.3 s in."""
    frame = gray_at(qc, args.file, 0)
    mean = sum(frame) / len(frame)
    spread = statistics.pstdev(frame)
    hits = []
    if mean < args.black or spread < args.flat:
        hits.append(f"empty first frame (mean {mean:.1f}, spread {spread:.1f})")
    if args.cues:
        cues = cue_list(qc, args.cues)
        if cues and cues[0]["start"] < 0.1:
            hits.append(f"first cue at {cues[0]['start']:.2f}s looks pre-existing; start 0.1-0.3 s in")
    return qc.report("firstframe", qc.FAIL if hits else qc.PASS, mean=round(mean, 1), spread=round(spread, 1), hits=hits)


def cmd_deadair(qc, args) -> int:
    """Over max seconds with a static picture and silent audio; a film over 15 s with no sound."""
    probe = qc.ffprobe(args.file)
    duration = float(probe.get("format", {}).get("duration", 0) or 0)
    has_audio = any(s.get("codec_type") == "audio" for s in probe.get("streams", []))
    hits = []
    if not has_audio:
        if duration > args.silent_film:
            hits.append(f"film_silent: {duration:.1f}s with no audio stream")
        quiet = [(0.0, duration)]
    else:
        log = qc.ffmpeg_filter_log(args.file, f"silencedetect=n={args.noise}dB:d={args.max}", audio=True)
        starts = [float(v) for v in re.findall(r"silence_start: (-?[\d.]+)", log)]
        ends = [float(v) for v in re.findall(r"silence_end: ([\d.]+)", log)] + [duration]
        quiet = list(zip(starts, ends))
    grid = "scale='if(gt(iw,ih),32,18)':'if(gt(iw,ih),18,32)':flags=area,format=gray"
    raw = qc.decode_raw(args.file, ["-an", "-vf", f"fps={args.fps},{grid}", "-f", "rawvideo"])
    frames = [raw[i:i + 576] for i in range(0, len(raw) - 575, 576)]
    spans, _ = qc.static_spans(frames, args.fps, args.thresh, args.max, [])
    warnings = []
    for span in spans:
        overlap = max((min(span["end"], b) - max(span["start"], a) for a, b in quiet), default=0.0)
        if overlap > args.max:
            text = f"{span['start']:.2f}-{span['end']:.2f}s: static picture and silent audio"
            # A held end card (list or CTA card, V07 rule 14) is the last span after real content: warn only.
            if span["start"] > 0 and span["end"] >= duration - 2 / args.fps:
                warnings.append(f"final_hold {text}")
            else:
                hits.append(f"dead_stretch {text}")
    return qc.report("deadair", qc.FAIL if hits else qc.PASS, duration=round(duration, 2), hits=hits, warnings=warnings)



def cmd_silencegap(qc, args) -> int:
    """RMS mid-answer silence gaps on the final render (post-denoise/loudnorm).

    silencedetect alone understated a ~1.25 s gap as 0.69 s. Gate: FAIL if any
    mid-answer gap (after hook_end, before content_end) exceeds --max (0.6 s).
    Pass --content-end (before end-card); default duration-0.3 is not safe for
    long end-card pads. Optional --target (default 0.45) warns above the soft
    comfort margin without failing; hard FAIL stays at --max (0.6).
    """
    import math
    import struct
    import subprocess
    import tempfile
    import wave

    src = Path(args.file)
    with tempfile.TemporaryDirectory() as folder:
        wav = Path(folder) / "mono.wav"
        if src.suffix.lower() == ".wav":
            wav = src
        else:
            subprocess.run(  # nosec B603 - fixed argv, absolute ffmpeg via qc.need
                [qc.need("ffmpeg"), "-v", "error", "-y", "-i", str(src), "-ac", "1", "-ar", "48000", str(wav)],
                check=True, capture_output=True)
        with wave.open(str(wav), "rb") as handle:
            if handle.getnchannels() != 1 or handle.getsampwidth() != 2:
                raise qc.Skip("need mono 16-bit PCM after decode")
            rate = handle.getframerate()
            frames = handle.readframes(handle.getnframes())
    samples = struct.unpack("<" + "h" * (len(frames) // 2), frames)
    if not samples:
        raise qc.Skip("empty audio")
    duration = len(samples) / rate
    hop = max(1, int(round(args.hop * rate)))
    n = len(samples) // hop
    silent = []
    for i in range(n):
        chunk = samples[i * hop:(i + 1) * hop]
        mean_sq = sum(s * s for s in chunk) / len(chunk) + 1e-20
        db = 20 * math.log10(math.sqrt(mean_sq) / 32768.0)
        silent.append(db <= args.thr)
    gaps = []
    i = 0
    while i < n:
        if not silent[i]:
            i += 1
            continue
        j = i
        while j < n and silent[j]:
            j += 1
        gaps.append((i * args.hop, j * args.hop, (j - i) * args.hop))
        i = j
    content_end = args.content_end if args.content_end is not None else max(0.0, duration - 0.3)
    mid, fail = [], []
    for start, end, _ in gaps:
        clipped_s, clipped_e = max(start, args.hook_end), min(end, content_end)
        length = clipped_e - clipped_s
        if length >= args.min_gap:
            item = {"s": round(clipped_s, 3), "e": round(clipped_e, 3), "d": round(length, 3)}
            mid.append(item)
            if length > args.max + 1e-9:
                fail.append(item)
    max_mid = max((g["d"] for g in mid), default=0.0)
    soft = []
    if args.target is not None:
        soft = [g for g in mid if args.target + 1e-9 < g["d"] <= args.max + 1e-9]
    warnings = []
    if soft:
        warnings.append(
            f"soft_target: max mid-answer gap {max_mid:.3f}s above target {args.target:.2f}s "
            f"(hard gate still {args.max:.2f}s)"
        )
    return qc.report(
        "silencegap", qc.FAIL if fail else qc.PASS,
        max_gap=args.max, target=args.target, hook_end=args.hook_end,
        content_end=round(content_end, 3),
        duration=round(duration, 3), max_mid_answer_gap=round(max_mid, 3),
        mid_answer_gaps=mid, fail_gaps=fail, soft_gaps=soft, warnings=warnings)


TXDIFF_PUNCT = re.compile(r"[.,!?;:؟،؛\"'()\[\]{}…—–-]+")


def _txdiff_tokens(text: str) -> list[str]:
    cleaned = TXDIFF_PUNCT.sub(" ", text.replace("\u00a0", " ").replace("\u0640", ""))
    return [w for w in cleaned.split() if w and not w.endswith("ـ")]


def _txdiff_fold(word: str) -> str:
    """Exact-raw fold: Latin .lower(); Arabic unchanged. No alias, no fuzzy."""
    return word.lower() if re.search(r"[A-Za-z]", word) else word


def _txdiff_misses(cues, words, tokens_out=None):
    """Return (tokens, miss_exact, miss_fuzzy) for SRT cues vs a words list."""
    import difflib

    tokens, miss_exact, miss_fuzzy = [], [], []
    for cue in cues:
        pool = [TXDIFF_PUNCT.sub("", w["word"]) for w in words
                if w["end"] is not None and w["start"] is not None
                and w["end"] > cue["start"] - 0.4 and w["start"] < cue["end"] + 0.4]
        pool_exact = [_txdiff_fold(x) for x in pool]
        for token in _txdiff_tokens(" ".join(cue["lines"])):
            tokens.append(token)
            folded = _txdiff_fold(token)
            if folded not in pool_exact:
                miss_exact.append(token)
            if not any(difflib.SequenceMatcher(None, folded, p).ratio() >= 0.72 for p in pool_exact):
                miss_fuzzy.append(token)
    if tokens_out is not None:
        tokens_out.extend(tokens)
    return tokens, miss_exact, miss_fuzzy


def cmd_txdiff(qc, args) -> int:
    """SRT ↔ words.json exact-raw + fuzzy (G9). G20: always report normalized + RAW ASR."""
    cues = qc.parse_cues(Path(args.srt).read_text(encoding="utf-8"))
    words = words_of(json.loads(Path(args.words).read_text(encoding="utf-8")))
    tokens, miss_exact, miss_fuzzy = _txdiff_misses(cues, words)
    n = max(1, len(tokens))
    diff_exact = len(miss_exact) / n
    diff_fuzzy = len(miss_fuzzy) / n
    problems = []
    if diff_exact > args.max:
        problems.append(f"exact-raw normalized {diff_exact:.2%} above {args.max:.0%}")

    diff_asr = None
    miss_asr: list[str] = []
    if getattr(args, "words_raw", None):
        raw_words = words_of(json.loads(Path(args.words_raw).read_text(encoding="utf-8")))
        _, miss_asr, _ = _txdiff_misses(cues, raw_words)
        diff_asr = len(miss_asr) / n
        if diff_asr > args.max:
            problems.append(f"exact-raw ASR {diff_asr:.2%} above {args.max:.0%}")
        note = (
            "G9+G20: gate on exact-raw vs normalized words AND vs RAW ASR (both ≤ max); "
            "always report diff_exact_raw_normalized + diff_exact_raw_asr + fuzzy"
        )
    else:
        note = (
            "gate on exact-raw (no alias, no fuzzy); fuzzy is companion only; "
            "G20: pass --words-raw to report ASR exact-raw too"
        )

    return qc.report(
        "txdiff", qc.FAIL if problems else qc.PASS,
        mode="exact-raw+fuzzy",
        gate="G20" if getattr(args, "words_raw", None) else "G9",
        diff_exact_raw=round(diff_exact, 4),
        diff_exact_raw_normalized=round(diff_exact, 4),
        diff_exact_raw_asr=None if diff_asr is None else round(diff_asr, 4),
        diff_fuzzy=round(diff_fuzzy, 4),
        unmatched_exact_raw=len(miss_exact),
        unmatched_exact_raw_asr=len(miss_asr),
        unmatched_fuzzy=len(miss_fuzzy),
        miss_exact_raw=miss_exact[:20],
        miss_exact_raw_asr=miss_asr[:20],
        miss_fuzzy=miss_fuzzy[:20],
        srt_words=len(tokens),
        max=args.max,
        problems=problems,
        note=note,
    )



QUESTION_MARK = re.compile(r"[?؟]")
# Leading/near-start interrogatives (Arabic + Latin); keep neutral / short.
QUESTION_LEAD = re.compile(
    r"(?i)(?:^|[\s\u0600-\u06FF]{0,3})(?:هل|وهل|إزاي|ازاي|ليه|مين|فين|متى|what|why|how|who|when|is|are|do|does|did)\b"
)


def _is_question(text: str) -> bool:
    return bool(QUESTION_MARK.search(text) or QUESTION_LEAD.search(text.strip()))



# Dodge / next-question / topic-pivot tails are not an answer beat (U9 G14).
ANSWER_DODGE = re.compile(
    r"(?i)مش\s*عارف|معرفش|ما\s*عرفش|مانيش\s*عارف|مش\s*متأكد|مش\s*فاهم|"
    r"الله\s*أعلم|مفيش\s*فكرة|ما\s*عنديش\s*(?:إجابة|اجابه|فكرة)|"
    r"I\s*don'?t\s*know|I\s*have\s*no\s*idea|no\s*idea|dunno|not\s*sure|"
    r"beats\s*me|can'?t\s*say|no\s*comment"
)
# Next-question / move-on + soft topic-pivot leads (after a dodge these kill the beat).
ANSWER_PIVOT = re.compile(
    r"(?i)السؤال\s+اللي\s+بعدي[هة]?|السؤال\s+التالي|السؤال\s+الجاي|"
    r"خلينا\s+(?:نعدي|ننتقل|نروح|نتكلم\s+عن)|طب\s+(?:نروح|نعدي|ننتقل)|"
    r"let'?s\s+move\s+(?:on|to\s+the\s+next)|(?:move|moving)\s+on\b|"
    r"next\s+question|another\s+question|"
    r"بس\s+بالنسب[ةه]\b|بالنسبة\s+(?:ل|لل)|speaking\s+of\b|anyway\b|"
    r"by\s+the\s+way\b|على\s+فكرة\b"
)

OPEN_DOUBT = re.compile(
    r"(?i)(?:\bopen\s+doubts?\b(?!\s*[:：]?\s*(?:none|n/?a|—|-)\b))"
    r"|(?:\bself-?flag(?:ged)?\s+doubt\b)"
    r"|(?:\bask\s+(?:the\s+)?coach\b)"
    r"|(?:\bneeds?\s+coach\s+(?:to\s+)?(?:decide|decision|call)\b)"
    r"|(?:\bnot\s+sure\s+(?:if|whether|about)\b)"
    r"|(?:\bdoubt:\s*\S)"
    r"|(?:\bunresolved\s+doubt\b)"
    r"|(?:محتاج\s*(?:رأي|الكوتش|coach))"
)


def _answer_beat_problems(text: str, min_chars: int = 40) -> list[str]:
    """Return FAIL reasons when post-question text has no real answer beat.

    Length-after-strip alone is too weak (U9 row9: dodge + gov/OpenAI pivot +
    «السؤال اللي بعديه» still looked long). Fail when:
    - a dodge appears and the post-dodge tail is a topic/next-question pivot, or
    - after stripping dodge + pivot markers, too little remains.
    """
    problems: list[str] = []
    dodge = ANSWER_DODGE.search(text)
    if dodge:
        after_dodge = text[dodge.end():]
        if ANSWER_PIVOT.search(after_dodge) or ANSWER_PIVOT.search(text):
            problems.append(
                "dodge plus topic/next-question pivot after the question — "
                "no answer beat; drop/skip the row; do not render"
            )
            return problems
    cleaned = ANSWER_DODGE.sub(" ", text)
    cleaned = ANSWER_PIVOT.sub(" ", cleaned)
    core = re.sub(r"\s+", " ", cleaned).strip(" \n\t.,;:!?؟،-")
    if len(core) < min_chars:
        problems.append(
            "no answer beat after question (dodge/pivot only or empty) — "
            "drop/skip the row; do not render"
        )
    return problems


def cmd_qaanswer(qc, args) -> int:
    """Pre-render G14: type=qa must show a real answer beat in the transcript; else drop."""
    problems: list[str] = []
    if args.after:
        problems.extend(_answer_beat_problems(
            Path(args.after).read_text(encoding="utf-8"), min_chars=args.min_chars
        ))
    if args.span:
        span = json.loads(Path(args.span).read_text(encoding="utf-8"))
        beat = span.get("answer_beat", span.get("answer_beat_s", span.get("answerBeat")))
        if beat is None or beat == "" or beat is False:
            problems.append(
                "span/BRIEF missing answer_beat timestamp/note — record it before render "
                "(or drop the row)"
            )
    if not args.after and not args.span:
        raise qc.Skip("pass --after and/or --span")
    return qc.report(
        "qaanswer", qc.FAIL if problems else qc.PASS,
        problems=problems,
        note="G14 pre-flight: confirm answer beat in transcript before render; else drop",
    )


def cmd_opendoubt(qc, args) -> int:
    """G15: self-flagged doubts in SELF-REVIEW must be fixed before Coach handoff."""
    text = Path(args.file).read_text(encoding="utf-8")
    hits = []
    for m in OPEN_DOUBT.finditer(text):
        # Allow explicit cleared lines like "Open doubts: none"
        line = text[max(0, m.start() - 40): m.end() + 40]
        if re.search(r"(?i)open\s+doubts?\s*[:：]?\s*(?:none|n/?a|—|-)\b", line):
            continue
        if re.search(r"(?i)no\s+(?:open\s+)?doubts?\b", line):
            continue
        hits.append(f"self-flagged doubt still open near: {m.group(0)!r} — fix before handoff")
    # ## Open doubts section with real content
    sec = re.search(
        r"(?im)^##\s+open\s+doubts?\s*\n(.*?)(?=^##\s|\Z)", text, re.S
    )
    if sec:
        body = sec.group(1).strip()
        if body and not re.fullmatch(r"(?i)(?:none|n/?a|—|-|\*\*none\*\*)\.?", body):
            if not re.search(r"(?i)\bnone\b", body) or len(body) > 40:
                hits.append("## Open doubts still lists items — fix before handoff")
    return qc.report(
        "opendoubt", qc.FAIL if hits else qc.PASS,
        hits=hits,
        note="G15: do not hand a self-flagged script/lexicon doubt to Coach to decide",
    )



META_TITLE_RE = re.compile(
    r"^## Title[^\n]*\n\n(?P<title>.+)\n",
    re.MULTILINE,
)
META_DESC_RE = re.compile(
    r"^## Description[^\n]*\n\n(?P<body>(?:.+\n){3}.+)\n?\Z",
    re.MULTILINE,
)
# G16 line-1 prefixes by Shorts type (L22). Default remains qa for back-compat.
META_LINE1_PREFIXES = {
    "qa": "مقطع من الـ Q&A: ",
    "viral": "مقطع: ",
}
META_LINE1_PREFIX = META_LINE1_PREFIXES["qa"]
META_LINE2_RE = re.compile(
    r"^الإجابة كاملة: https://www\.youtube\.com/watch\?v=[A-Za-z0-9_-]+&t=\d+(?:\.\d+)?s$"
)


def cmd_metadatadesc(qc, args) -> int:
    """G16: METADATA description is exactly 4 lines matching the Shorts template.

    Project site URL and hashtag line are passed in (--site / --tags); never
    hard-coded here (generic placeholders only). --type qa|viral selects the
    line-1 prefix (L22 ear-lock templates).
    """
    text = Path(args.file).read_text(encoding="utf-8")
    tm = META_TITLE_RE.search(text)
    dm = META_DESC_RE.search(text)
    errors: list[str] = []
    title = tm.group("title").strip() if tm else ""
    kind = getattr(args, "type", None) or "qa"
    prefix = META_LINE1_PREFIXES.get(kind, META_LINE1_PREFIXES["qa"])
    if not tm:
        errors.append("title_block_missing")
    if not dm:
        errors.append("desc_block_missing")
        return qc.report(
            "metadatadesc", qc.FAIL, errors=errors, title=title, lines=[], gate="G16",
            type=kind,
        )
    body_lines = [ln.rstrip() for ln in dm.group("body").splitlines() if ln.strip()]
    if len(body_lines) != 4:
        errors.append(f"desc_line_count={len(body_lines)} want 4")
    lines = (body_lines + [""] * 4)[:4]
    if not lines[0].startswith(prefix):
        errors.append("line1_prefix")
    elif title and lines[0] != prefix + title:
        errors.append("line1_title_mismatch")
    if not META_LINE2_RE.match(lines[1]):
        errors.append("line2_watch_url")
    if lines[2] != args.site:
        errors.append("line3_site")
    if lines[3] != args.tags:
        errors.append("line4_hashtags")
    return qc.report(
        "metadatadesc",
        qc.FAIL if errors else qc.PASS,
        errors=errors,
        title=title,
        lines=lines,
        gate="G16",
        type=kind,
        prefix=prefix,
        note="4-line desc template; --type qa|viral; pass --site/--tags for project fill-ins",
    )


def cmd_hookmodality(qc, args) -> int:
    """Hook card must keep the speaker's modality (question ≠ claim) for every hook type.

    U9 G14: with --type qa (or auto when spoken is a question) and --after, also require
    a real answer beat in the post-question transcript before render.
    """
    hook = Path(args.hook).read_text(encoding="utf-8").strip()
    spoken = Path(args.spoken).read_text(encoding="utf-8").strip()
    hook_q, spoken_q = _is_question(hook), _is_question(spoken)
    problems = []
    if spoken_q and not hook_q:
        problems.append("spoken is a question but hook reads as a claim (add ؟/? / interrogative)")
    if hook_q and not spoken_q:
        problems.append("hook is a question but spoken is a claim (drop ؟/? or match claim modality)")
    want_answer = args.type == "qa" or (args.type == "auto" and spoken_q)
    if want_answer:
        if args.after:
            problems.extend(_answer_beat_problems(
                Path(args.after).read_text(encoding="utf-8"), min_chars=args.min_chars
            ))
        elif args.type == "qa":
            problems.append(
                "type=qa requires --after post-question transcript (G14 pre-render answer beat)"
            )
    return qc.report(
        "hookmodality", qc.FAIL if problems else qc.PASS,
        hook_question=hook_q, spoken_question=spoken_q, type=args.type, problems=problems,
        note="modality match is never N/A — viral claim↔claim and qa question↔؟; "
             "type=qa also gates answer beat via --after",
    )


def cmd_hookbleed(qc, args) -> int:
    """OUT SRT must not keep under-hook speech past hook_end (split cues at the card edge)."""
    cues = qc.parse_cues(Path(args.srt).read_text(encoding="utf-8"))
    hits = []
    pad = args.pad
    for i, cue in enumerate(cues, 1):
        # Cue that still holds under-hook words while extending well past the card.
        if cue["start"] < args.hook_end and cue["end"] > args.hook_end + pad:
            hits.append(
                f"cue {i}: {cue['start']:.2f}-{cue['end']:.2f}s straddles hook_end "
                f"{args.hook_end:.2f}s (split so under-hook ends ≤ hook_end+{pad:.1f})"
            )
    return qc.report(
        "hookbleed", qc.FAIL if hits else qc.PASS,
        hook_end=args.hook_end, pad=pad, hits=hits,
    )


def cmd_softjump(qc, args) -> int:
    """Big content jumps (long removed bridges) need a soft transition flag in the edit plan."""
    plan = json.loads(Path(args.file).read_text(encoding="utf-8"))
    cuts = plan.get("cuts") or plan.get("splices") or []
    hits = []
    for i, cut in enumerate(cuts, 1):
        removed = float(cut.get("removed_s") or cut.get("removed") or 0)
        soft = bool(cut.get("soft") or cut.get("soft_transition") or cut.get("punch_in")
                    or cut.get("crossfade"))
        if removed > args.min_removed + 1e-9 and not soft:
            hits.append(
                f"cut {i}: removed {removed:.2f}s without soft transition "
                f"(crossfade / punch-in / soft flag)"
            )
    return qc.report(
        "softjump", qc.FAIL if hits else qc.PASS,
        min_removed=args.min_removed, hits=hits,
        note="prefer keeping 1–2 bridge sentences; if you must jump, soft-mask",
    )


# Arabic binder pronouns / demonstratives that open a land cue weakly (G17).
# Opener class must match binder class: هو/هم → --noun-ho; هي/دي/ده/… → --noun.
# Project nouns stay on flags (Automatest lists live in brain).
WEAK_OPEN = frozenset({"هو", "هي", "هم", "هن", "ده", "دي", "دا", "دول", "هذا", "هذه"})
HO_OPEN = frozenset({"هو", "هم", "هذا"})  # masculine / "he" class


def _norm_tok(word: str) -> str:
    word = re.sub("[\u064b-\u0652\u0640]", "", word)
    word = word.translate(str.maketrans("أإآىة", "ااايه"))
    return word.strip(".,!?;:\"'()[]")


def _toks(text: str) -> list[str]:
    return [t for t in (_norm_tok(x) for x in text.split()) if t]


def _parse_splices(spec: str) -> list[tuple[float, float]]:
    cuts = []
    for part in spec.split(","):
        part = part.strip()
        if not part:
            continue
        a, _, b = part.partition("-")
        cuts.append((float(a), float(b)))
    return sorted(cuts)


def cmd_antecedent(qc, args) -> int:
    """G13 kept-span epithets, or G17 per-splice land openers that need a class-matched prior binder."""
    nouns = [n.strip() for n in (args.noun or []) if n.strip()]
    nouns_ho = [n.strip() for n in (getattr(args, "noun_ho", None) or []) if n.strip()]
    if getattr(args, "splice", None):
        # G17: pre-cut SRT + splice list; land opener binds only to matching class in kept-prior.
        if not nouns and not nouns_ho:
            raise qc.Skip("pass --noun and/or --noun-ho (class-matched binders for splice lands)")
        cues = cue_list(qc, args.file)
        cuts = _parse_splices(args.splice)
        hits, lands = [], []
        she_norms = {_norm_tok(n) for n in nouns}
        ho_norms = {_norm_tok(n) for n in nouns_ho}
        for start, end in cuts:
            land = next((c for c in cues if c["start"] >= end - 0.05), None)
            if land is None:
                hits.append(f"splice {start}-{end}: no land cue")
                continue
            land_text = " ".join(land["lines"])
            opener = (_toks(land_text) or [""])[0]
            prior = " ".join(" ".join(c["lines"]) for c in cues if c["end"] <= start + 1e-6)
            prior_set = set(_toks(prior))
            if opener not in WEAK_OPEN:
                lands.append({"cut": [start, end], "opener": opener, "bind": "not-weak-opener", "ok": True})
                continue
            if opener in HO_OPEN:
                ok = bool(prior_set & ho_norms)
                why = "ho-binder-in-prior" if ok else "ho-no-binder"
            else:
                ok = bool(prior_set & she_norms)
                why = "she-binder-in-prior" if ok else "she-no-binder"
            lands.append({"cut": [start, end], "opener": opener, "bind": why, "ok": ok,
                          "land_cue": land_text, "prior": prior})
            if not ok:
                hits.append(
                    f"splice {start}-{end}: land opens with dangling «{opener}» — {why} "
                    f"(cue {land['start']:.2f}-{land['end']:.2f}: {land_text!r})"
                )
        return qc.report(
            "antecedent", qc.FAIL if hits else qc.PASS,
            gate="G17", nouns=nouns, nouns_ho=nouns_ho, hits=hits, lands=lands,
            note="per-splice land opener; --noun = she-class, --noun-ho = هو-class "
                 "(prior cues only; same-cue Latin does not bind)",
        )
    # G13: whole kept span
    text = Path(args.file).read_text(encoding="utf-8")
    epithets = [e.strip() for e in (args.epithet or []) if e.strip()]
    if not epithets:
        raise qc.Skip("pass --epithet at least once (or --splice for G17)")
    hits = []
    for epi in epithets:
        if epi not in text:
            continue
        pos = text.find(epi)
        window = text if args.anywhere else text[: pos + len(epi)]
        if not any(noun in window for noun in nouns):
            hits.append(f"epithet {epi!r} with no antecedent noun in kept span "
                        f"(pass --noun for referents that must remain)")
    return qc.report(
        "antecedent", qc.FAIL if hits else qc.PASS,
        epithets=epithets, nouns=nouns, hits=hits,
    )


# Generic Arabic / Latin fillers skipped by overlay verbatim (G18).
OVERLAY_FILLER = frozenset("يعني طيب تمام اه آه ايوه أيوه يا فندم بقى و ف ب ل".split())


def _overlay_norm(word: str) -> str:
    word = re.sub("[\u064b-\u0652\u0640\u2066-\u2069]", "", word)
    word = word.translate(str.maketrans("أإآىة", "ااايه"))
    word = re.sub(r"^[^\w]+|[^\w]+$", "", word, flags=re.UNICODE)
    word = word.lower()
    if len(word) > 3:
        word = re.sub(r"^(و|ف|ب|ك)?(ال|لل)", "", word)
    return word


def _overlay_tokens(text: str) -> list[tuple[str, str]]:
    out = []
    for raw in text.replace("|", " ").split():
        raw = raw.strip()
        if not raw:
            continue
        n = _overlay_norm(raw)
        if n and n not in OVERLAY_FILLER:
            out.append((raw, n))
    return out


def _spoken_bag(payload) -> list[str]:
    """Normalize words.json (list / {words:[{w|word}]} / words_of shapes) to overlay norms."""
    bag: list[str] = []
    entries = words_of(payload)
    if entries:
        for w in entries:
            n = _overlay_norm(w["word"])
            if n:
                bag.append(n)
        return bag
    if isinstance(payload, list):
        for item in payload:
            tok = item.get("w", item.get("word", item)) if isinstance(item, dict) else item
            n = _overlay_norm(str(tok))
            if n:
                bag.append(n)
    elif isinstance(payload, dict):
        for item in payload.get("words", payload.get("tokens", [])):
            tok = item.get("w", item.get("word", item)) if isinstance(item, dict) else item
            n = _overlay_norm(str(tok))
            if n:
                bag.append(n)
    return bag


def cmd_overlayverbatim(qc, args) -> int:
    """G18: every on-screen overlay string must be verbatim-spoken (words.json bag)."""
    import difflib

    payload = json.loads(Path(args.words).read_text(encoding="utf-8"))
    bag = _spoken_bag(payload)
    surfaces: list[tuple[str, str]] = []
    if (args.short or "").strip():
        surfaces.append(("short", args.short.strip()))
    for i, part in enumerate([p.strip() for p in (args.hook or "").split("|") if p.strip()], 1):
        surfaces.append((f"hook[{i}]", part))
    for i, cue in enumerate(cue_list(qc, args.srt), 1):
        surfaces.append((f"srt[{i}]", " ".join(cue["lines"])))

    need = float(args.threshold)
    errors, checked = [], []
    for label, text in surfaces:
        miss = []
        for raw, n in _overlay_tokens(text):
            hit = n in bag or max(
                (difflib.SequenceMatcher(None, n, s).ratio() for s in bag), default=0.0
            ) >= need
            if not hit:
                miss.append(raw)
        checked.append({"surface": label, "text": text, "miss": miss})
        if miss:
            errors.append(f"{label}: invented/not-spoken {miss} ← {text!r}")
    return qc.report(
        "overlayverbatim", qc.FAIL if errors else qc.PASS,
        gate="G18", errors=errors, checked=checked, spoken_n=len(bag),
        note="hook/short/SRT tokens must match spoken words (exact or fuzzy ≥ threshold)",
    )



# Baseline ASR ه-for-ة / listed noun forms (G19). Extend via --fix AR=CORRECT.
ASR_ORTHO_FIX = {
    "حاجه": "حاجة",
    "وضعيه": "وضعية",
    "السفنجه": "السفنجة",
    "سفنجه": "سفنجة",
    "الشغلانه": "الشغلانة",
    "شغلانه": "شغلانة",
    "مشكله": "مشكلة",
    "فكره": "فكرة",
    "كلمه": "كلمة",
    "نتيجه": "نتيجة",
    "مهمه": "مهمة",
    "علاقه": "علاقة",
    "مطبقه": "مطبقة",
    "موجوده": "موجودة",
    "خاصه": "خاصة",
    "عامه": "عامة",
    "جيده": "جيدة",
    "سيئه": "سيئة",
    "طريقه": "طريقة",
    "قيمه": "قيمة",
    "خبره": "خبرة",
    "قائمه": "قائمة",
    "منصه": "منصة",
    "شركه": "شركة",
    "استراتيجيه": "استراتيجية",
    "اولويه": "أولوية",
    "اولوية": "أولوية",
}

ORTHO_TOK = re.compile(r"[A-Za-z0-9_+'#-]+|[\u0600-\u06FF\u064B-\u065F]+")


def _ortho_map(extra: list[str] | None = None) -> dict[str, str]:
    mapping = dict(ASR_ORTHO_FIX)
    for item in extra or []:
        if "=" not in item:
            continue
        bad, good = item.split("=", 1)
        bad, good = bad.strip(), good.strip()
        if bad and good:
            mapping[bad] = good
    return mapping


def cmd_orthography(qc, args) -> int:
    """G19: ASR orthography audit on title + short + hook + every SRT cue (ه→ة etc.)."""
    mapping = _ortho_map(getattr(args, "fix", None))
    surfaces: list[tuple[str, str]] = []
    if (args.title or "").strip():
        surfaces.append(("title", args.title.strip()))
    if (args.short or "").strip():
        surfaces.append(("short", args.short.strip()))
    for i, part in enumerate([p.strip() for p in (args.hook or "").split("|") if p.strip()], 1):
        surfaces.append((f"hook[{i}]", part))
    for i, cue in enumerate(cue_list(qc, args.srt), 1):
        surfaces.append((f"srt[{i}]", " ".join(cue["lines"])))

    errors, checked = [], []
    for name, text_s in surfaces:
        hits = []
        for word in ORTHO_TOK.findall(text_s.replace("—", " ").replace("|", " ")):
            if word in mapping:
                hits.append({"bad": word, "fix": mapping[word]})
        checked.append({"surface": name, "text": text_s, "hits": hits})
        for hit in hits:
            errors.append(f"{name}: ASR «{hit['bad']}» → «{hit['fix']}» in {text_s!r}")
    return qc.report(
        "orthography", qc.FAIL if errors else qc.PASS,
        gate="G19", errors=errors, checked=checked,
        note="fix taa marbuta / listed ASR nouns only; keep spoken colloquial form",
    )


# G21 title polarity / negation lemmas (Coach U13 REJECT).
TITLE_NEG_LEMMAS = {"مافيش", "مفيش", "مش", "ما", "لسه", "not", "no"}
TITLE_REL_AFTER = {
    "قالوا", "قال", "كانت", "كان", "كنت", "يكون", "تكون", "فى", "في",
    "هو", "هي", "هم", "احنا", "إحنا", "انت", "إن", "ان",
}
TITLE_SEG_SPLIT = re.compile(r"\s*[—–\-]\s*")
TITLE_META_RE = re.compile(r"^## Title[^\n]*\n\n(?P<title>.+)\n", re.MULTILINE)
TITLE_PUNCT = re.compile(r"[.,!?;:؟،؛\"\'()\[\]{}…]+")


def _title_fold_tok(word: str) -> str:
    word = TITLE_PUNCT.sub("", word.replace("\u00a0", " ").replace("\u0640", "")).strip()
    word = word.translate(str.maketrans("أإآىة", "ااايه"))
    if re.search(r"[A-Za-z]", word):
        word = word.lower()
    if word.startswith("و") and len(word) > 2 and word[1:] in TITLE_NEG_LEMMAS:
        word = word[1:]
    return word


def _title_tokenize(text: str) -> list[str]:
    return [t for t in (_title_fold_tok(x) for x in text.replace("|", " ").split()) if t]


def _title_neg_in_tokens(toks: list[str]) -> set[str]:
    out: set[str] = set()
    for i, tok in enumerate(toks):
        lem = tok if tok in TITLE_NEG_LEMMAS else None
        if not lem:
            continue
        if lem == "ما":
            nxt = toks[i + 1] if i + 1 < len(toks) else ""
            if nxt in TITLE_REL_AFTER:
                continue
        out.add(lem)
    return out


def _title_content_tokens(toks: list[str]) -> set[str]:
    neg = _title_neg_in_tokens(toks)
    return {t for t in toks if t not in neg and t not in TITLE_NEG_LEMMAS and len(t) > 1}


def _title_from_metadata(path: Path) -> str:
    text = path.read_text(encoding="utf-8")
    match = TITLE_META_RE.search(text)
    if not match:
        raise ValueError(f"no ## Title in {path}")
    return match.group("title").strip()



def _title_kept_covers(span: tuple[float, float], cuts: list[tuple[float, float]]) -> bool:
    """True iff [s,e] lies in kept speech (no overlap with any cut)."""
    start, end = span
    if end < start:
        return False
    for a, b in cuts:
        if min(end, b) - max(start, a) > 1e-6:
            return False
    return True


def _title_ordered_match_span(seg_toks: list[str], words: list[dict]) -> tuple[float, float] | None:
    """Minimal [start,end] covering an ordered match of content tokens in words."""
    need = [t for t in seg_toks if t not in TITLE_NEG_LEMMAS and len(t) > 1]
    if not need:
        return None
    wi = 0
    first_s = None
    last_e = None
    for w in words:
        tok = _title_fold_tok(str(w.get("word") or w.get("w") or ""))
        if not tok:
            continue
        if tok == need[wi]:
            if first_s is None:
                first_s = float(w["start"] if w.get("start") is not None else w["s"])
            last_e = float(w["end"] if w.get("end") is not None else w["e"])
            wi += 1
            if wi == len(need):
                return (first_s, last_e)
    return None


def cmd_titlepolarity(qc, args) -> int:
    """G21/G22: title polarity + claim binds to kept-speech clause (not cut span)."""
    title = (args.title or "").strip()
    if getattr(args, "metadata", None):
        title = _title_from_metadata(Path(args.metadata))
    if not title:
        return qc.report(
            "titlepolarity", qc.USAGE,
            gate="G21",
            errors=["pass --title or --metadata with a non-empty title"],
            note="fail-closed: empty title/metadata is error (exit 2), not skip",
        )
    cues = cue_list(qc, args.srt)
    words = None
    if getattr(args, "words", None):
        words = words_of(json.loads(Path(args.words).read_text(encoding="utf-8")))
    cuts = _parse_splices(args.splice) if getattr(args, "splice", None) else []
    errors, checked = [], []
    title_toks = _title_tokenize(title)
    title_negs = _title_neg_in_tokens(title_toks)
    segments = [s.strip() for s in TITLE_SEG_SPLIT.split(title) if s.strip()] or [title]
    g22 = words is not None

    for si, seg in enumerate(segments, 1):
        seg_toks = _title_tokenize(seg)
        seg_negs = _title_neg_in_tokens(seg_toks)
        content = _title_content_tokens(seg_toks)
        if not content:
            checked.append(
                {"segment": si, "text": seg, "content": [], "required_negs": [], "ok": True}
            )
            continue
        required: set[str] = set()
        hit_cues: list[str] = []
        for cue in cues:
            text_c = " ".join(cue["lines"])
            ct = _title_tokenize(text_c)
            if not (content & _title_content_tokens(ct)):
                continue
            cue_negs = _title_neg_in_tokens(ct)
            hit_cues.append(text_c)
            if cue_negs:
                required |= cue_negs
        missing = sorted(required - seg_negs)
        ok = not missing
        row = {
            "segment": si, "text": seg, "content": sorted(content),
            "required_negs": sorted(required), "title_negs": sorted(title_negs),
            "source_cues": hit_cues, "missing": missing, "ok": ok,
        }
        if g22:
            # words_of entries use word/start/end
            span = _title_ordered_match_span(seg_toks, [
                {"word": w["word"], "w": w["word"], "start": w["start"], "s": w["start"],
                 "end": w["end"], "e": w["end"]}
                for w in words
                if w.get("word") and w.get("start") is not None and w.get("end") is not None
            ])
            row["clause_span"] = list(span) if span else None
            row["cuts"] = [list(c) for c in cuts]
            if span is None:
                ok = False
                errors.append(
                    f"segment[{si}] {seg!r}: G22 no ordered kept-speech match "
                    f"for content tokens {sorted(content)}"
                )
            elif not _title_kept_covers(span, cuts):
                ok = False
                errors.append(
                    f"segment[{si}] {seg!r}: G22 title clause span {span} "
                    f"overlaps cut or is not ⊆ kept spans (cuts={cuts})"
                )
            row["ok"] = ok
        checked.append(row)
        if missing:
            errors.append(
                f"segment[{si}] {seg!r}: polarity flip — source negation {missing} "
                f"not preserved in title (cues: {hit_cues})"
            )
    note = (
        "G21+G22: segment-local negation; title clause ordered match ⊆ kept spans"
        if g22 else
        "segment-local negation; first-half مافيش does not license a positive second claim; "
        "G22: pass --words + --splice to bind claim to kept speech"
    )
    return qc.report(
        "titlepolarity", qc.FAIL if errors else qc.PASS,
        gate="G21+G22" if g22 else "G21", title=title, title_negs=sorted(title_negs),
        errors=errors, checked=checked, note=note,
    )




def cmd_textfloor(qc, args) -> int:
    """Text under 2.6% of the frame's short side; nothing read under 28 px on portrait."""
    spec = json.loads(Path(args.file).read_text(encoding="utf-8"))
    width, height = (int(v) for v in str(spec.get("frame", args.frame)).lower().split("x"))
    floor = max(args.ratio * min(width, height), args.portrait_px if height > width else 0)
    hits = [f"{b.get('id', '?')}: {b['size']}px under {floor:.1f}px" for b in spec["boxes"]
            if b.get("kind", "text") != "decoration" and float(b["size"]) < floor - 1e-6]
    return qc.report("textfloor", qc.FAIL if hits else qc.PASS, floor_px=round(floor, 1), hits=hits)


def cmd_mediaclamp(qc, args) -> int:
    """Last cue or card ends after the media: the render ends in black."""
    duration = float(qc.ffprobe(args.media).get("format", {}).get("duration", 0) or 0)
    cues = cue_list(qc, args.cues)
    hits = [f"cue {i} ends {c['end']:.3f}s after media {duration:.3f}s" for i, c in enumerate(cues, 1)
            if c["end"] > duration + args.tolerance]
    return qc.report("mediaclamp", qc.FAIL if hits else qc.PASS, media_s=round(duration, 3), hits=hits)


def cmd_artrack(qc, args) -> int:
    """Letter-spacing on Arabic text breaks the joins: it stays normal."""
    hits = []
    spaced = re.compile(r"letter-spacing\s*:(?!\s*(?:normal|0(?:\.0+)?(?:em|px|rem)?\s*(?:[;}\"']|$)))\s*[^;}\"'\s][^;}\"']*", re.I)
    for file in files_of(args.path, {".css", ".html", ".htm", ".svg", ".json"}):
        text = file.read_text(encoding="utf-8", errors="replace")
        for selector, body, _hover, _line in css_rules(text):
            if re.search(r":lang\(ar|\[lang=['\"]?ar|\[dir=['\"]?rtl|arab", selector, re.I) and spaced.search(body):
                hits.append(f"{file}: {selector.strip()[:60]} letter-spacing on Arabic")
        for match in re.finditer(r"<[^>]*style=['\"]([^'\"]*)['\"][^>]*>([^<]*)", text):
            if spaced.search(match.group(1) + ";") and ARABIC.search(match.group(2)):
                hits.append(f"{file}:{text.count(chr(10), 0, match.start()) + 1} inline letter-spacing on Arabic")
        if file.suffix == ".json":
            for node in re.finditer(r"\{[^{}]*\}", text):
                block = node.group(0)
                if ARABIC.search(block) and re.search(r"\"(letter_?spacing|letterSpacing|tracking)\"\s*:\s*(-?[1-9]|-?0?\.\d*[1-9])", block):
                    hits.append(f"{file}:{text.count(chr(10), 0, node.start()) + 1} tracking on Arabic text")
    return qc.report("artrack", qc.FAIL if hits else qc.PASS, hits=hits)


def cmd_pptxbidi(qc, args) -> int:
    """PPTX Arabic: native rtl="1" per paragraph, no Unicode bidi controls in text."""
    import zipfile
    hits, warnings = [], []
    with zipfile.ZipFile(args.file) as deck:
        names = sorted(n for n in deck.namelist() if re.match(r"ppt/(slides|notesSlides|slideLayouts)/[^/]+\.xml$", n))
        for name in names:
            xml = deck.read(name).decode("utf-8", errors="replace")
            for number, para in enumerate(re.findall(r"<a:p>.*?</a:p>|<a:p .*?</a:p>", xml, re.S), 1):
                text = "".join(re.findall(r"<a:t>([^<]*)</a:t>", para))
                if BIDI_CONTROLS.search(text):
                    hits.append(f"{name} p{number}: Unicode bidi control in PPTX text (use native rtl)")
                if not ARABIC.search(text):
                    continue
                if not re.search(r"<a:pPr\b[^>]*\brtl=\"1\"", para):
                    hits.append(f"{name} p{number}: Arabic paragraph without <a:pPr rtl=\"1\">")
                if re.match(r"\s*[\u2022\u25cf\-\*]\s", text):
                    hits.append(f"{name} p{number}: typed bullet; use native bullets")
                if 'lang="ar' not in para:
                    warnings.append(f"{name} p{number}: Arabic runs without lang ar-*")
    return qc.report("pptxbidi", qc.FAIL if hits else qc.PASS, slides=len(names), hits=hits, warnings=warnings)



def _tok_alnum(s: str) -> str:
    return re.sub(r"[^\w\u0600-\u06FF]", "", s or "")


def _srt_cue_lines(text: str) -> list[tuple[int, str]]:
    """Return (lineno, cue-text) pairs, skipping index and timing rows."""
    out: list[tuple[int, str]] = []
    for i, line in enumerate(text.splitlines(), 1):
        s = line.strip()
        if not s or re.match(r"^\d+$", s) or "-->" in s:
            continue
        out.append((i, line))
    return out


def _name_hits_in_line(line: str, names: set[str], vocative_only: set[str]) -> list[str]:
    hits: list[str] = []
    for match in re.finditer(r"(?<![\w\u0600-\u06FF])(?:ياا?)\s+([\w\u0600-\u06FF]+)", line):
        name = _tok_alnum(match.group(1))
        if name in names:
            hits.append(name)
    for name in names - vocative_only:
        if re.search(rf"(?<![\w\u0600-\u06FF]){re.escape(name)}(?![\w\u0600-\u06FF])", line):
            if name not in hits:
                hits.append(name)
    return hits


def _load_asr_words(path: Path) -> list[dict]:
    data = json.loads(path.read_text(encoding="utf-8"))
    if isinstance(data, list):
        return data
    if isinstance(data, dict):
        if isinstance(data.get("words"), list):
            return data["words"]
        segs = data.get("segments") or []
        words: list[dict] = []
        for seg in segs:
            words.extend(seg.get("words") or [])
        return words
    return []


DEFAULT_AMOUNT_RE = re.compile(
    r"(\d[\d,]*\s*(?:USD|EUR|EGP|\$)|(?:USD|EUR|EGP)\s*\d)",
    re.I,
)



def _asr_words_payload_ok(data) -> bool:
    """True when words payload is a non-empty list (or dict with non-empty words list)."""
    if data is None:
        return False
    if isinstance(data, list):
        return len(data) > 0
    if isinstance(data, dict):
        words = data.get("words")
        if isinstance(words, list):
            return len(words) > 0
        # segments-only with nested words still counts if any word present
        segs = data.get("segments")
        if isinstance(segs, list):
            for seg in segs:
                if isinstance(seg, dict) and isinstance(seg.get("words"), list) and seg["words"]:
                    return True
        return False
    return False


def _privacynames_require_words_fail(qc, args) -> int | None:
    """L18: check --require-words BEFORE load; missing/empty/null/{} / non-list = FAIL."""
    if not getattr(args, "require_words", False):
        return None
    note = "ASR words required in run_gates; missing ≠ pass (L18)"
    if not args.words:
        return qc.report(
            "privacynames", qc.FAIL,
            hits=["missing words.json (--require-words)"],
            note=note,
        )
    words_path = Path(args.words)
    if not words_path.exists():
        return qc.report(
            "privacynames", qc.FAIL,
            hits=["missing words.json (--require-words)"],
            note=note,
        )
    try:
        raw = words_path.read_text(encoding="utf-8").strip()
    except OSError:
        return qc.report(
            "privacynames", qc.FAIL,
            hits=["missing words.json (--require-words)"],
            note=note,
        )
    if not raw:
        return qc.report(
            "privacynames", qc.FAIL,
            hits=["empty words.json (--require-words)"],
            note=note,
        )
    try:
        data = json.loads(raw)
    except json.JSONDecodeError:
        return qc.report(
            "privacynames", qc.FAIL,
            hits=["empty words.json (--require-words)"],
            note=note,
        )
    if not _asr_words_payload_ok(data):
        return qc.report(
            "privacynames", qc.FAIL,
            hits=["empty words.json (--require-words)"],
            note=note,
        )
    return None


def cmd_privacynames(qc, args) -> int:
    """L09: caption + ASR personal-name / hard-amount scan; mute notes alone never PASS.

    Project denylist via --name (repeatable). --vocative-only marks tokens that
    only count after يا/ياا. Synthetic examples in tests use فلان / Alice.
    """
    require_fail = _privacynames_require_words_fail(qc, args)
    if require_fail is not None:
        return require_fail
    names = {_tok_alnum(n) for n in (args.name or []) if _tok_alnum(n)}
    vocative_only = {_tok_alnum(n) for n in (args.vocative_only or []) if _tok_alnum(n)}
    amount_re = re.compile(args.amount_re, re.I) if args.amount_re else DEFAULT_AMOUNT_RE
    if not names and not args.allow_empty_names:
        return qc.report(
            "privacynames", qc.USAGE,
            error="pass --name TOKEN (repeatable); project denylist stays out of CE source",
        )
    hits: list[str] = []
    scanned = False
    if args.srt:
        scanned = True
        for lineno, line in _srt_cue_lines(Path(args.srt).read_text(encoding="utf-8")):
            for name in _name_hits_in_line(line, names, vocative_only):
                hits.append(f"srt:{lineno} name={name}")
            if amount_re.search(line):
                hits.append(f"srt:{lineno} hard-amount")
    if args.words:
        scanned = True
        words = _load_asr_words(Path(args.words))
        for i, item in enumerate(words):
            w = item.get("w") or item.get("word") or ""
            tok = _tok_alnum(w)
            prev = _tok_alnum(
                (words[i - 1].get("w") or words[i - 1].get("word") or "") if i else ""
            )
            is_voc = prev in ("يا", "ياا")
            if tok in names:
                if tok in vocative_only and not is_voc:
                    continue
                hits.append(f"asr:{i} name={tok}" + (" vocative" if is_voc else ""))
            if amount_re.search(w):
                hits.append(f"asr:{i} hard-amount")
    notes_ok = True
    if args.notes:
        notes = Path(args.notes).read_text(encoding="utf-8")
        has_mute = bool(re.search(r"\bmute", notes, re.I))
        has_text_scan = bool(re.search(r"text[- ]?scan|srt[- ]?scan|asr[- ]?scan|privacynames", notes, re.I))
        if has_mute and not has_text_scan:
            notes_ok = False
            hits.append("notes:mute-without-text-scan")
    if not scanned:
        return qc.report(
            "privacynames", qc.USAGE,
            error="pass --srt and/or --words; mute notes alone never PASS",
        )
    return qc.report(
        "privacynames",
        qc.FAIL if hits or not notes_ok else qc.PASS,
        hits=hits,
        names=sorted(names),
        scanned=["srt"] * bool(args.srt) + ["asr"] * bool(args.words),
        notes_ok=notes_ok,
        note="fail-closed on residual name/amount hits; mute notes alone never PASS (L09)",
    )


def _script_direction(text: str) -> str:
    """rtl if any Arabic letter, else ltr (strong Latin/digit)."""
    if ARABIC.search(text):
        return "rtl"
    return "ltr"


def _titlepunct_blocks(metadata_path: str) -> dict[str, str]:
    text = Path(metadata_path).read_text(encoding="utf-8")
    section, buf = None, []
    blocks: dict[str, str] = {}
    for line in text.splitlines() + ["## _end"]:
        if line.startswith("## "):
            if section:
                blocks[section] = "\n".join(buf).strip()
            heading = line[3:].strip().lower()
            buf = []
            if heading.startswith("title"):
                section = "title"
            elif heading.startswith("description"):
                section = "description"
            else:
                section = None
            continue
        if section:
            buf.append(line)
    return blocks


def _titlepunct_mark_errors(label: str, raw: str, url_re, ar_q: str, lat_q: str) -> list[str]:
    if not raw or not raw.strip():
        return []
    cleaned = url_re.sub(" ", raw)
    if not cleaned.strip():
        return []
    direction = _script_direction(cleaned.rstrip("؟?"))
    expected = ar_q if direction == "rtl" else lat_q
    has_ar, has_lat = ar_q in cleaned, lat_q in cleaned
    if not has_ar and not has_lat:
        return []
    out: list[str] = []
    if expected == lat_q and has_ar:
        out.append(f"{label}: Latin-script uses Arabic ؟; use ASCII '?'")
    if expected == ar_q and has_lat:
        out.append(f"{label}: Arabic/RTL uses ASCII '?'; use Arabic '؟'")
    return out


def _titlepunct_desc_errors(
    desc: str, title_bare: str, title_exp: str | None, url_re, ar_q: str, lat_q: str,
) -> list[str]:
    errors: list[str] = []
    for i, line in enumerate(desc.splitlines(), 1):
        cleaned = url_re.sub(" ", line)
        if "؟" not in cleaned and "?" not in cleaned:
            continue
        if title_bare and title_bare in cleaned and title_exp:
            after = cleaned[cleaned.find(title_bare) + len(title_bare):]
            span = cleaned[cleaned.find(title_bare):]
            if title_exp == lat_q and (ar_q in span or after.lstrip().startswith(ar_q)):
                errors.append(f"desc:L{i}: embedded Latin title uses ؟")
            if title_exp == ar_q and lat_q in after and ar_q not in span:
                errors.append(f"desc:L{i}: embedded Arabic title uses ?")
        else:
            errors.extend(_titlepunct_mark_errors(f"desc:L{i}", cleaned, url_re, ar_q, lat_q))
    return errors


def cmd_titlepunct(qc, args) -> int:
    """L11: title/desc question mark matches script (Latin ? vs Arabic ؟)."""
    ar_q, lat_q = "؟", "?"
    url_re = re.compile(r"https?://\S+|www\.\S+", re.I)
    title = args.title or ""
    desc = args.desc or ""
    if args.metadata:
        blocks = _titlepunct_blocks(args.metadata)
        title = title or blocks.get("title", "")
        desc = desc or blocks.get("description", "")
    if not (title.strip() or desc.strip() or args.metadata):
        return qc.report(
            "titlepunct", qc.USAGE,
            error="pass --title, --desc, and/or --metadata; empty input never PASS",
        )
    errors = _titlepunct_mark_errors("title", title, url_re, ar_q, lat_q)
    title_bare = title.strip().rstrip("؟?").rstrip() if title else ""
    title_dir = _script_direction(title_bare) if title_bare else None
    title_exp = (ar_q if title_dir == "rtl" else lat_q) if title_dir else None
    errors.extend(_titlepunct_desc_errors(desc, title_bare, title_exp, url_re, ar_q, lat_q))
    errors = list(dict.fromkeys(errors))
    return qc.report(
        "titlepunct",
        qc.FAIL if errors else qc.PASS,
        errors=errors,
        title=title,
        expected_title_q=(ar_q if title_dir == "rtl" else lat_q) if title_dir else None,
        note="Latin ASCII '?'; Arabic '؟'; URLs ignored (L11)",
    )


def _band_ink_frac(rgb, y0: int, y1: int) -> float:
    """Fraction of bright (near-white) pixels in band rows — proxy for text ink.

    Returns NaN when the band is empty so callers fail closed (never 0.0 PASS).
    """
    if y0 >= y1 or y0 < 0 or y1 > rgb.shape[0]:
        return float("nan")
    region = rgb[y0:y1, :, :].astype("int16")
    if region.size == 0:
        return float("nan")
    r, g, b = region[:, :, 0], region[:, :, 1], region[:, :, 2]
    bright = (r > 200) & (g > 200) & (b > 200)
    # Exclude near-navy ground and warm gold accents
    navy = (r < 40) & (g < 80) & (b < 100)
    gold = (r > 180) & (g > 140) & (b < 180) & (r > b + 40)
    ink = bright & ~navy & ~gold
    if ink.size == 0:
        return float("nan")
    return float(ink.mean())


def cmd_endcardlive(qc, args) -> int:
    """L10: live end-card frame band-row ink vs clean endcard plate."""
    try:
        from PIL import Image
        import numpy as np
    except ImportError as exc:  # pragma: no cover - optional dep
        raise qc.Skip(f"Pillow/numpy required for endcardlive: {exc}") from exc

    clean = Path(args.clean)
    live_path = Path(args.live) if args.live else None
    if live_path is None:
        if not args.file:
            return qc.report("endcardlive", qc.USAGE, error="pass final.mp4 or --live frame")
        # Grab a frame near end of end-card hold (L16: video-stream duration + pix_fmt)
        import tempfile, subprocess
        probe = qc.ffprobe(args.file)
        vdur = None
        for stream in probe.get("streams") or []:
            if stream.get("codec_type") == "video":
                raw = stream.get("duration")
                if raw not in (None, "N/A", "n/a"):
                    try:
                        vdur = float(raw)
                    except (TypeError, ValueError):
                        vdur = None
                break
        fmt_dur = float((probe.get("format") or {}).get("duration", 0) or 0)
        dur = vdur if vdur is not None else fmt_dur
        ss = max(0.0, dur - float(args.offset_from_end))
        tmp = Path(tempfile.mkdtemp(prefix="ce-endcard-")) / "live.jpg"
        cmd = [
            qc.need("ffmpeg"), "-v", "error", "-y", "-ss", str(ss), "-i", args.file,
            "-frames:v", "1", "-update", "1", "-q:v", "2", "-pix_fmt", "yuvj420p", str(tmp),
        ]
        result = subprocess.run(cmd, capture_output=True, text=True, check=False)  # nosec B603
        if result.returncode != 0 or not tmp.exists() or tmp.stat().st_size < 1000:
            return qc.report(
                "endcardlive", qc.USAGE,
                error=f"ffmpeg failed or empty frame at ss={ss}: {(result.stderr or '')[-400:]}",
            )
        live_path = tmp

    live_im = np.array(Image.open(live_path).convert("RGB"))
    clean_im = np.array(Image.open(clean).convert("RGB"))
    y0 = int(args.band_y)
    y1 = y0 + int(args.band_h)
    live_h, clean_h = int(live_im.shape[0]), int(clean_im.shape[0])
    # Both frames must fully cover a non-empty band; never clamp into empty slice.
    if y0 < 0 or y1 <= y0 or live_h < y1 or clean_h < y1:
        return qc.report(
            "endcardlive", qc.FAIL,
            error="band out of bounds or empty; both frames must cover y0:y1",
            band=[y0, y1],
            live_h=live_h,
            clean_h=clean_h,
            live_frame=str(live_path),
            clean_png=str(clean),
        )
    live_ink = _band_ink_frac(live_im, y0, y1)
    clean_ink = _band_ink_frac(clean_im, y0, y1)
    delta = abs(live_ink - clean_ink)
    tl = live_im[0:100, 0:100] if live_im.shape[0] >= 100 and live_im.shape[1] >= 100 else live_im
    topleft_white = float(np.all(tl >= 250, axis=2).mean()) if tl.size else 0.0
    finite = all(math.isfinite(v) for v in (live_ink, clean_ink, delta, topleft_white))
    ok = (
        finite
        and live_ink <= float(args.max_live_ink)
        and delta <= float(args.max_ink_delta)
        and topleft_white < 0.05
    )
    return qc.report(
        "endcardlive",
        qc.PASS if ok else qc.FAIL,
        live_ink_frac=None if not math.isfinite(live_ink) else round(live_ink, 5),
        clean_ink_frac=None if not math.isfinite(clean_ink) else round(clean_ink, 5),
        ink_delta=None if not math.isfinite(delta) else round(delta, 5),
        max_live_ink=float(args.max_live_ink),
        max_ink_delta=float(args.max_ink_delta),
        topleft_white_frac=round(topleft_white, 4) if math.isfinite(topleft_white) else None,
        band=[y0, y1],
        live_frame=str(live_path),
        clean_png=str(clean),
        note="burn bands on content only; gate live end card vs clean plate (L10)",
    )


def _line_components(mask) -> list[tuple[int, int, int, int]]:
    import numpy as np
    cols = np.where(mask.any(axis=0))[0]
    if len(cols) == 0:
        return []
    bounds: list[tuple[int, int]] = []
    start = int(cols[0])
    for i in range(1, len(cols)):
        if int(cols[i]) - int(cols[i - 1]) > 2:
            bounds.append((start, int(cols[i - 1])))
            start = int(cols[i])
    bounds.append((start, int(cols[-1])))
    out = []
    for x0, x1 in bounds:
        ys, xs = np.where(mask[:, x0: x1 + 1])
        if len(ys) == 0:
            continue
        out.append((x0, x1, int(ys.min()), int(ys.max())))
    return out


def cmd_qmarkbaseline(qc, args) -> int:
    """L12: trailing ?/؟ ink bottom vs body baseline; fail if offset/line_h > 6%."""
    try:
        from PIL import Image
        import numpy as np
    except ImportError as exc:  # pragma: no cover
        raise qc.Skip(f"Pillow/numpy required for qmarkbaseline: {exc}") from exc

    path = Path(args.file)
    threshold = float(args.max_frac)
    im = np.array(Image.open(path).convert("RGBA"))
    mask = (im[:, :, 0] > 200) & (im[:, :, 1] > 200) & (im[:, :, 2] > 200)
    # Ignore bottom chrome (gold rule / footer) — keep upper band text
    cut = int(args.ignore_below) if args.ignore_below is not None else max(0, im.shape[0] - 40)
    if cut < im.shape[0]:
        mask[cut:, :] = False
    rows = np.where(mask.any(axis=1))[0]
    if len(rows) == 0:
        return qc.report(
            "qmarkbaseline", qc.FAIL, reason="no_text",
            path=str(path), note="blank/no-ink frame never PASS (L12)",
        )
    groups: list[tuple[int, int]] = []
    start = prev = int(rows[0])
    for r in rows[1:]:
        r = int(r)
        if r - prev > 4:
            groups.append((start, prev))
            start = r
        prev = r
    groups.append((start, prev))
    ls, le = groups[-1]
    comps = _line_components(mask[ls: le + 1])
    if len(comps) < 2:
        return qc.report(
            "qmarkbaseline", qc.PASS, reason="no_trailing_qmark",
            path=str(path), components=len(comps),
        )
    widths = [c[1] - c[0] + 1 for c in comps]
    med_w = float(np.median(widths))
    max_mark_w = max(22.0, 0.7 * med_w)
    direction = (args.direction or "").lower() or None

    def eval_side(side: str):
        q = comps[0] if side == "left" else comps[-1]
        body = comps[1:] if side == "left" else comps[:-1]
        if not body:
            return None
        qw = q[1] - q[0] + 1
        if qw > max_mark_w:
            return None
        bottoms = sorted(c[3] for c in body)
        if len(bottoms) >= 4:
            keep = bottoms[: max(1, int(round(len(bottoms) * 0.75)))]
            baseline = float(np.median(keep))
        else:
            baseline = float(np.median(bottoms))
        body_top = float(min(c[2] for c in body))
        line_h = max(1.0, baseline - body_top + 1.0)
        q_bottom = float(q[3])
        frac = abs(q_bottom - baseline) / line_h
        fail = (not math.isfinite(frac)) or frac > threshold
        return {
            "side": side, "q_bottom": q_bottom, "baseline": baseline,
            "line_h": line_h,
            "bottom_offset_frac": None if not math.isfinite(frac) else round(frac, 4),
            "fail": fail,
        }

    sides = ["right"] if direction == "ltr" else ["left"] if direction == "rtl" else ["left", "right"]
    checked = [c for s in sides if (c := eval_side(s)) is not None]
    if not checked:
        return qc.report(
            "qmarkbaseline", qc.PASS, reason="no_trailing_qmark",
            path=str(path), direction=direction, components=len(comps),
        )
    # Prefer finite fractions for worst-of ranking; non-finite always fail.
    def _rank(c):
        v = c["bottom_offset_frac"]
        return float("inf") if v is None else v
    worst = max(checked, key=_rank)
    status = qc.FAIL if any(c["fail"] for c in checked) else qc.PASS
    return qc.report(
        "qmarkbaseline", status,
        path=str(path), direction=direction, threshold=threshold,
        components=len(comps), checked=checked,
        side=worst["side"], bottom_offset_frac=worst["bottom_offset_frac"],
        baseline=worst["baseline"], line_h=worst["line_h"],
        note="mixed-font trailing ?/؟ share body baseline; fail >6% line_h (L12)",
    )




def _leak_ink_below_band(rgb, band_y: int, band_h: int) -> dict:
    """Detect text-like near-white ink below the overlay band (L14)."""
    import numpy as np
    h = int(rgb.shape[0])
    y0 = int(band_y) + int(band_h)
    if y0 >= h - 1:
        return {"status": "pass", "ink": 0, "text_rows": 0, "bbox": None}
    below = rgb[y0:h].astype("float32")
    mn, mx = below.min(axis=2), below.max(axis=2)
    bright = (mn > 200) & ((mx - mn) < 40)
    row_counts = bright.sum(axis=1)
    text_rows = int((row_counts >= 20).sum())
    ink = int(bright.sum())
    ys, xs = np.where(bright)
    bbox = None
    if len(ys):
        bbox = [int(xs.min()), int(ys.min()) + y0, int(xs.max()), int(ys.max()) + y0]
    status = "fail" if text_rows >= 3 and ink >= 200 else "pass"
    return {"status": status, "ink": ink, "text_rows": text_rows, "bbox": bbox, "band_bottom": y0}


def cmd_bandcoverage(qc, args) -> int:
    """L14: text-like ink outside burned band on sampled content frames."""
    try:
        from PIL import Image
        import numpy as np
    except ImportError as exc:  # pragma: no cover
        raise qc.Skip(f"Pillow/numpy required for bandcoverage: {exc}") from exc

    if int(args.band_h) <= 0:
        return qc.report(
            "bandcoverage", qc.FAIL,
            error="band_h must be > 0",
            band=[int(args.band_y), int(args.band_h)],
            note="invalid band never PASS (L14)",
        )

    frames: list[Path] = []
    if args.frame:
        frames = [Path(p) for p in args.frame]
    elif args.mp4:
        import tempfile, subprocess
        mp4 = Path(args.mp4)
        probe = qc.ffprobe(str(mp4))
        dur = float((probe.get("format") or {}).get("duration", 0) or 0)
        end = float(args.content_end) if args.content_end is not None else max(0.5, dur - 3.0)
        end = max(0.5, min(end, dur - 0.05)) if dur > 0.5 else 0.5
        n = max(1, int(args.n))
        times = [0.3] if n == 1 else [0.3 + i * (end - 0.3) / (n - 1) for i in range(n)]
        tmp = Path(tempfile.mkdtemp(prefix="ce-bandcov-"))
        for i, t in enumerate(times):
            out = tmp / f"f{i}.png"
            cmd = [
                qc.need("ffmpeg"), "-v", "error", "-y", "-ss", f"{t:.3f}", "-i", str(mp4),
                "-frames:v", "1", str(out),
            ]
            subprocess.run(cmd, capture_output=True, text=True, check=False)  # nosec B603
            if out.exists():
                frames.append(out)
    else:
        return qc.report("bandcoverage", qc.USAGE, error="pass --frame and/or --mp4")

    if not frames:
        return qc.report("bandcoverage", qc.FAIL, error="no frames to check", frames_checked=0)

    samples = []
    worst = None
    for frame in frames:
        rgb = np.array(Image.open(frame).convert("RGB"))
        detail = _leak_ink_below_band(rgb, int(args.band_y), int(args.band_h))
        detail["frame"] = str(frame)
        samples.append(detail)
        if worst is None or (
            (detail["status"] == "fail" and worst["status"] != "fail")
            or detail["ink"] > worst.get("ink", 0)
        ):
            worst = detail
    status = qc.FAIL if any(s["status"] == "fail" for s in samples) else qc.PASS
    return qc.report(
        "bandcoverage", status,
        frames_checked=len(samples),
        band=[int(args.band_y), int(args.band_h)],
        worst=worst,
        samples=[{"status": s["status"], "ink": s["ink"], "text_rows": s["text_rows"]} for s in samples],
        note="multi-sample ink outside band = FAIL (L14)",
    )


def check_mute_spans(
    mutes: list[tuple[float, float]] | None,
    content_dur: float,
    *,
    max_span: float = 2.0,
    max_frac: float = 0.10,
) -> dict:
    """L15: reject mute spans > max_span or total mute frac > max_frac.

    Fail-closed: content_dur NaN/<=0 or any NaN mute end → FAIL.
    """
    mutes = list(mutes or [])
    spans = []
    total = 0.0
    worst = 0.0
    reasons: list[str] = []
    try:
        content_dur_f = float(content_dur)
    except (TypeError, ValueError):
        content_dur_f = float("nan")
    if not math.isfinite(content_dur_f) or content_dur_f <= 0:
        reasons.append(f"content_dur NaN or <=0 ({content_dur!r})")
        return {
            "status": "fail",
            "max_span_s": 0.0,
            "total_mute_s": 0.0,
            "content_dur_s": None,
            "total_mute_frac": None,
            "limits": {"max_span_s": max_span, "max_frac": max_frac},
            "mutes": [],
            "reasons": reasons,
        }
    for a, b in mutes:
        try:
            a_f, b_f = float(a), float(b)
        except (TypeError, ValueError):
            reasons.append(f"NaN mute end {a!r},{b!r}")
            continue
        if not math.isfinite(a_f) or not math.isfinite(b_f):
            reasons.append(f"NaN mute end {a_f},{b_f}")
            continue
        if b_f <= a_f:
            continue
        d = b_f - a_f
        spans.append({"start": a_f, "end": b_f, "dur": round(d, 3)})
        total += d
        worst = max(worst, d)
    frac = total / content_dur_f
    if worst > max_span + 1e-6:
        reasons.append(f"max_mute_span {worst:.3f}s > {max_span}s")
    if frac > max_frac + 1e-9:
        reasons.append(f"total_mute_frac {frac:.3%} > {max_frac:.0%}")
    return {
        "status": "fail" if reasons else "pass",
        "max_span_s": round(worst, 3),
        "total_mute_s": round(total, 3),
        "content_dur_s": round(content_dur_f, 3),
        "total_mute_frac": round(frac, 4),
        "limits": {"max_span_s": max_span, "max_frac": max_frac},
        "mutes": spans,
        "reasons": reasons,
    }


def cmd_mutespan(qc, args) -> int:
    """L15: mute ≤2s/span and ≤10% of content duration."""
    mutes: list[tuple[float, float]] = []
    if args.mute:
        for item in args.mute:
            a, b = item.split(",", 1)
            mutes.append((float(a), float(b)))
    if args.mutes:
        raw = json.loads(Path(args.mutes).read_text(encoding="utf-8"))
        if isinstance(raw, dict) and "mutes" in raw:
            raw = raw["mutes"]
        for m in raw:
            if isinstance(m, (list, tuple)) and len(m) >= 2:
                mutes.append((float(m[0]), float(m[1])))
            elif isinstance(m, dict) and "start" in m:
                mutes.append((float(m["start"]), float(m["end"])))
    if args.content_dur is None:
        return qc.report("mutespan", qc.USAGE, error="pass --content-dur seconds")
    report = check_mute_spans(
        mutes, float(args.content_dur),
        max_span=float(args.max_span), max_frac=float(args.max_frac),
    )
    return qc.report(
        "mutespan",
        qc.FAIL if report["status"] == "fail" else qc.PASS,
        **{k: v for k, v in report.items() if k != "status"},
        note="mute ≤2s/span and ≤10% content; else recut (L15)",
    )


def check_viral_pacing(
    content_dur: float,
    punchline_at: float,
    *,
    type_: str = "viral",
    threshold: float = 0.65,
) -> dict:
    """L21: late punchline on type=viral → FLAG (advisory), never FAIL.

    punchline_rel = punchline_at / content_dur. FLAG when type=viral and
    punchline_rel > threshold. Non-viral types PASS (N/A). Bad inputs → error.
    """
    reasons: list[str] = []
    try:
        content_dur_f = float(content_dur)
    except (TypeError, ValueError):
        content_dur_f = float("nan")
    try:
        punchline_f = float(punchline_at)
    except (TypeError, ValueError):
        punchline_f = float("nan")
    if not math.isfinite(content_dur_f) or content_dur_f <= 0:
        reasons.append(f"content_dur NaN or <=0 ({content_dur!r})")
    if not math.isfinite(punchline_f) or punchline_f < 0:
        reasons.append(f"punchline_at NaN or <0 ({punchline_at!r})")
    if reasons:
        return {
            "status": "error",
            "type": type_,
            "content_dur_s": None,
            "punchline_at_s": None,
            "punchline_rel": None,
            "threshold": threshold,
            "reasons": reasons,
        }
    base = {
        "type": type_,
        "content_dur_s": round(content_dur_f, 3),
        "punchline_at_s": round(punchline_f, 3),
        "punchline_rel": round(punchline_f / content_dur_f, 4),
        "threshold": threshold,
        "reasons": [],
    }
    if type_ != "viral":
        return {**base, "status": "pass", "note": "pacing FLAG applies to type=viral only"}
    rel = punchline_f / content_dur_f
    if rel > threshold + 1e-9:
        return {
            **base,
            "status": "flag",
            "reasons": [f"punchline_rel {rel:.3f} > {threshold}"],
            "note": "advisory FLAG: late punchline; do not REJECT on pacing alone",
        }
    return {**base, "status": "pass", "note": "punchline within threshold"}


def cmd_viralpacing(qc, args) -> int:
    """L21: viral punchline timing FLAG (late → flag exit 0, never fail)."""
    if args.content_dur is None:
        return qc.report("viralpacing", qc.USAGE, error="pass --content-dur seconds")
    if args.punchline_at is None:
        return qc.report("viralpacing", qc.USAGE, error="pass --punchline-at seconds")
    report = check_viral_pacing(
        float(args.content_dur),
        float(args.punchline_at),
        type_=str(args.type),
        threshold=float(args.threshold),
    )
    status = report["status"]
    payload = {k: v for k, v in report.items() if k != "status"}
    if status == "error":
        return qc.report("viralpacing", qc.USAGE, **payload)
    if status == "flag":
        return qc.report("viralpacing", qc.FLAG, **payload)
    return qc.report("viralpacing", qc.PASS, **payload)


def snap_cue_micro_gaps(cues: list[dict], *, max_gap: float = 0.08) -> tuple[list[dict], list[str]]:
    """Snap sub-max_gap gaps; close micro-overlaps at midpoint (L17)."""
    if not cues:
        return [], []
    out = [dict(c) for c in cues]
    notes: list[str] = []
    for i in range(1, len(out)):
        gap = out[i]["start"] - out[i - 1]["end"]
        if 1e-6 < gap < max_gap - 1e-9:
            notes.append(f"snap gap {gap:.4f}s at {out[i - 1]['end']:.3f}")
            out[i]["start"] = out[i - 1]["end"]
        elif gap < -1e-6:
            mid = (out[i - 1]["end"] + out[i]["start"]) / 2.0
            notes.append(f"close overlap {gap:.4f}s at {mid:.3f}")
            out[i - 1]["end"] = mid
            out[i]["start"] = mid
    return out, notes


def _format_srt(cues: list[dict]) -> str:
    def stamp(s: float) -> str:
        ms = int(round(max(0.0, s) * 1000))
        h, rem = divmod(ms, 3_600_000)
        m, rem = divmod(rem, 60_000)
        sec, milli = divmod(rem, 1000)
        return f"{h:02d}:{m:02d}:{sec:02d},{milli:03d}"

    nl = "\n"
    blocks = []
    for i, c in enumerate(cues, 1):
        lines = c.get("lines")
        if lines is None:
            lines = [c.get("text") or ""]
        if isinstance(lines, str):
            lines = lines.splitlines() or [lines]
        body = nl.join(lines)
        blocks.append(f"{i}{nl}{stamp(c['start'])} --> {stamp(c['end'])}{nl}{body}")
    return (nl * 2).join(blocks) + nl


def cmd_cuegapsnap(qc, args) -> int:
    """L17: snap cue gaps under ~80ms; --check fails while micro-gaps remain."""
    raw = Path(args.file).read_text(encoding="utf-8")
    cues = qc.parse_cues(raw)
    max_gap = float(args.max_gap)
    pending = []
    for i in range(1, len(cues)):
        gap = cues[i]["start"] - cues[i - 1]["end"]
        if 1e-6 < gap < max_gap - 1e-9:
            pending.append(round(gap, 4))
        elif gap < -1e-6:
            # micro-overlaps also fail --check (snap closes them; S1)
            pending.append(round(gap, 4))
    snapped, notes = snap_cue_micro_gaps(cues, max_gap=max_gap)
    if args.check:
        return qc.report(
            "cuegapsnap",
            qc.FAIL if pending else qc.PASS,
            pending_gaps=pending,
            max_gap=max_gap,
            note="FAIL while micro-gaps or overlaps remain; snap then re-check (L17)",
        )
    if args.out:
        rebuilt = []
        for c in snapped:
            item = {"start": c["start"], "end": c["end"]}
            if "lines" in c:
                item["lines"] = c["lines"]
            elif "text" in c:
                item["lines"] = str(c["text"]).splitlines() or [str(c["text"])]
            else:
                item["lines"] = [""]
            rebuilt.append(item)
        Path(args.out).write_text(_format_srt(rebuilt), encoding="utf-8")
    return qc.report(
        "cuegapsnap", qc.PASS,
        snapped=len(notes),
        notes=notes,
        max_gap=max_gap,
        out=args.out,
        note="micro-gaps < max_gap snapped to abut (L17)",
    )





# --- Arabic tech copy / bidi-wao (L32) ----------------------------------------
# Fail Arabic conjunction و adjacent to <bdi> Latin in headings / card titles.
# Ported from field site gate; default scope is h1–h6 only (card titles are h3).
WAO_BEFORE = re.compile(r"(?<![ء-يأإآؤئ])و(?:\s|&nbsp;|\u00a0)*<bdi\b", re.I)
WAO_BETWEEN = re.compile(r"</bdi>(?:\s|&nbsp;|\u00a0)*و(?:\s|&nbsp;|\u00a0)*<bdi\b", re.I)
HEADING_BLOCKS = re.compile(r"<h([1-6])\b[^>]*>(.*?)</h\1>", re.I | re.S)


def _heading_snippets(html: str) -> list[tuple[str, str]]:
    """Return (label, inner_html) for each h1–h6 (card titles included)."""
    out = []
    for match in HEADING_BLOCKS.finditer(html):
        level, body = match.group(1), match.group(2)
        out.append((f"h{level}", body))
    return out


def cmd_bidiwao(qc, args) -> int:
    """L32: flag و adjacent to <bdi> Latin in headings/card titles (use ،)."""
    files = files_of(args.path, {".html", ".htm"})
    if not files:
        return qc.report("bidiwao", qc.USAGE, error="pass an .html/.htm file or directory")
    hits = []
    for file in files:
        html = file.read_text(encoding="utf-8", errors="replace")
        snippets = [(f"{file}:doc", html)] if args.all else [
            (f"{file}:{label}", body) for label, body in _heading_snippets(html)
        ]
        if not args.all and not snippets:
            continue
        for where, body in snippets:
            if "design-qc: allow" in body:
                continue
            if WAO_BEFORE.search(body) or WAO_BETWEEN.search(body):
                hits.append(f"{where} wao-adjacent-bdi (use ، or one <bdi> island)")
    return qc.report(
        "bidiwao",
        qc.FAIL if hits else qc.PASS,
        files=len(files),
        hits=hits,
        note="L32: no و next to <bdi> in headings/card titles",
    )




# --- LESS Custom CSS grid/calc (L33) ------------------------------------------
# Fail slash grid-column/row shorthand and bare (unescaped) calc() under LESS
# 1.3.3. Ported from Squarespace Custom CSS field lesson.
# Strip /* */ and // comments before matching so commented examples are ignored.
_LESS_BLOCK_COMMENT = re.compile(r"/\*.*?\*/", re.S)
_LESS_LINE_COMMENT = re.compile(r"(?<!:)//.*?$", re.M)
# grid-column: 1 / -1  or grid-row: 2 / span 3  (slash in the value)
_GRID_SLASH = re.compile(
    r"(?i)(?:grid-column|grid-row)\s*:\s*[^;{}\n]*?/"
)
def _strip_css_comments(src: str) -> str:
    return _LESS_LINE_COMMENT.sub("", _LESS_BLOCK_COMMENT.sub("", src))


def _bare_calc_snippets(body: str) -> list[str]:
    """Snippets of bare calc( not escaped as ~\"calc(...)\" / ~'calc(...)'."""
    out = []
    for match in re.finditer(r"(?i)\bcalc\s*\(", body):
        start = match.start()
        prefix = body[max(0, start - 2):start]
        if prefix in ('~"', "~'"):
            continue
        out.append(body[start:start + 40].split("\n", 1)[0][:60])
    return out


def cmd_lessgrid(qc, args) -> int:
    """L33: flag slash grid shorthand and bare calc() in LESS Custom CSS."""
    files = files_of(args.path, {".css", ".less", ".scss"})
    if not files:
        return qc.report("lessgrid", qc.USAGE, error="pass a .css/.less/.scss file or directory")
    hits = []
    for file in files:
        raw = file.read_text(encoding="utf-8", errors="replace")
        if "design-qc: allow" in raw:
            continue
        body = _strip_css_comments(raw)
        for match in _GRID_SLASH.finditer(body):
            snippet = match.group(0).strip()[:80]
            hits.append(f"{file}: slash-grid `{snippet}` (use grid-*-start/end)")
        for snippet in _bare_calc_snippets(body):
            hits.append(f'{file}: bare-calc `{snippet}` (use ~"calc(...)" or split)')
    return qc.report(
        "lessgrid",
        qc.FAIL if hits else qc.PASS,
        files=len(files),
        hits=hits,
        note="L33: no slash grid shorthand / bare calc under LESS 1.3.3; re-check gutters live after CSS paste",
    )

# --- Hosted-CMS embed gates (L23–L31) -----------------------------------------
def _hosted_cms():
    import importlib.util
    spec = importlib.util.spec_from_file_location(
        "hosted_cms_gates", Path(__file__).with_name("hosted_cms_gates.py"),
    )
    mod = importlib.util.module_from_spec(spec)
    if spec.loader is None:
        raise RuntimeError("hosted_cms_gates loader missing")
    spec.loader.exec_module(mod)
    return mod


def cmd_cmsmeta(qc, args) -> int:
    """L25: paste HTML must not include og:/twitter: meta or <html lang>."""
    html = Path(args.file).read_text(encoding="utf-8")
    report = _hosted_cms().check_cms_meta(html)
    status = report.pop("status")
    code = qc.PASS if status == "pass" else qc.FAIL
    return qc.report("cmsmeta", code, **report)


def cmd_cmsassets(qc, args) -> int:
    """L24: fail relative local assets in paste HTML."""
    html = Path(args.file).read_text(encoding="utf-8")
    report = _hosted_cms().check_cms_assets(html)
    status = report.pop("status")
    code = qc.PASS if status == "pass" else qc.FAIL
    return qc.report("cmsassets", code, **report)


def cmd_cmssticky(qc, args) -> int:
    """L23: require --header-height and scroll-margin-top for sticky offset."""
    body = Path(args.file).read_text(encoding="utf-8")
    report = _hosted_cms().check_cms_sticky(
        body,
        header_px=getattr(args, "header_px", None),
        scroll_margin_px=getattr(args, "scroll_margin_px", None),
    )
    status = report.pop("status")
    if status == "error":
        return qc.report("cmssticky", qc.USAGE, **report)
    code = qc.PASS if status == "pass" else qc.FAIL
    return qc.report("cmssticky", code, **report)


def cmd_cmschrome(qc, args) -> int:
    """L23: paste must not include in-page topbar/nav."""
    html = Path(args.file).read_text(encoding="utf-8")
    report = _hosted_cms().check_cms_chrome(html)
    status = report.pop("status")
    code = qc.PASS if status == "pass" else qc.FAIL
    return qc.report("cmschrome", code, **report)


def cmd_hostilecms(qc, args) -> int:
    """L26: heading luminance and/or squircle tile geometry under hostile CSS."""
    hcg = _hosted_cms()
    parts = []
    if args.heading_color:
        parts.append(hcg.check_hostile_heading(args.heading_color, min_lum=args.min_lum))
    if args.tile_w is not None:
        missing = [n for n, v in (
            ("tile-h", args.tile_h), ("img-x", args.img_x), ("img-y", args.img_y),
            ("img-w", args.img_w), ("img-h", args.img_h),
        ) if v is None]
        if missing:
            return qc.report(
                "hostilecms", qc.USAGE,
                error="pass " + ", ".join("--" + m for m in missing),
            )
        parts.append(hcg.check_hostile_tile(
            args.tile_w, args.tile_h, args.img_x, args.img_y, args.img_w, args.img_h,
            tile_x=args.tile_x, tile_y=args.tile_y,
            sq_tol=args.sq_tol, inset_tol=args.inset_tol,
        ))
    if not parts:
        return qc.report(
            "hostilecms", qc.USAGE,
            error="pass --heading-color and/or --tile-w …",
        )
    if any(p.get("status") == "error" for p in parts):
        return qc.report("hostilecms", qc.USAGE, parts=parts)
    failed = [p for p in parts if p.get("status") == "fail"]
    code = qc.FAIL if failed else qc.PASS
    return qc.report("hostilecms", code, parts=parts, note="hostile heading/tile gates (L26)")


def cmd_cmswidth(qc, args) -> int:
    """L27: full-width ratio + phone gutter parity / no-double-pad."""
    report = _hosted_cms().check_cms_width(
        args.code,
        args.column,
        min_ratio=args.min_ratio,
        hero_gutter=args.hero_gutter,
        embed_gutter=args.embed_gutter,
        gutter_tol=args.gutter_tol,
        outer_pad=args.outer_pad,
        inner_pad=args.inner_pad,
        content_inset=args.content_inset,
        viewport=args.viewport,
        max_phone_inset=args.max_phone_inset,
    )
    status = report.pop("status")
    if status == "error":
        return qc.report("cmswidth", qc.USAGE, **report)
    code = qc.PASS if status == "pass" else qc.FAIL
    return qc.report("cmswidth", code, **report)


def cmd_cmsduration(qc, args) -> int:
    """L30: explainer duration band + optional sibling parity."""
    report = _hosted_cms().check_cms_duration(
        args.dur, min_s=args.min, max_s=args.max, peer=args.peer, peer_tol=args.peer_tol,
    )
    status = report.pop("status")
    if status == "error":
        return qc.report("cmsduration", qc.USAGE, **report)
    code = qc.PASS if status == "pass" else qc.FAIL
    return qc.report("cmsduration", code, **report)


def cmd_livecms(qc, args) -> int:
    """L28/L31: fail-closed live matrix receipt JSON."""
    report_obj = json.loads(Path(args.file).read_text(encoding="utf-8"))
    report = _hosted_cms().check_livecms(report_obj)
    status = report.pop("status")
    if status == "error":
        return qc.report("livecms", qc.USAGE, **report)
    code = qc.PASS if status == "pass" else qc.FAIL
    return qc.report("livecms", code, **report)



def _liveseo():
    import importlib.util
    spec = importlib.util.spec_from_file_location(
        "liveseo_gates", Path(__file__).with_name("liveseo_gates.py"),
    )
    mod = importlib.util.module_from_spec(spec)
    if spec.loader is None:
        raise RuntimeError("liveseo_gates loader missing")
    spec.loader.exec_module(mod)
    return mod


def cmd_liveseo(qc, args) -> int:
    """W16: cheap public SEO + AI crawler / llms.txt gate (published HTML)."""
    ls = _liveseo()
    if getattr(args, "fixture", None):
        bundle = ls.load_fixture(args.fixture)
    elif getattr(args, "url", None):
        bundle = ls.fetch_bundle(
            args.url,
            link_sample=args.link_sample,
            missing_path=args.missing_path,
        )
        if args.allow_og_unfixable:
            bundle["allow_og_unfixable"] = True
    else:
        return qc.report("liveseo", qc.USAGE, error="pass a url or --fixture DIR")
    report = ls.evaluate_bundle(bundle)
    status = report.pop("status")
    if status == "error":
        return qc.report("liveseo", qc.USAGE, **report)
    if status == "flag":
        return qc.report("liveseo", qc.FLAG, **report)
    code = qc.PASS if status == "pass" else qc.FAIL
    return qc.report("liveseo", code, **report)


def register(add, qc) -> None:
    """Add the craft subcommands to design_qc's parser."""
    def bind(func):
        return lambda args: func(qc, args)

    for name, rules, text, case in (("chrome", CHROME, "page chrome / HUD in a frame", False),
                                    ("copyslop", COPYSLOP, "slogan copy", False)):
        p = add(name, (lambda r, n, c: lambda args: scan(qc, n, args.path, r, args.allow, c))(rules, name, case), text)
        p.add_argument("path")
        p.add_argument("--allow", action="append", default=[])
    p = add("uimotion", bind(cmd_uimotion), "scale(0) or layout-property animation")
    p.add_argument("path")
    p = add("hovergate", bind(cmd_hovergate), ":hover outside (hover: hover) or hover-gated content")
    p.add_argument("path")
    p = add("maskprefix", bind(cmd_maskprefix), "mask-image without -webkit-mask-image in the same rule, or vice versa")
    p.add_argument("path")
    p = add("svga11y", bind(cmd_svga11y), "svg role=img, title and desc")
    p.add_argument("path")
    p = add("floor", bind(cmd_floor), "open Floor finding or unreasoned Reflex overrule in a review")
    p.add_argument("file")
    p = add("bidi", bind(cmd_bidi), "Latin runs with neutral chars outside an isolate in RTL text (captions, hooks, labels)")
    p.add_argument("path")
    p = add("cuegaps", bind(cmd_cuegaps), "captionless frames at splice joins")
    p.add_argument("file")
    p.add_argument("--join", type=float, action="append", default=[])
    p.add_argument("--frame", type=float, default=1 / 30)
    p = add("asrhall", bind(cmd_asrhall), "ASR pause hallucinations in a transcript")
    p.add_argument("file")
    p.add_argument("--phrase", action="append", default=[])
    p = add("boundprefix", bind(cmd_boundprefix), "caption line ending on a bound prefix")
    p.add_argument("file")
    p = add("termlexicon", bind(cmd_termlexicon),
            "ASR Arabic-script tech/loan transliterations left in captions (prefer Latin)")
    p.add_argument("file")
    p.add_argument("--term", action="append", default=[], metavar="AR=LATIN",
                   help="extend placeholder lexicon (multiword AR allowed before =)")
    p = add("fadetail", bind(cmd_fadetail), "tail fade clears the last spoken word")
    p.add_argument("--words", required=True)
    p.add_argument("--end", type=float, required=True)
    p.add_argument("--fade", type=float, required=True)
    p.add_argument("--pause", type=float, default=0.2)
    p = add("cuemids", bind(cmd_cuemids), "cue midpoints and snapped probe times for contact sheets")
    p.add_argument("file")
    p.add_argument("--probe", type=float, action="append", default=[])
    p = add("fadeonly", bind(cmd_fadeonly), "opacity-only keyframe entries")
    p.add_argument("path")
    p = add("springs", bind(cmd_springs), "visible springs or overshoots per film")
    p.add_argument("path")
    p.add_argument("--max", type=int, default=1)
    p = add("motiondur", bind(cmd_motiondur), "median declared motion duration")
    p.add_argument("path")
    p.add_argument("--max", type=float, default=0.6)
    p = add("measure", bind(cmd_measure), "line length in ch")
    p.add_argument("path")
    p.add_argument("--min", type=float, default=45)
    p.add_argument("--max", type=float, default=75)
    p = add("typescale", bind(cmd_typescale), "type scale steps and count")
    p.add_argument("path")
    p.add_argument("--min-ratio", type=float, default=1.125)
    p.add_argument("--max-sizes", type=int, default=7)
    p = add("safezones", bind(cmd_safezones), "text boxes inside platform safe zones")
    p.add_argument("file")
    p.add_argument("--format", choices=sorted(SAFE), default="9x16")
    p.add_argument("--px-map", choices=sorted(PX_MAP), help="px boxes on 1080x1920: words inside 168-1330")
    p.add_argument("--rail", type=int, default=0, help="right action-rail width in px for the platform (0 = off)")
    p = add("endhold", bind(cmd_endhold), "frozen end-card hold length")
    p.add_argument("file")
    p.add_argument("--max", type=float, default=1.4)
    p.add_argument("--min", type=float, default=0.6)
    p.add_argument("--thresh", type=float, default=1.0)
    p = add("motionband", bind(cmd_motionband), "mean frame-to-frame motion band")
    p.add_argument("file")
    # 64x36 luma metric; motionmaxxing's own pro comparison films measure 0.7-3.1 here
    p.add_argument("--low", type=float, default=0.5)
    p.add_argument("--high", type=float, default=6.0)
    p = add("hook", bind(cmd_hook), "poster-worthy first frame and early change")
    p.add_argument("file")
    p.add_argument("--window", type=float, default=1.0)
    p.add_argument("--min-std", type=float, default=12.0)
    p.add_argument("--min-change", type=float, default=4.0)
    p = add("cutrhythm", bind(cmd_cutrhythm), "longest shot between cuts")
    p.add_argument("file")
    p.add_argument("--max-shot", type=float, default=4.0)
    p.add_argument("--cut", type=float, default=3.0)
    p = add("capluma", bind(cmd_capluma), "luma behind caption box picks blend; plain subtitles over 60 need a scrim")
    p.add_argument("file")
    p.add_argument("--cues", required=True)
    p.add_argument("--box", required=True, help="x,y,w,h in px of the caption box")
    p.add_argument("--plain", action="store_true", help="plain subtitles with no scrim")
    p = add("firstframe", bind(cmd_firstframe), "empty first frame; first cue before 0.1 s")
    p.add_argument("file")
    p.add_argument("--cues")
    p.add_argument("--black", type=float, default=16.0)
    p.add_argument("--flat", type=float, default=4.0)
    p = add("deadair", bind(cmd_deadair), "static picture with silent audio over max s; silent film over 15 s")
    p.add_argument("file")
    p.add_argument("--max", type=float, default=3.0)
    p.add_argument("--silent-film", type=float, default=15.0)
    p.add_argument("--noise", type=float, default=-35.0)
    p.add_argument("--fps", type=float, default=10.0)
    p.add_argument("--thresh", type=float, default=6.0)
    p = add("silencegap", bind(cmd_silencegap),
            "RMS mid-answer silence gap on the final render (FAIL if any gap > max s)")
    p.add_argument("file")
    p.add_argument("--hook-end", type=float, default=0.0,
                   help="exclude [0, hook_end) from mid-answer")
    p.add_argument("--content-end", type=float, default=None,
                   help="end of mid-answer (before end-card / last speech); "
                        "default duration-0.3 is NOT safe for end-card shorts — pass it")
    p.add_argument("--thr", type=float, default=-40.0, help="RMS dBFS threshold")
    p.add_argument("--hop", type=float, default=0.01)
    p.add_argument("--min-gap", type=float, default=0.08)
    p.add_argument("--max", type=float, default=0.6, help="hard FAIL if any mid-answer gap exceeds this")
    p.add_argument("--target", type=float, default=0.45,
                   help="soft comfort target; warn (do not fail) when max mid gap is above this but ≤ --max")
    p = add("txdiff", bind(cmd_txdiff),
            "SRT vs words.json exact-raw + fuzzy; G20 also reports RAW ASR via --words-raw")
    p.add_argument("srt")
    p.add_argument("words", help="normalized words.json (post orthography)")
    p.add_argument("--max", type=float, default=0.12)
    p.add_argument("--words-raw", default=None, dest="words_raw",
                   help="G20: RAW ASR words.json (pre-orthography); gate both ≤ --max")
    p = add("hookmodality", bind(cmd_hookmodality),
            "hook card keeps speaker modality (question ≠ claim) for every hook type")
    p.add_argument("--hook", required=True, help="hook card text file")
    p.add_argument("--spoken", required=True, help="spoken hook / under-card transcript file")
    p.add_argument("--type", choices=("auto", "qa", "claim"), default="auto",
                   help="qa also requires --after answer-beat check (G14)")
    p.add_argument("--after", help="post-question transcript for G14 answer-beat pre-flight")
    p.add_argument("--min-chars", type=int, default=40,
                   help="min non-dodge chars after question to count as an answer beat")
    p = add("qaanswer", bind(cmd_qaanswer),
            "pre-render: qa rows need an answer beat in transcript / span (else drop)")
    p.add_argument("--after", help="post-question transcript text")
    p.add_argument("--span", help="BRIEF/span.json that must carry answer_beat")
    p.add_argument("--min-chars", type=int, default=40)
    p = add("opendoubt", bind(cmd_opendoubt),
            "SELF-REVIEW must not leave self-flagged doubts open before handoff (G15)")
    p.add_argument("file", help="SELF-REVIEW markdown (or handoff note)")
    p = add("metadatadesc", bind(cmd_metadatadesc),
            "METADATA.md description is exact 4-line Shorts template (G16)")
    p.add_argument("file", help="METADATA.md with ## Title and ## Description")
    p.add_argument("--site", required=True,
                   help="exact line-3 site URL (project fill-in; no default)")
    p.add_argument("--tags", required=True,
                   help="exact line-4 hashtag string (project fill-in; no default)")
    p.add_argument("--type", choices=("qa", "viral"), default="qa",
                   help="L22: line-1 prefix qa=مقطع من الـ Q&A: / viral=مقطع:")
    p = add("hookbleed", bind(cmd_hookbleed),
            "OUT SRT must not straddle hook_end with under-hook speech")
    p.add_argument("srt")
    p.add_argument("--hook-end", type=float, required=True)
    p.add_argument("--pad", type=float, default=0.3,
                   help="cues may end ≤ hook_end+pad; beyond that is bleed")
    p = add("softjump", bind(cmd_softjump),
            "big content jumps need soft transition / punch-in in the edit plan")
    p.add_argument("file", help="JSON edit plan with cuts[].removed_s and soft/punch_in/crossfade")
    p.add_argument("--min-removed", type=float, default=2.0,
                   help="removed bridge seconds that require a soft mask")
    p = add("antecedent", bind(cmd_antecedent),
            "G13 kept-span epithets, or G17 per-splice land openers (--splice)")
    p.add_argument("file", help="kept SRT (G17) or script/text (G13)")
    p.add_argument("--epithet", action="append", default=[],
                   help="pronoun/descriptor that needs a referent (repeatable; G13)")
    p.add_argument("--noun", action="append", default=[],
                   help="G13 referent, or G17 she-class binder (هي/دي/ده/…) in prior (repeatable)")
    p.add_argument("--noun-ho", action="append", default=[], dest="noun_ho",
                   help="G17 هو/هم-class binder in prior (repeatable); not used for هي/دي lands")
    p.add_argument("--anywhere", action="store_true",
                   help="allow noun anywhere in the file, not only before the epithet (G13)")
    p.add_argument("--splice", default=None,
                   help="G17: pre-cut splice list a-b,c-d; land opener binds via class-matched --noun/--noun-ho")
    p = add("overlayverbatim", bind(cmd_overlayverbatim),
            "G18: hook/short/SRT overlay tokens must be spoken verbatim in words.json")
    p.add_argument("words", help="words.json (spoken word bag)")
    p.add_argument("--srt", required=True, help="caption SRT (every cue checked)")
    p.add_argument("--hook", default="", help="hook card text; | splits parts")
    p.add_argument("--short", default="", help="short/footer/label overlay text")
    p.add_argument("--threshold", type=float, default=0.72,
                   help="fuzzy match ratio against spoken bag (default 0.72)")
    p = add("orthography", bind(cmd_orthography),
            "G19: ASR orthography audit (ه-for-ة etc.) on title/short/hook/SRT")
    p.add_argument("--srt", required=True, help="caption SRT (every cue checked)")
    p.add_argument("--title", default="", help="title overlay text")
    p.add_argument("--short", default="", help="short/footer/label overlay text")
    p.add_argument("--hook", default="", help="hook card text; | splits parts")
    p.add_argument("--fix", action="append", default=[], metavar="AR=CORRECT",
                   help="extend baseline ASR_ORTHO_FIX map")
    p = add("titlepolarity", bind(cmd_titlepolarity),
            "G21/G22: title polarity + claim binds to kept-speech clause (--words/--splice)")
    p.add_argument("--srt", required=True, help="source/caption SRT overlapping title content")
    p.add_argument("--title", default="", help="title text (or use --metadata)")
    p.add_argument("--metadata", default=None, help="METADATA.md with ## Title block")
    p.add_argument("--words", default=None, help="G22: pre-cut words.json for ordered clause binding")
    p.add_argument("--splice", default="", help="G22: span-relative cuts a-b,c-d; clause must ⊆ kept")
    p = add("textfloor", bind(cmd_textfloor), "text under 2.6%% of the short side (28 px on portrait)")
    p.add_argument("file")
    p.add_argument("--frame", default="1080x1920")
    p.add_argument("--ratio", type=float, default=0.026)
    p.add_argument("--portrait-px", type=float, default=28.0)
    p = add("mediaclamp", bind(cmd_mediaclamp), "cue or card ends past the media's ffprobe length")
    p.add_argument("cues")
    p.add_argument("media")
    p.add_argument("--tolerance", type=float, default=0.001)
    p = add("artrack", bind(cmd_artrack), "letter-spacing on Arabic text")
    p.add_argument("path")
    p = add("pptxbidi", bind(cmd_pptxbidi), "Arabic PPTX paragraphs: native rtl, no bidi controls, native bullets")
    p.add_argument("file")
    p = add("privacynames", bind(cmd_privacynames),
            "L09: caption+ASR personal names/amounts; mute notes alone never PASS")
    p.add_argument("--srt", default=None, help="caption SRT to scan")
    p.add_argument("--words", default=None, help="ASR words.json to scan")
    p.add_argument("--name", action="append", default=[],
                   help="denylist token (repeatable; project-supplied)")
    p.add_argument("--vocative-only", action="append", default=[], dest="vocative_only",
                   help="token only counts after يا/ياا")
    p.add_argument("--notes", default=None, help="review notes; mute without text-scan fails")
    p.add_argument("--amount-re", default=None, dest="amount_re",
                   help="override hard-amount regex")
    p.add_argument("--allow-empty-names", action="store_true", dest="allow_empty_names",
                   help="allow amount-only scan with no --name")
    p.add_argument("--require-words", action="store_true", dest="require_words",
                   help="L18: FAIL when --words missing/empty (run_gates fail-closed)")
    p = add("titlepunct", bind(cmd_titlepunct),
            "L11: title/desc question mark matches script (Latin ? vs Arabic ؟)")
    p.add_argument("--title", default="", help="title string")
    p.add_argument("--desc", default="", help="description string")
    p.add_argument("--metadata", default=None, help="METADATA.md with ## Title / ## Description")
    p = add("endcardlive", bind(cmd_endcardlive),
            "L10: live end-card band-row ink vs clean endcard plate")
    p.add_argument("file", nargs="?", default=None, help="final mp4 (optional if --live given)")
    p.add_argument("--clean", required=True, help="clean endcard.png plate")
    p.add_argument("--live", default=None, help="pre-grabbed live end-card frame")
    p.add_argument("--band-y", type=int, default=500, dest="band_y")
    p.add_argument("--band-h", type=int, default=175, dest="band_h")
    p.add_argument("--offset-from-end", type=float, default=1.5, dest="offset_from_end")
    p.add_argument("--max-live-ink", type=float, default=0.01, dest="max_live_ink")
    p.add_argument("--max-ink-delta", type=float, default=0.008, dest="max_ink_delta")
    p = add("qmarkbaseline", bind(cmd_qmarkbaseline),
            "L12: trailing ?/؟ ink bottom vs body baseline (fail >6%% line_h)")
    p.add_argument("file", help="rendered band PNG")
    p.add_argument("--direction", choices=("ltr", "rtl"), default=None)
    p.add_argument("--max-frac", type=float, default=0.06, dest="max_frac")
    p.add_argument("--ignore-below", type=int, default=None, dest="ignore_below",
                   help="clear mask rows at/after this y (drop footer chrome)")

    p = add("bandcoverage", bind(cmd_bandcoverage),
            "L14: text-like ink outside burned band on content frames")
    p.add_argument("--frame", action="append", default=[], help="PNG/JPG frame to check (repeatable)")
    p.add_argument("--mp4", default=None, help="sample frames from rendered unit mp4")
    p.add_argument("--band-y", type=int, default=500, dest="band_y")
    p.add_argument("--band-h", type=int, default=175, dest="band_h")
    p.add_argument("--content-end", type=float, default=None, dest="content_end",
                   help="stop sampling before end-card (seconds)")
    p.add_argument("--n", type=int, default=8, help="sample count for --mp4")
    p = add("mutespan", bind(cmd_mutespan),
            "L15: mute ≤2s/span and ≤10%% of content duration")
    p.add_argument("--mutes", default=None, help="JSON list of [start,end] or {mutes:[...]}")
    p.add_argument("--mute", action="append", default=[], help="start,end seconds (repeatable)")
    p.add_argument("--content-dur", type=float, default=None, dest="content_dur",
                   help="content duration seconds (exclude end card)")
    p.add_argument("--max-span", type=float, default=2.0, dest="max_span")
    p.add_argument("--max-frac", type=float, default=0.10, dest="max_frac")
    p = add("viralpacing", bind(cmd_viralpacing),
            "L21: viral late-punchline FLAG (advisory; never FAIL)")
    p.add_argument("--content-dur", type=float, required=True, dest="content_dur",
                   help="content duration seconds (exclude end card)")
    p.add_argument("--punchline-at", type=float, required=True, dest="punchline_at",
                   help="absolute seconds where title claim / punchline lands")
    p.add_argument("--type", choices=("viral", "qa", "other"), default="viral",
                   help="only type=viral can FLAG; others PASS")
    p.add_argument("--threshold", type=float, default=0.65,
                   help="FLAG when punchline_at/content_dur > threshold")
    p = add("cmsmeta", bind(cmd_cmsmeta),
            "L25: paste HTML must not include og:/twitter: meta")
    p.add_argument("file", help="paste HTML file")
    p = add("cmsassets", bind(cmd_cmsassets),
            "L24: fail relative local assets in paste HTML")
    p.add_argument("file", help="paste HTML file")
    p = add("cmssticky", bind(cmd_cmssticky),
            "L23: require --header-height and scroll-margin-top (sticky offset)")
    p.add_argument("file", help="paste HTML or CSS file")
    p.add_argument("--header-px", type=float, default=None, dest="header_px",
                   help="optional sticky header height px")
    p.add_argument("--scroll-margin-px", type=float, default=None,
                   dest="scroll_margin_px",
                   help="optional measured scroll-margin-top px")

    p = add("cmschrome", bind(cmd_cmschrome),
            "L23: paste must not include in-page topbar/nav")
    p.add_argument("file", help="paste HTML file")
    p = add("hostilecms", bind(cmd_hostilecms),
            "L26: heading luminance and/or squircle tile under hostile CSS")
    p.add_argument("--heading-color", default=None, dest="heading_color",
                   help="computed CSS color e.g. rgb(255,255,255)")
    p.add_argument("--min-lum", type=float, default=0.85, dest="min_lum")
    p.add_argument("--tile-w", type=float, default=None, dest="tile_w")
    p.add_argument("--tile-h", type=float, default=None, dest="tile_h")
    p.add_argument("--tile-x", type=float, default=0.0, dest="tile_x")
    p.add_argument("--tile-y", type=float, default=0.0, dest="tile_y")
    p.add_argument("--img-x", type=float, default=None, dest="img_x")
    p.add_argument("--img-y", type=float, default=None, dest="img_y")
    p.add_argument("--img-w", type=float, default=None, dest="img_w")
    p.add_argument("--img-h", type=float, default=None, dest="img_h")
    p.add_argument("--sq-tol", type=float, default=1.5, dest="sq_tol")
    p.add_argument("--inset-tol", type=float, default=0.75, dest="inset_tol")
    p = add("cmswidth", bind(cmd_cmswidth),
            "L27: full-width ratio + phone gutter parity / no-double-pad")
    p.add_argument("--code", type=float, required=True, help="code container width px")
    p.add_argument("--column", type=float, required=True, help="content column width px")
    p.add_argument("--min-ratio", type=float, default=0.92, dest="min_ratio")
    p.add_argument("--hero-gutter", type=float, default=None, dest="hero_gutter",
                   help="hero container left inset px (phone)")
    p.add_argument("--embed-gutter", type=float, default=None, dest="embed_gutter",
                   help="embed content left inset px (phone)")
    p.add_argument("--gutter-tol", type=float, default=4.0, dest="gutter_tol",
                   help="max |embed-hero| gutter delta px (default 4)")
    p.add_argument("--outer-pad", type=float, default=None, dest="outer_pad",
                   help="outer padding-inline px (no-double-pad)")
    p.add_argument("--inner-pad", type=float, default=None, dest="inner_pad",
                   help="nested padding-inline px (no-double-pad)")
    p.add_argument("--content-inset", type=float, default=None, dest="content_inset",
                   help="measured content left inset px (phone double-pad band)")
    p.add_argument("--viewport", type=float, default=None,
                   help="viewport width px (phone checks when <=430 or omitted)")
    p.add_argument("--max-phone-inset", type=float, default=36.0, dest="max_phone_inset",
                   help="fail phone content-inset above this (default 36)")
    p = add("cmsduration", bind(cmd_cmsduration),
            "L30: explainer duration band + optional sibling parity")
    p.add_argument("--dur", type=float, required=True, help="duration seconds")
    p.add_argument("--min", type=float, default=60.0, help="min seconds (default 60)")
    p.add_argument("--max", type=float, default=120.0, help="max seconds (default 120)")
    p.add_argument("--peer", type=float, default=None, help="sibling duration seconds")
    p.add_argument("--peer-tol", type=float, default=30.0, dest="peer_tol")
    p = add("livecms", bind(cmd_livecms),
            "L28/L31: fail-closed live matrix receipt JSON")
    p.add_argument("file", help="livecms report JSON")
    p = add("bidiwao", bind(cmd_bidiwao),
            "L32: Arabic و adjacent to <bdi> Latin in headings/card titles (use ،)")
    p.add_argument("path", help="HTML file or directory")
    p.add_argument("--all", action="store_true",
                   help="scan whole document, not only h1–h6")

    p = add("lessgrid", bind(cmd_lessgrid),
            "L33: slash grid-column/row + bare calc under LESS 1.3.3 Custom CSS")
    p.add_argument("path", help="CSS/LESS/SCSS file or directory")
    p = add("liveseo", bind(cmd_liveseo),
            "W16: public SEO + AI crawler / llms.txt (url or --fixture)")
    p.add_argument("url", nargs="?", default=None, help="https URL to audit")
    p.add_argument("--fixture", default=None, help="offline fixture directory")
    p.add_argument("--link-sample", type=int, default=5, dest="link_sample")
    p.add_argument("--missing-path", default="/chaosengine-liveseo-missing-probe-404",
                   dest="missing_path")
    p.add_argument("--allow-og-unfixable", action="store_true",
                   dest="allow_og_unfixable",
                   help="treat missing/http og:image as UNFIXABLE (plan-locked)")

    p = add("cuegapsnap", bind(cmd_cuegapsnap),
            "L17: snap cue micro-gaps under ~80ms; --check fails while pending")
    p.add_argument("file", help="caption SRT")
    p.add_argument("--max-gap", type=float, default=0.08, dest="max_gap")
    p.add_argument("--check", action="store_true", help="FAIL while micro-gaps remain")
    p.add_argument("--out", default=None, help="write snapped SRT")
