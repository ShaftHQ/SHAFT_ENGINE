"""Craft checks for design_qc.py: motion grammar, copy, layout and edit rhythm.

Sources: references/research/motionmaxxing-map.md (Apache-2.0 ideas, re-expressed)
and the craft cards in references/craft/. Each check is red-then-green tested.
"""

from __future__ import annotations

import json
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
    """Flag leftover Arabic-script forms of known tech terms (optional bound prefixes)."""
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
    return qc.report(
        "silencegap", qc.FAIL if fail else qc.PASS,
        max_gap=args.max, hook_end=args.hook_end, content_end=round(content_end, 3),
        duration=round(duration, 3), max_mid_answer_gap=round(max_mid, 3),
        mid_answer_gaps=mid, fail_gaps=fail)


TXDIFF_PUNCT = re.compile(r"[.,!?;:؟،؛\"\'()\\[\\]{}…—–\\-]+")


def _txdiff_tokens(text: str) -> list[str]:
    cleaned = TXDIFF_PUNCT.sub(" ", text.replace("\u00a0", " ").replace("\u0640", ""))
    return [w for w in cleaned.split() if w and not w.endswith("ـ")]


def _txdiff_fold(word: str) -> str:
    """Exact-raw fold: Latin .lower(); Arabic unchanged. No alias, no fuzzy."""
    return word.lower() if re.search(r"[A-Za-z]", word) else word


def cmd_txdiff(qc, args) -> int:
    """SRT ↔ words.json exact-raw + fuzzy side-by-side (G9). Gate on exact-raw only."""
    import difflib

    cues = qc.parse_cues(Path(args.srt).read_text(encoding="utf-8"))
    words = words_of(json.loads(Path(args.words).read_text(encoding="utf-8")))
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
    n = max(1, len(tokens))
    diff_exact = len(miss_exact) / n
    diff_fuzzy = len(miss_fuzzy) / n
    problems = []
    if diff_exact > args.max:
        problems.append(f"exact-raw {diff_exact:.2%} above {args.max:.0%}")
    return qc.report(
        "txdiff", qc.FAIL if problems else qc.PASS,
        mode="exact-raw+fuzzy",
        diff_exact_raw=round(diff_exact, 4),
        diff_fuzzy=round(diff_fuzzy, 4),
        unmatched_exact_raw=len(miss_exact),
        unmatched_fuzzy=len(miss_fuzzy),
        miss_exact_raw=miss_exact[:20],
        miss_fuzzy=miss_fuzzy[:20],
        srt_words=len(tokens),
        max=args.max,
        problems=problems,
        note="gate on exact-raw (no alias, no fuzzy); fuzzy is companion only",
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
            "ASR Arabic-script tech transliterations left in captions (prefer Latin)")
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
    p.add_argument("--hook-end", type=float, default=0.0)
    p.add_argument("--content-end", type=float, default=None)
    p.add_argument("--thr", type=float, default=-40.0, help="RMS dBFS threshold")
    p.add_argument("--hop", type=float, default=0.01)
    p.add_argument("--min-gap", type=float, default=0.08)
    p.add_argument("--max", type=float, default=0.6)
    p = add("txdiff", bind(cmd_txdiff),
            "SRT vs words.json exact-raw + fuzzy; gate on exact-raw only (G9)")
    p.add_argument("srt")
    p.add_argument("words")
    p.add_argument("--max", type=float, default=0.12)
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
