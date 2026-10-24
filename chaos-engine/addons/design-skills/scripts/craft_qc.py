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
    left, top, right, bottom = SAFE[args.format]
    boxes = json.loads(Path(args.file).read_text(encoding="utf-8"))["boxes"]
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
