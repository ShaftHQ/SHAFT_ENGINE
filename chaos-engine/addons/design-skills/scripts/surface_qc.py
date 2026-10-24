"""Surface checks for design_qc.py: web tokens, a11y, colour, captions, stills, decks, Arabic and OG images.

Sources: the batch-3 cards W07-W14, V10, I01, S04 and L07 (re-expressed rules, pins in INVENTORY.md).
Each check is red-then-green tested in tests/scripts/test_ce_design_batch3.py.
"""

from __future__ import annotations

import json
import math
import re
import struct
import zipfile
from pathlib import Path

CSS_LIKE = {".css", ".scss", ".html", ".htm", ".vue", ".svelte", ".jsx", ".tsx"}
ARABIC = re.compile("[\u0600-\u06FF\u0750-\u077F\uFB50-\uFDFF\uFE70-\uFEFF]")
AR_SELECTOR = re.compile(r":lang\(ar|\[lang=['\"]?ar|\[dir=['\"]?rtl|arab", re.I)
HUES = ("red|orange|amber|yellow|lime|green|emerald|teal|cyan|sky|blue|indigo|violet|purple|fuchsia|pink|rose|"
        "slate|gray|grey|zinc|neutral|stone")
PRIMITIVE_USE = re.compile(rf"var\(\s*--(?:{HUES})-\d{{2,3}}\b|\b(?:bg|text|border|ring|fill|stroke|from|to|via)-(?:{HUES})-\d{{2,3}}\b")
AL_LATIN = re.compile(r"(?:^|[\s(«\"])(?:وبال|فبال|بال|كال|فال|وال|لل|ال)ـ?\s?[A-Za-z]")


def files_of(root: str, suffixes: set[str]) -> list[Path]:
    base = Path(root)
    return [base] if base.is_file() else sorted(p for p in base.rglob("*") if p.suffix in suffixes and p.is_file())


def line_of(text: str, index: int) -> int:
    return text.count("\n", 0, index) + 1


def rules(text: str):
    """Yield (selector, body, start) for innermost CSS rules."""
    stack = []
    for match in re.finditer(r"[{}]", text):
        if match.group() == "{":
            head_start = max(text.rfind("}", 0, match.start()), text.rfind("{", 0, match.start())) + 1
            stack.append((text[head_start:match.start()], match.end()))
        elif stack:
            head, start = stack.pop()
            body = text[start:match.start()]
            if "{" not in body:
                yield head.strip(), body, start


def cmd_spacing(qc, args) -> int:
    scale = {int(v) for v in args.scale.split(",")} | {0}
    hits = []
    prop = re.compile(r"\b(margin|padding|gap|row-gap|column-gap)(?:-[a-z-]+)?\s*:\s*([^;}\n]+)", re.I)
    for file in files_of(args.path, CSS_LIKE):
        text = file.read_text(encoding="utf-8", errors="replace")
        for match in prop.finditer(text):
            for value in re.findall(r"(-?\d*\.?\d+)px", match.group(2)):
                if abs(float(value)) not in scale:
                    hits.append(f"{file}:{line_of(text, match.start())} {match.group(1)} {value}px off the scale")
    return qc.report("spacing", qc.FAIL if hits else qc.PASS, scale=sorted(scale), hits=hits)


def cmd_tokens(qc, args) -> int:
    hits = []
    for file in files_of(args.path, CSS_LIKE | {".js", ".ts"}):
        if re.search(r"token|theme|palette", file.name, re.I):
            continue
        text = file.read_text(encoding="utf-8", errors="replace")
        for match in PRIMITIVE_USE.finditer(text):
            hits.append(f"{file}:{line_of(text, match.start())} primitive {match.group(0).strip()} in a component")
    return qc.report("tokens", qc.FAIL if hits else qc.PASS, hits=hits)


def cmd_a11yscan(qc, args) -> int:
    checks = {
        "positive-tabindex": r"tabindex\s*=\s*['\"]?\s*[1-9]",
        "zoom-lock": r"user-scalable\s*=\s*(?:no|0)|maximum-scale\s*=\s*1(?:\.0)?\b",
        "transition-all": r"transition\s*:\s*all\b|transition-property\s*:\s*all\b",
        "ease-in-ui": r"(?:transition|animation)(?:-timing-function)?\s*:[^;}]*\bease-in\b(?!-)",
    }
    hits = []
    for file in files_of(args.path, CSS_LIKE):
        text = file.read_text(encoding="utf-8", errors="replace")
        for name, pattern in checks.items():
            for match in re.finditer(pattern, text, re.I):
                hits.append(f"{file}:{line_of(text, match.start())} {name}")
        for match in re.finditer(r"transition(?:-duration)?\s*:([^;}]*)", text, re.I):
            for number, unit in re.findall(r"(\d*\.?\d+)(ms|s)\b", match.group(1)):
                if float(number) * (1 if unit == "ms" else 1000) > args.max_ms:
                    hits.append(f"{file}:{line_of(text, match.start())} ui-duration {number}{unit} over {args.max_ms:g}ms")
                    break
    return qc.report("a11yscan", qc.FAIL if hits else qc.PASS, hits=hits)


def cmd_p3fallback(qc, args) -> int:
    hits = []
    for file in files_of(args.path, CSS_LIKE):
        text = file.read_text(encoding="utf-8", errors="replace")
        masked = text
        for media in re.finditer(r"@media[^{]*color-gamut\s*:\s*p3[^{]*\{", text, re.I):
            depth, end = 1, media.end()
            while depth and end < len(text):
                depth += {"{": 1, "}": -1}.get(text[end], 0)
                end += 1
            masked = masked[:media.start()] + " " * (end - media.start()) + masked[end:]
        for _selector, body, start in rules(masked):
            seen = set()
            for decl in re.finditer(r"([a-z-]+)\s*:\s*([^;]+)", body, re.I):
                name, value = decl.group(1).lower(), decl.group(2)
                if "display-p3" in value and name not in seen:
                    hits.append(f"{file}:{line_of(text, start + decl.start())} {name} p3 with no sRGB fallback first")
                if "display-p3" not in value:
                    seen.add(name)
    return qc.report("p3fallback", qc.FAIL if hits else qc.PASS, hits=hits)


def oklch(value: str) -> tuple[float, float, float]:
    match = re.fullmatch(r"\s*oklch\(\s*([\d.]+)(%?)\s+([\d.]+)\s+([\d.]+)(?:deg)?\s*(?:/[^)]*)?\)\s*", value)
    if not match:
        raise ValueError(f"not oklch(): {value}")
    lightness = float(match.group(1)) / (100 if match.group(2) else 1)
    return lightness, float(match.group(3)), float(match.group(4))


def hue_gap(a: float, b: float) -> float:
    gap = abs(a - b) % 360
    return min(gap, 360 - gap)


def cmd_oklch(qc, args) -> int:
    spec = json.loads(Path(args.file).read_text(encoding="utf-8"))
    hits = []
    for name, steps in spec.get("ramps", {}).items():
        hues = [oklch(s)[2] for s in steps if oklch(s)[1] >= 0.02]
        if hues and max(hue_gap(a, b) for a in hues for b in hues) > args.drift:
            hits.append(f"ramp {name}: hue drift over {args.drift:g} deg")
    accent = spec.get("accent")
    for name, value in spec.get("status", {}).items():
        if accent and hue_gap(oklch(value)[2], oklch(accent)[2]) < args.sep:
            hits.append(f"status {name}: within {args.sep:g} deg of the accent hue")
    for fg, bg in spec.get("pairs", []):
        lf, lb = oklch(fg)[0], oklch(bg)[0]
        if (lb > 0.85 and lf >= 0.45) or (lb < 0.25 and lf <= 0.75):
            hits.append(f"pair {fg} on {bg}: lightness gap too small")
    return qc.report("oklch", qc.FAIL if hits else qc.PASS, hits=hits)


def cmd_sidecar(qc, args) -> int:
    raw = Path(args.file).read_bytes()
    try:
        text = raw.decode("utf-8-sig")
    except UnicodeDecodeError:
        return qc.report("sidecar", qc.FAIL, hits=["not UTF-8"], warnings=[])
    hits, warnings = [], []
    if args.file.lower().endswith(".vtt") and not text.lstrip("\ufeff").startswith("WEBVTT"):
        hits.append("WebVTT file without a WEBVTT header")
    cues = qc.parse_cues(re.sub(r"(?<![\d:])(\d{2}:\d{2}\.\d{3})", r"00:\1", text))  # WebVTT mm:ss.ttt
    if not cues:
        hits.append("no cues")
    for index, cue in enumerate(cues, 1):
        if cue["end"] <= cue["start"]:
            hits.append(f"cue {index}: end not after start")
        if index > 1 and cue["start"] < cues[index - 2]["start"]:
            hits.append(f"cue {index}: starts before the previous cue")
        if len(cue["lines"]) > args.max_lines:
            hits.append(f"cue {index}: {len(cue['lines'])} lines")
        hits += [f"cue {index}: {len(line)} chars in a line" for line in cue["lines"] if len(line) > args.max_chars]
        duration = cue["end"] - cue["start"]
        chars = sum(len(line) for line in cue["lines"])
        if duration > 0 and chars / duration > args.warn_cps:
            warnings.append(f"cue {index}: {chars / duration:.1f} chars/s")
    return qc.report("sidecar", qc.FAIL if hits else qc.PASS, cues=len(cues), hits=hits, warnings=warnings)


def cmd_thumb(qc, args) -> int:
    spec = json.loads(Path(args.file).read_text(encoding="utf-8"))
    width, height = (int(v) for v in str(spec.get("frame", "1280x720")).split("x"))
    hits = []
    count = len(re.findall(r"\S+", spec.get("headline", "")))
    if not 2 <= count <= 4:
        hits.append(f"headline of {count} words (2-4)")
    box = spec.get("subject")
    if not box:
        hits.append("no focal subject box")
    else:
        share = box["w"] * box["h"] / (width * height)
        if not 0.40 <= share <= 0.60:
            hits.append(f"focal subject fills {share:.0%} (40-60%)")
    if spec.get("people", 0) > 3:
        hits.append(f"{spec['people']} people (0-3)")
    if spec.get("headline_baked"):
        hits.append("headline baked into the generated image; add it as an overlay")
    return qc.report("thumb", qc.FAIL if hits else qc.PASS, hits=hits)


def cmd_pptxlint(qc, args) -> int:
    hits, path = [], Path(args.file)
    if path.suffix == ".pptx":
        with zipfile.ZipFile(path) as deck:
            size = re.search(r'<p:sldSz[^>]*cx="(\d+)"[^>]*cy="(\d+)"', deck.read("ppt/presentation.xml").decode()) \
                if "ppt/presentation.xml" in deck.namelist() else None
            for name in sorted(n for n in deck.namelist() if re.match(r"ppt/slides/slide\d+\.xml$", n)):
                xml = deck.read(name).decode("utf-8", errors="replace")
                for para in re.findall(r"<a:p>.*?</a:p>|<a:p .*?</a:p>", xml, re.S):
                    if re.match(r"\s*[\u2022\u25cf\u25aa]", "".join(re.findall(r"<a:t>([^<]*)</a:t>", para))):
                        hits.append(f"{name}: typed bullet; use native bullets")
                if size:
                    cx, cy = int(size.group(1)), int(size.group(2))
                    for x, y, w, h in re.findall(r'<a:off x="(-?\d+)" y="(-?\d+)"/>\s*<a:ext cx="(\d+)" cy="(\d+)"', xml):
                        if int(x) < 0 or int(y) < 0 or int(x) + int(w) > cx or int(y) + int(h) > cy:
                            hits.append(f"{name}: shape off the slide canvas")
    else:
        text = path.read_text(encoding="utf-8", errors="replace")
        checks = {
            "hash-hex": r"(?:color|fill|fontFace|line)\w*\s*:\s*['\"]#[0-9a-fA-F]{3,8}['\"]",
            "8-digit-hex": r"['\"]#?[0-9a-fA-F]{8}['\"]",
            "typed-bullet": r"['\"`]\s*[\u2022\u25cf]\s",
            "negative-shadow-offset": r"offset\s*:\s*-\d",
        }
        for name, pattern in checks.items():
            hits += [f"{path}:{line_of(text, m.start())} {name}" for m in re.finditer(pattern, text)]
    return qc.report("pptxlint", qc.FAIL if hits else qc.PASS, hits=hits)


def cmd_arabiccss(qc, args) -> int:
    hits = []
    for file in files_of(args.path, CSS_LIKE):
        text = file.read_text(encoding="utf-8", errors="replace")
        for selector, body, start in rules(text):
            if not AR_SELECTOR.search(selector):
                continue
            if re.search(r"font-style\s*:\s*italic", body, re.I):
                hits.append(f"{file}:{line_of(text, start)} italic on Arabic")
            height = re.search(r"line-height\s*:\s*(\d*\.?\d+)\s*(?:;|$)", body)
            if height and float(height.group(1)) < args.min_line_height:
                hits.append(f"{file}:{line_of(text, start)} line-height {height.group(1)} on Arabic")
            if re.search(r"direction\s*:\s*ltr", body, re.I):
                hits.append(f"{file}:{line_of(text, start)} direction ltr on an Arabic block")
        for match in re.finditer(r"<(\w+)[^>]*\bstyle=['\"][^'\"]*font-style\s*:\s*italic[^>]*>([^<]*)", text, re.I):
            if ARABIC.search(match.group(2)):
                hits.append(f"{file}:{line_of(text, match.start())} italic on Arabic")
        for match in re.finditer(r"<(\w+)[^>]*\bdir=['\"]ltr['\"][^>]*>([^<]*)", text, re.I):
            if ARABIC.search(match.group(2)):
                hits.append(f"{file}:{line_of(text, match.start())} Arabic text inside dir=ltr")
    return qc.report("arabiccss", qc.FAIL if hits else qc.PASS, hits=hits)


def cmd_alprefix(qc, args) -> int:
    hits = []
    for file in files_of(args.path, {".srt", ".vtt", ".txt", ".md", ".html", ".htm", ".json"}):
        for number, line in enumerate(file.read_text(encoding="utf-8", errors="replace").splitlines(), 1):
            if AL_LATIN.search(line):
                hits.append(f"{file}:{number} Arabic article bound to a Latin word")
    return qc.report("alprefix", qc.FAIL if hits else qc.PASS, hits=hits)


def image_size(qc, path: Path) -> tuple[int, int]:
    head = path.read_bytes()[:32]
    if head[:8] == b"\x89PNG\r\n\x1a\n":
        return struct.unpack(">II", head[16:24])
    stream = next(s for s in qc.ffprobe(str(path))["streams"] if s.get("codec_type") == "video")
    return int(stream["width"]), int(stream["height"])


def cmd_ogimage(qc, args) -> int:
    hits, path = [], Path(args.file)
    if not path.is_file():
        return qc.report("ogimage", qc.FAIL, hits=["no OG image file"])
    width, height = image_size(qc, path)
    if (width, height) != (1200, 630):
        hits.append(f"{width}x{height}, want 1200x630")
    if args.spec:
        spec = json.loads(Path(args.spec).read_text(encoding="utf-8"))
        count = len(re.findall(r"\S+", spec.get("headline", "")))
        if not 1 <= count <= args.max_words:
            hits.append(f"headline of {count} words (max {args.max_words})")
        floor = math.ceil(min(width, height) * args.ratio * 10) / 10
        if spec.get("size", 0) < floor:
            hits.append(f"headline {spec.get('size', 0)} px under {floor} px")
        if spec.get("fg") and spec.get("bg") and qc.contrast_ratio(spec["fg"], spec["bg"]) < args.contrast:
            hits.append(f"contrast {qc.contrast_ratio(spec['fg'], spec['bg']):.2f} under {args.contrast}")
        if not spec.get("brand_mark", False):
            hits.append("no brand mark")
    return qc.report("ogimage", qc.FAIL if hits else qc.PASS, size=f"{width}x{height}", hits=hits)


def register(add, qc) -> None:
    """Add the surface subcommands to design_qc's parser."""
    def bind(func):
        return lambda args: func(qc, args)

    p = add("spacing", bind(cmd_spacing), "margin/padding/gap px values off the spacing scale")
    p.add_argument("path")
    p.add_argument("--scale", default="4,8,16,24,32,48,64")
    p = add("tokens", bind(cmd_tokens), "primitive colour tokens used in component files")
    p.add_argument("path")
    p = add("a11yscan", bind(cmd_a11yscan), "positive tabindex, zoom lock, transition: all, ease-in, long UI motion")
    p.add_argument("path")
    p.add_argument("--max-ms", type=float, default=300.0)
    p = add("p3fallback", bind(cmd_p3fallback), "display-p3 colour without an sRGB value first")
    p.add_argument("path")
    p = add("oklch", bind(cmd_oklch), "OKLCH ramp hue drift, status-vs-accent hue gap, lightness pairs")
    p.add_argument("file")
    p.add_argument("--drift", type=float, default=10.0)
    p.add_argument("--sep", type=float, default=15.0)
    p = add("sidecar", bind(cmd_sidecar), "sidecar caption file: UTF-8, WEBVTT header, order, lines; CPS warns")
    p.add_argument("file")
    p.add_argument("--max-lines", type=int, default=2)
    p.add_argument("--max-chars", type=int, default=42)
    p.add_argument("--warn-cps", type=float, default=20.0)
    p = add("thumb", bind(cmd_thumb), "thumbnail spec: 2-4 word overlay headline, subject 40-60%%, 0-3 people")
    p.add_argument("file")
    p = add("pptxlint", bind(cmd_pptxlint), "pptxgenjs source or .pptx: hex, bullets, shadow offset, off-canvas")
    p.add_argument("file")
    p = add("arabiccss", bind(cmd_arabiccss), "Arabic CSS: no italic, line-height floor, no ltr block")
    p.add_argument("path")
    p.add_argument("--min-line-height", type=float, default=1.3)
    p = add("alprefix", bind(cmd_alprefix), "Arabic article (al-, bil-, lil-) bound to a Latin word")
    p.add_argument("path")
    p = add("ogimage", bind(cmd_ogimage), "OG image exists at 1200x630; headline words, size and contrast")
    p.add_argument("file")
    p.add_argument("--spec")
    p.add_argument("--max-words", type=int, default=6)
    p.add_argument("--ratio", type=float, default=0.026)
    p.add_argument("--contrast", type=float, default=4.5)
