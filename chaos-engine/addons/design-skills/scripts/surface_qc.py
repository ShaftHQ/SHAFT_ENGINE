"""Surface checks for design_qc.py: web tokens, a11y, colour, captions, stills, decks, Arabic and OG images.

Sources: the batch-3 cards W07-W14, V10, I01, S04 and L07 (re-expressed rules, pins in INVENTORY.md).
Each check is red-then-green tested in tests/scripts/test_ce_design_batch3.py (deck review checks: test_ce_design_decks.py).
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
# Caption form from L01/L02: tatweel then a space before the Latin word («الـ X») is accepted in .srt/.vtt.
AL_ATTACHED = re.compile(r"(?:^|[\s(«\"])(?:وبال|فبال|بال|كال|فال|وال|لل|ال)(?:ـ)?[A-Za-z]")


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


def oklch_css(text: str) -> dict:
    """Read --name-step: oklch(...) custom properties into ramps, accent and status (pairs need JSON)."""
    spec: dict = {"ramps": {}, "status": {}}
    for name, value in re.findall(r"--([\w-]+)\s*:\s*(oklch\([^)]*\))", text):
        step = re.fullmatch(r"(.+?)-(\d{2,3})", name)
        if name == "accent":
            spec["accent"] = value
        elif re.fullmatch(r"(?:status-)?(?:error|danger|warning|success|info)", name):
            spec["status"][name] = value
        elif step:
            spec["ramps"].setdefault(step.group(1), []).append(value)
    return spec


def cmd_oklch(qc, args) -> int:
    text = Path(args.file).read_text(encoding="utf-8")
    spec = oklch_css(text) if Path(args.file).suffix in CSS_LIKE else json.loads(text)
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
        pattern = AL_ATTACHED if file.suffix in {".srt", ".vtt"} else AL_LATIN
        for number, line in enumerate(file.read_text(encoding="utf-8", errors="replace").splitlines(), 1):
            if pattern.search(line):
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


# ---------------------------------------------------------------- deck review checks (#7418)
EMU_PT = 12700.0
DECK_SLIDE = re.compile(r"ppt/slides/slide(\d+)\.xml$")


def _attr(tag: str, name: str) -> str | None:
    found = re.search(rf'(?<![\w:-]){name}="([^"]*)"', tag)
    return found.group(1) if found else None


def _deck_slides(deck: zipfile.ZipFile) -> list[tuple[int, str]]:
    found = [(int(m.group(1)), n) for n in deck.namelist() if (m := DECK_SLIDE.match(n))]
    return sorted(found)


def cmd_cxnglue(qc, args) -> int:
    """Connectors are glued at both ends (stCxn and endCxn) and not shorter than --min-length pt."""
    hits = []
    with zipfile.ZipFile(args.file) as deck:
        slides = _deck_slides(deck)
        for number, name in slides:
            xml = deck.read(name).decode("utf-8", errors="replace")
            for index, block in enumerate(re.findall(r"<p:cxnSp\b.*?</p:cxnSp>", xml, re.S), 1):
                label = _attr(block, "name") or f"connector {index}"
                for end, tag in (("start", "a:stCxn"), ("end", "a:endCxn")):
                    if not re.search(rf"<{tag}\b", block):
                        hits.append(f"slide {number} {label}: {end} not glued ({tag} missing)")
                ext = re.search(r"<a:ext\b([^>]*)/?>", block)
                if ext and args.min_length:
                    cx, cy = float(_attr(ext.group(1), "cx") or 0), float(_attr(ext.group(1), "cy") or 0)
                    length = max(cx, cy) / EMU_PT
                    if length < args.min_length:
                        hits.append(f"slide {number} {label}: {length:.1f} pt long (min {args.min_length:g})")
    return qc.report("cxnglue", qc.FAIL if hits else qc.PASS, slides=len(slides), hits=hits)


def _deck_facts(path: str) -> dict:
    """Per-slide notes text and external hyperlink targets, plus the typefaces used, from a .pptx."""
    facts: dict = {"notes": {}, "links": {}, "fonts": set()}
    with zipfile.ZipFile(path) as deck:
        slides = _deck_slides(deck)
        names = set(deck.namelist())
        for position, (number, name) in enumerate(slides, 1):
            xml = deck.read(name).decode("utf-8", errors="replace")
            facts["fonts"].update(re.findall(r'<a:(?:latin|ea|cs)\b[^>]*typeface="([^"]+)"', xml))
            rels_name = f"ppt/slides/_rels/slide{number}.xml.rels"
            rels = deck.read(rels_name).decode("utf-8", errors="replace") if rels_name in names else ""
            links, notes = [], ""
            for tag in re.findall(r"<Relationship\b[^>]*>", rels):
                target, kind = _attr(tag, "Target") or "", _attr(tag, "Type") or ""
                if kind.endswith("/hyperlink") and _attr(tag, "TargetMode") == "External":
                    links.append(target)
                elif kind.endswith("/notesSlide"):
                    notes_name = "ppt/notesSlides/" + target.rsplit("/", 1)[-1]
                    if notes_name in names:
                        notes = _notes_text(deck.read(notes_name).decode("utf-8", errors="replace"))
            facts["notes"][position], facts["links"][position] = notes, sorted(links)
    return facts


def _notes_text(xml: str) -> str:
    """Body-placeholder text of a notes slide (slide image and number fields are not notes)."""
    parts = []
    for shape in re.findall(r"<p:sp\b.*?</p:sp>", xml, re.S):
        if re.search(r'<p:ph\b[^>]*type="body"', shape):
            parts.append("".join(re.findall(r"<a:t>([^<]*)</a:t>", shape)))
    return re.sub(r"\s+", " ", " ".join(parts)).strip()


def cmd_pptxparity(qc, args) -> int:
    """After a re-upload or export: size under the cap, and (with --before) notes, links and fonts unchanged."""
    hits, size = [], Path(args.file).stat().st_size
    limit = args.max_bytes * (1 - args.headroom)
    if size > limit:
        hits.append(f"size {size} bytes above {limit:.0f} ({args.headroom:.0%} headroom under {args.max_bytes})")
    slides = None
    if args.before:
        old, new = _deck_facts(args.before), _deck_facts(args.file)
        slides = len(new["notes"])
        if len(old["notes"]) != slides:
            hits.append(f"slide count {len(old['notes'])} -> {slides}")
        for position in sorted(set(old["notes"]) & set(new["notes"])):
            if position not in args.allow and old["notes"][position] != new["notes"][position]:
                hits.append(f"slide {position}: speaker notes changed")
            if position not in args.allow and old["links"][position] != new["links"][position]:
                hits.append(f"slide {position}: hyperlinks changed {old['links'][position]} -> {new['links'][position]}")
        hits += [f"typeface removed: {f}" for f in sorted(old["fonts"] - new["fonts"])]
        hits += [f"typeface added: {f}" for f in sorted(new["fonts"] - old["fonts"])]
    return qc.report("pptxparity", qc.FAIL if hits else qc.PASS, size=size, slides=slides, hits=hits)


def _box(item: dict) -> tuple[float, float, float, float]:
    return float(item["x"]), float(item["y"]), float(item["w"]), float(item["h"])


def _pad_rows(entry: dict, default: dict, sym: float | None) -> tuple[list[str], dict]:
    ox, oy, ow, oh = _box(entry["outer"])
    ix, iy, iw, ih = _box(entry["inner"])
    got = {"l": ix - ox, "r": (ox + ow) - (ix + iw), "t": iy - oy, "b": (oy + oh) - (iy + ih)}
    floor = {**default, **entry.get("min", {})}
    hits = [f"{entry['id']}: padding {side} {got[side]:.1f} below {floor[side]:g}" for side in "lrtb" if got[side] < floor[side]]
    tol = entry.get("sym", sym)
    if tol is not None:
        hits += [f"{entry['id']}: padding {a}/{b} {got[a]:.1f} vs {got[b]:.1f} not within {tol:g}"
                 for a, b in ("lr", "tb") if abs(got[a] - got[b]) > tol]
    return hits, {k: round(v, 1) for k, v in got.items()}


def cmd_deckgap(qc, args) -> int:
    """Spacing contracts on measured (rendered ink) boxes: container padding, gaps, whole gap chains."""
    data = json.loads(Path(args.file).read_text(encoding="utf-8"))
    default = dict(zip("lrtb", (float(v) for v in args.pad.split(","))))
    hits, table = [], []
    for entry in data.get("pads", []):
        found, got = _pad_rows(entry, default, args.sym)
        hits += found
        table.append({"id": entry["id"], "padding": got})
    for entry in data.get("gaps", []):
        hits += _gap_rows(entry, entry["a"], entry["b"], entry.get("min", args.min_gap), table, entry["id"])
    for chain in data.get("chains", []):
        boxes = chain["boxes"]
        for before, after in zip(boxes, boxes[1:]):
            hits += _gap_rows(chain, before, after, chain.get("min", args.min_gap), table, f"{chain['id']}:{before['id']}>{after['id']}")
    if not table:
        raise qc.Skip("no pads, gaps or chains in the measurement file")
    return qc.report("deckgap", qc.FAIL if hits else qc.PASS, hits=hits, table=table)


def _gap_rows(entry: dict, first: dict, second: dict, floor: float, table: list, label: str) -> list[str]:
    axis = entry.get("axis", "y")
    ax, ay, aw, ah = _box(first)
    bx, by, bw, bh = _box(second)
    gap = (by - (ay + ah)) if axis == "y" else (bx - (ax + aw))
    table.append({"id": label, "gap": round(gap, 1)})
    return [f"{label}: gap {gap:.1f} below {floor:g}"] if gap < floor else []


def _pixels(qc, path: str) -> tuple[int, int, bytes]:
    stream = qc.video_stream(qc.ffprobe(path))
    width, height = int(stream["width"]), int(stream["height"])
    return width, height, qc.decode_raw(path, ["-frames:v", "1", "-f", "rawvideo", "-pix_fmt", "rgb24"])


def _changed_share(a: bytes, b: bytes, width: int, chan: int) -> float:
    row, over = width * 3, 0
    for start in range(0, len(a), row):
        left, right = a[start:start + row], b[start:start + row]
        if left == right:
            continue
        for i in range(0, row, 3):
            if abs(left[i] - right[i]) + abs(left[i + 1] - right[i + 1]) + abs(left[i + 2] - right[i + 2]) > chan:
                over += 1
    return 100.0 * over / (len(a) // 3)


def cmd_slidediff(qc, args) -> int:
    """Per-slide PNG diff between two rounds: only the slides listed in --expect may change."""
    before, after = {p.name: p for p in Path(args.before).glob("*.png")}, {p.name: p for p in Path(args.after).glob("*.png")}
    hits, changed = [f"{n}: missing from {side}" for side, a, b in (("after", before, after), ("before", after, before))
                     for n in sorted(set(a) - set(b))], {}
    for name in sorted(set(before) & set(after)):
        w1, h1, a = _pixels(qc, str(before[name]))
        w2, h2, b = _pixels(qc, str(after[name]))
        share = 100.0 if (w1, h1) != (w2, h2) else _changed_share(a, b, w1, args.chan)
        if share > args.min_pct:
            changed[name] = round(share, 3)
    expect = {e if e.endswith(".png") else f"{e}.png" for e in args.expect}
    hits += [f"{n}: changed {share}% of pixels, not expected" for n, share in changed.items() if n not in expect]
    hits += [f"{n}: expected to change but did not" for n in sorted(expect - set(changed)) if n in after]
    return qc.report("slidediff", qc.FAIL if hits else qc.PASS, compared=len(set(before) & set(after)), changed=changed, hits=hits)


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
    p = add("oklch", bind(cmd_oklch), "OKLCH ramps, status-vs-accent hue gap, lightness pairs; CSS custom properties or JSON")
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
    p = add("alprefix", bind(cmd_alprefix), "Arabic article (al-, bil-, lil-) bound to a Latin word in web/UI copy; captions accept the L02 form")
    p.add_argument("path")
    p = add("ogimage", bind(cmd_ogimage), "OG image exists at 1200x630; headline words, size and contrast")
    p.add_argument("file")
    p.add_argument("--spec")
    p.add_argument("--max-words", type=int, default=6)
    p.add_argument("--ratio", type=float, default=0.026)
    p.add_argument("--contrast", type=float, default=4.5)
    p = add("cxnglue", bind(cmd_cxnglue), ".pptx connectors glued at both ends (stCxn, endCxn) and long enough")
    p.add_argument("file")
    p.add_argument("--min-length", type=float, default=36.0, help="shortest connector in pt (0 disables)")
    p = add("pptxparity", bind(cmd_pptxparity), ".pptx export size under the cap; with --before: notes, links, typefaces unchanged")
    p.add_argument("file")
    p.add_argument("--before", help="the previous round's .pptx")
    p.add_argument("--max-bytes", type=int, default=10 * 1024 * 1024)
    p.add_argument("--headroom", type=float, default=0.05)
    p.add_argument("--allow", type=int, action="append", default=[], help="slide number whose notes or links may change")
    p = add("deckgap", bind(cmd_deckgap), "measured boxes JSON: container padding, gaps and gap chains")
    p.add_argument("file")
    p.add_argument("--pad", default="12,12,7,7", help="minimum padding l,r,t,b in pt")
    p.add_argument("--sym", type=float, default=None, help="opposite paddings may differ by at most this")
    p.add_argument("--min-gap", type=float, default=6.0)
    p = add("slidediff", bind(cmd_slidediff), "per-slide PNG diff between rounds; unexpected changes fail")
    p.add_argument("before")
    p.add_argument("after")
    p.add_argument("--expect", action="append", default=[], help="slide file (stem or name) that must change")
    p.add_argument("--chan", type=int, default=30, help="summed RGB difference that counts as a changed pixel")
    p.add_argument("--min-pct", type=float, default=0.02, help="percent of changed pixels that counts as a changed slide")
