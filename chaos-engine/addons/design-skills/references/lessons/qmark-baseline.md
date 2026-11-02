---
name: qmark-baseline
description: Use when a mixed-font title or question band draws a trailing ?/؟ that must share the body baseline.
---

# L12 Question-mark baseline

## Use when

A burned band or title line mixes typefaces (for example Arabic body +
Latin `?` fallback) and the trailing mark must sit on the same baseline
as the body. Extends [L02](mixed-script-captions.md) and
[L07](arabic-rtl-type-mirroring.md).

## Rules

1. **One typographic baseline for the whole line.** Draw body and
   trailing `?` / `؟` with the same baseline anchor (Pillow / skia
   `anchor=ls`, or equivalent "left/baseline" alignment). Never place the
   mark with a top or mid anchor that floats it as a superscript.
2. **Gate the rendered PNG**, not the draw call. Measure ink: body
   baseline ≈ median of per-glyph ink bottoms (drop deepest ~25% for
   descenders). Fail when
   `|qmark_ink_bottom − body_baseline| / line_h > 0.06`.
3. **Side follows script.** LTR → rightmost narrow glyph is the mark;
   RTL → leftmost. Pass `--direction` or a prepared sidecar when the
   frame is ambiguous.
4. **Why 6%?** Inter / Cairo ascent mismatch routinely lifts ASCII `?`
   by more than a descender; 6% of line height catches that lift without
   failing normal optical rounding.

## Verify

- [ ] `design_qc.py qmarkbaseline band.png --direction ltr` fails on a
      synthetic frame where the trailing `?` sits high, and passes when
      body and mark share `anchor=ls`.
- [ ] Same for RTL with trailing `؟` on the left.
- [ ] Contact sheet: mark bottoms align with the body baseline across
      Latin-only and Arabic bands.

## Why

Fallback fonts for ASCII `?` often have a different ascent than the
Arabic body face. Drawing both on `ls` (baseline) keeps the mark from
reading as a raised footnote; the ink gate makes the bug visible in CI.

## Sources

- Field lesson from a content QA recut batch (Coach SIGN r4 / baseline
  align): mixed-font lines share one baseline (`anchor=ls`); gate final
  `?`/`؟` bottom vs body baseline, fail above 6% of line height.
