---
name: web-typography-measured
description: Use when setting or reviewing web type values: line-height, weights, tracking, wrapping, numerals, font loading, input and UI floors.
---

# W06 Measured web type

## Use when

Web text values are set or reviewed. Extends [D05 typography and
layout](../typography-layout.md) and [W04 micro-polish](micro-polish.md).

## Rules

1. **Unitless line-height:** display 1.1, headings 1.2-1.3, body
   1.5-1.6; any text that wraps to 3 lines or more at least 1.4.
2. **Weights:** 100-300 only at 28 px and up; below 18 px weight 400 or
   heavier.
3. **Tracking in em:** headings at 24 px and up -0.01em to -0.02em;
   uppercase labels at 14 px or smaller +0.05em; body 0. Never
   `font-kerning: none`.
4. **Wrapping:** `text-wrap: balance` on headings only (Chromium ignores
   it past 6 lines); `text-wrap: pretty` on paragraphs; `text-align:
   start`, no justified UI text.
5. **Properties over raw settings:** `font-weight: 650`,
   `font-variant-numeric: tabular-nums`; tabular numerals on timers,
   counters, amounts and numeric columns, proportional in prose.
6. **Load every face used:** no synthesized bold or italic; turn synthesis
   off only per longhand, after checking the fallback stack.
7. **Floors:** inputs 16 px on iOS (or keep 16 px and scale); UI menus
   14 px, captions 13 px, rarely under 12 px; sizes in rem.
8. **Edges:** `text-box: trim-both cap alphabetic` on buttons and badges
   (progressive enhancement); underlines `from-font`.
9. **Punctuation:** real typographic punctuation in rendered strings (…
   – — and curly quotes); code keeps straight quotes. In RTL caption
   lines, L02 rule 9 wins: neutrals touching a Latin run go inside the
   isolate or use ASCII.
10. **Severity:** unreadable text, truncation with no way back, or text
    clipped at 320 px width or 200% zoom is **Floor** (blocks delivery).
    Arabic text keeps letter-spacing normal ([L07](../lessons/arabic-rtl-type-mirroring.md)).

## Verify

- [ ] `design_qc.py typescale page.css` and `design_qc.py measure page.css`
- [ ] `design_qc.py artrack page/` (no tracking on Arabic runs)
- [ ] Checked at 320 px width and 200% zoom; inputs 16 px on iOS.
- [ ] Every weight and style used is loaded (no synthesis in DevTools).

## Sources

- [jakubkrehel/skills better-typography](https://github.com/jakubkrehel/skills/tree/019ab7c199154f4509b55f12d84a4f5b578dc606/skills/better-typography)
  @ `019ab7c199154f4509b55f12d84a4f5b578dc606` (MIT, adapted).
