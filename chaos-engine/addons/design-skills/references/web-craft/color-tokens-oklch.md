---
name: color-tokens-oklch
description: Use when building or reviewing a colour palette, design tokens, OKLCH ramps, P3 colour or a dark mode.
---

# W08 Colour tokens, OKLCH and dark mode

## Use when

A palette, token set or dark theme is made or reviewed. Contrast floors
stay in [D06 colour and contrast](../color-contrast.md).

## Rules

1. **Two tiers:** primitives (`--blue-600`) are never used in components;
   components use semantic tokens only (`--accent`, `--surface-2`).
2. **Role-driven ramps:** only generate steps a role uses; light-end
   steps about 0.04-0.05 OKLCH L apart.
3. **One hue, one purpose:** hues within 15° OKLCH are the same colour; a
   status hue sits at least 15° from the accent.
4. **Hue drift:** more than 10° hue spread across one ramp is drift.
5. **Lightness pairs:** bg L above 0.85 needs fg L under 0.45; bg L under
   0.25 needs fg L over 0.75. APCA |Lc| 60 body (75 preferred) is a
   second opinion beside WCAG, never a replacement.
6. **Alpha text:** text over alpha colour (`/50`, `rgba`, `color-mix`
   with transparent) is measured on the rendered composite. White on a
   500 fill usually fails; use 600.
7. **P3:** declare the sRGB value first, the vivid value inside
   `@media (color-gamut: p3)`.
8. **Dark mode:** 4 elevation surfaces from about `#121212` up, primaries
   desaturated 10-20%, text off-white (about `#E0E0E0`), not `#FFF`.
9. **Never report a contrast value you did not measure.**

## Verify

- [ ] `design_qc.py tokens src/` (no primitive tokens in components)
- [ ] `design_qc.py oklch tokens.css` (reads `--blue-600: oklch(...)`,
      `--accent`, `--error`) or `oklch palette.json` (adds fg/bg pairs)
- [ ] `design_qc.py p3fallback src/` and `design_qc.py contrast --tokens theme.json`
- [ ] Alpha text measured from a rendered screenshot.

## Sources

- [jakubkrehel/skills better-colors](https://github.com/jakubkrehel/skills/tree/019ab7c199154f4509b55f12d84a4f5b578dc606)
  @ `019ab7c199154f4509b55f12d84a4f5b578dc606` (MIT, adapted).
- [jakubkrehel/oklch-skill](https://github.com/jakubkrehel/oklch-skill/tree/0e81f040e16d48fcc61d3e1b7ea548542371321f)
  @ `0e81f040e16d48fcc61d3e1b7ea548542371321f` (MIT, adapted).
- Dark mode: owl-listener/designer-skills @ `9a6930cf84a822eb458624bd11c61aac5bbdf224` (MIT).
