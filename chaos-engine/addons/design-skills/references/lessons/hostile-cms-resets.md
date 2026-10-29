---
name: hostile-cms-resets
description: Use before SIGN of a hosted-CMS Code-block page; simulate hostile global CSS and gate heading colour plus identity squircle.
---

# L26 Hostile CMS resets and identity tile

## Use when

A product embed must survive hosted-builder global type/colour and
`img{width:100%}` overrides. Playbook:
[hosted-cms-embed](../site-builders/hosted-cms-embed.md).

## Rules

1. **Hostile injection before SIGN.** Inject global heading colour/font
   hijack, visible skip spoiler, and `img{width:100%}` (see
   [`hostile_cms_css.py`](../../scripts/hostile_cms_css.py)).
2. **Near-white headings on dark.** `h1–h3` on dark surfaces compute
   relative luminance ≥ **0.85** (or RGB ≥ 230 each) under hostile CSS.
3. **Identity squircle.** Light tile (near-white background) holds a dark
   mark inset; tile aspect ≈ 1 ± tolerance; mark bbox fully inside tile
   under hostile sizing. Lock tile/img with `!important`.
4. **Accent only on chrome.** Product accent on pill/CTA only — never on
   dark-surface headings or body text.

## Verify

- [ ] `design_qc.py hostilecms --heading-color …` fails a blue/dark heading
      and passes near-white.
- [ ] `design_qc.py hostilecms --tile-w … --tile-h … --img-…` fails a
      non-square or overflowing mark.
- [ ] Gates PASS at 390 / 768 / 1440 both normal and hostile.

## Why

Hosted builders inject global type and stretch images. Without a reset
root and hostile rehearsal, blue slab headings and invisible dark-on-navy
marks ship.

## Sources

Field lesson from a hosted-CMS embed redesign: hostile CSS plus squircle
and heading-colour gates closed live rejects on colour and identity tile.
