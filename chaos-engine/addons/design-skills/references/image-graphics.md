---
name: image-graphics
description: Use when producing posters, social or OG cards, video thumbnails, icons, or technical diagrams as deterministic files.
---

# D07 Image graphics

## Use when

A static visual ships: an Open Graph card, a video thumbnail, a social
post, a poster, an icon, or an architecture diagram.

## Size presets

| Asset | Pixels | Notes |
| --- | --- | --- |
| Open Graph / link card | 1200 x 630 | under 5 MB, key text centered |
| Video thumbnail | 1280 x 720 (also 3840 x 2160) | readable at 320 px wide, under 2 MB |
| Square social | 1080 x 1080 | |
| Vertical story | 1080 x 1920 | keep platform UI zones clear (D20) |

## Rules

- Text is rendered, never generated: compose in HTML/CSS or SVG and render
  with headless Chrome (`--screenshot --window-size=W,H`) or `rsvg-convert`.
  Generated imagery never contains words, code, or UI.
- State a one-sentence design idea before composing (what the viewer should
  notice first, second, third).
- Keep all text inside a 5% safe margin; no element overlaps another unless
  the overlap is the idea.
- Thumbnails: three to five words, one face or one object, strong figure and
  ground contrast, no clutter in the bottom-right timestamp area.
- Diagrams: source committed (SVG, D2, or Mermaid), consistent stroke width,
  token colors, a `<title>` and `<desc>` in SVG, alt text where embedded.
- Third-party logos only with permission or under their published usage
  rules.
- Long titles fail closed: rewrite the copy instead of shrinking below the
  D05 minimum glyph height.

## Workflow

1. Pick the preset and write the design idea.
2. Build an HTML or SVG template wired to the brand tokens (D02).
3. Render, view at 100% and at the smallest display size, adjust, re-render.
4. Record the source file, fonts, and licences in the manifest.

## Verify

- `design_qc.py image out.png --size 1280x720 --max-bytes 2097152` passes.
- `design_qc.py contrast` passes for text and background colors used.
- Thumbnail copy is legible at 320 px wide (downscale and check; OCR with
  tesseract when installed).
- SVG passes `xmllint --noout` and `design_qc.py image file.svg`.

## Sources

Re-expressed with attribution from Anthropic `canvas-design` and
`algorithmic-art` (Apache-2.0) and leonxlnx/taste-skill (MIT); platform
thumbnail guidance.
