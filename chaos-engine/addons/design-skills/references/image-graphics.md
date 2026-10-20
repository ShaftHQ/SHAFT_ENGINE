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
| Video thumbnail | 1280 x 720 (also 3840 x 2160) | under 2 MB; reads at 168 x 94; bottom-right ~180 x 60 empty |
| Square social | 1080 x 1080 | |
| Vertical story | 1080 x 1920 | keep platform UI zones clear (D20) |
| Short cover | 1080 x 1920 | key content out of top ~250 px, bottom 25 %, right 15 % |

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

## Thumbnails and covers

- Design the thumbnail: photo on one side (face about 35-45 % of the
  height), a 3-5 word hook faithful to the video's real words on the other,
  hook cap height at least 12 % of the frame, small brand mark and badge.
- A vertical cover is designed in the same system, never a picked video
  frame: mid-speech frames blink, carry captions, and ignore safe zones.

## Photo frames

- Sample candidates (4 fps) from every take; score sharpness (Laplacian
  variance on the face box), open eyes, and face exposure; keep the top few.
- Glasses: count cyan/green highlight pixels on the lens band and inspect a
  close-up of the eyes; white ring-light arcs escape colour detection, so
  the visual check is required.
- Extract the chosen frame fresh and pixel-diff the photo crop and the
  final render against it (max diff 0 where unmasked) so no debug overlay
  or stale frame leaks; sample the same way the candidates were scored.

## Workflow

1. Pick the preset and write the design idea.
2. Build an HTML or SVG template wired to the brand tokens (D02).
3. Render, view at 100% and at the smallest display size, adjust, re-render.
4. Record the source file, fonts, and licences in the manifest.

## Verify

- `design_qc.py image out.png --size 1280x720 --max-bytes 2097152` passes.
- `design_qc.py contrast` passes for text and background colors used.
- Thumbnail hook and face read at 168 x 94 (downscale and look; OCR with
  tesseract when installed).
- Eye close-up shows open eyes and no lens highlights; the fresh-frame
  pixel diff is 0.
- SVG passes `xmllint --noout` and `design_qc.py image file.svg`.

## Sources

Re-expressed with attribution from Anthropic `canvas-design` and
`algorithmic-art` (Apache-2.0) and leonxlnx/taste-skill (MIT); platform
thumbnail guidance.
