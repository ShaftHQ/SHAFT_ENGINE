---
name: color-contrast
description: Use when choosing palettes or checking contrast for text, UI controls, captions, and overlays on images or moving video.
---

# D06 Color and contrast

## Use when

Building or changing a palette, adding a theme, or placing text over an
image, a gradient, or video.

## Thresholds (WCAG 2.2)

| Element | Minimum ratio | Criterion |
| --- | --- | --- |
| Body text | 4.5:1 | 1.4.3 |
| Large text (24 px, or 18.66 px bold) | 3:1 | 1.4.3 |
| Enhanced body text | 7:1 | 1.4.6 |
| UI components, focus rings, chart marks | 3:1 | 1.4.11 |

## Rules

- Build palettes in OKLCH: step lightness evenly, keep chroma moderate, and
  derive dark-theme roles by lightness, not by inversion.
- One accent role. Semantic roles (success, warning, danger) stay
  distinguishable under deuteranopia, protanopia, and tritanopia simulation;
  never encode meaning by hue alone (add an icon or a label).
- Avoid pure #000 and #fff surfaces; use near-black and near-white tokens.
- Text over video or photos sits on a scrim (a 60 to 80% opaque token-color
  band or a soft gradient) and is measured against the brightest frame in
  its span, not the average.
- A brand color that fails contrast gets a tonal variant for text use; record
  the variant in the token file.

## Workflow

1. List every foreground and background pair, per theme, in
   `theme.json` under `contrastPairs`.
2. Run the checker; adjust lightness until each pair passes.
3. For overlays, export the brightest frame under the text and measure the
   sampled colors.
4. Simulate color-vision deficiencies (browser devtools rendering emulation)
   and screenshot the result.

## Verify

- `design_qc.py contrast --tokens theme.json` passes, including UI pairs at
  3:1.
- Overlay pairs measured on the worst frame pass `design_qc.py contrast
  --pair fg:bg:large` or `:text`.
- Color-vision simulation screenshots attached; semantic roles remain
  distinguishable.

## Sources

WCAG 2.2 success criteria 1.4.3, 1.4.6, and 1.4.11; CSS Color Module
Level 4 (OKLCH).
