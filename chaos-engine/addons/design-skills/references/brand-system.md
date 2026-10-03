---
name: brand-system
description: Use when work needs brand tokens, logo usage, or voice rules shared across web, image, and video output.
---

# D02 Brand system

## Use when

Any visual output must look like the same product: colors, type, spacing,
radius, motion, logo, and tone. Run before D03, D07, D09, D10, and D14.

## Inputs

The project's brand files when they exist (a `brand/` folder, a theme JSON,
CSS custom properties, a logo set). The ChaosEngine source keeps its own
palette in `assets/brand/` (repository only). Project packs may point at
their own files.

## Token schema (`theme.json`)

```json
{"color": {"bg": "#0f1115", "surface": "#171a21", "text": "#e8eaf0",
           "muted": "#a3a9b8", "accent": "#5b8cff", "danger": "#ff5d5d"},
 "type": {"display": "Inter Tight", "body": "Inter", "mono": "JetBrains Mono",
          "scale": 1.25, "base": 16},
 "space": [4, 8, 12, 16, 24, 32, 48, 64], "radius": {"sm": 4, "md": 8},
 "motion": {"enter": {"ms": 400, "ease": "cubic-bezier(0.05,0.7,0.1,1)"},
            "exit": {"ms": 200, "ease": "cubic-bezier(0.3,0,0.8,0.15)"}},
 "contrastPairs": [{"fg": "#e8eaf0", "bg": "#0f1115", "kind": "text"}]}
```

## Rules

- No color, font, radius, or easing outside the tokens. A new need becomes a
  new token with a reason, reviewed by the owner.
- Light and dark themes are both specified; a role missing in one theme is a
  defect.
- Logo: keep clear space of at least the logo's cap height on every side,
  respect the minimum size, use the light or dark lockup that passes contrast.
  Never recolor, stretch, outline, or add effects.
- Voice: plain, specific, verifiable. Name the reader's outcome, then the
  feature. No hype words the evidence cannot back.
- Fonts need a recorded licence (OFL or owned) before use in renders.

## Workflow

1. Look for existing brand sources; do not invent a brand when one exists.
2. If none exist, derive a minimal token set from the shipped CSS and mark it
   "proposed" for owner review.
3. Publish tokens as CSS variables for web and HyperFrames, and as Python or
   JSON constants for Manim and image templates.

## Verify

- `design_qc.py contrast --tokens theme.json` passes for every declared pair.
- Sampled frames or images use token colors only (eyedropper or palette
  extraction, documented in the PR).
- Logo clear space and minimum size measured on the rendered asset.

## Sources

Re-expressed with attribution from Anthropic `brand-guidelines` and
`theme-factory` (Apache-2.0); see INVENTORY.md.
