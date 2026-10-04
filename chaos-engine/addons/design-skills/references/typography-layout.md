---
name: typography-layout
description: Use when text hierarchy, type scale, line length, or grid layout decides whether a page, image, or frame reads well.
---

# D05 Typography and layout

## Use when

Setting or fixing a type scale, body measure, vertical rhythm, or grid on a
page, poster, slide, or video frame.

## Rules for screens

- Modular scale from the token `type.scale` (1.2 to 1.333); sizes come from
  the scale, not ad-hoc pixel values.
- Body measure 45 to 90 characters; 60 to 75 is the target.
- Body text at least 16 CSS px; never sized by viewport width alone (use
  `clamp()` with a rem floor).
- Line height 1.4 to 1.6 for body, 1.1 to 1.25 for display.
- Hierarchy by size, weight, and contrast. Two typefaces at most plus mono.
- Grid: 4 or 8 px spacing unit, 12-column web grid, consistent gutters.
  Content aligns to the grid; decoration may break it once, on purpose.

## Rules for images and video frames

- Minimum glyph cap height 24 px at 1080p (48 px at 2160p) for any text the
  viewer must read.
- Keep text inside a 5% safe margin; vertical video adds the platform UI
  zones from D20.
- One idea per frame; at most about seven words of on-screen text per beat.
- Code and command text at least 40 px at 1080p (about 3.7% of frame
  height) and legible in the 9:16 cut. Wrap only at valid shell
  continuations (bash `\`, PowerShell after a pipe, operator, or backtick)
  or split into steps; never inside a quoted string or URL.

## Script and language notes

- CJK text: measure counts characters, target 30 to 40 per line.
- RTL: mirror layout with logical properties, never hard-coded left/right.
- Code blocks are exempt from the measure rule but must scroll inside their
  own box, never the page.

## Workflow

1. Read the type tokens; derive the scale table.
2. Apply sizes and measure; check every ui-delivery viewport.
3. For frames and images, render at the delivery size and inspect at 100%.

## Verify

- An e2e test asserts body line length (characters per line from
  `getBoundingClientRect()` and font metrics) between 45 and 90 at each
  viewport, and `scrollWidth <= innerWidth`.
- Computed font sizes match the scale within 2%.
- Every font file has a recorded licence in the manifest.
- The smallest code frame is inspected at 1080p and in the 9:16 cut.

## Sources

Butterick, *Practical Typography*; Müller-Brockmann, *Grid Systems in
Graphic Design* (paraphrase only); Material 3 type scale.
