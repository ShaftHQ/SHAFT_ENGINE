---
name: graphic-design
description: Use when a poster, OG card, diagram, slide, or still needs composition and type craft.
---

# C02 Graphic design (craft)

## Use when

A poster, OG card, diagram, slide, or still needs composition and type craft. Load after the matching D-card; this card raises the ceiling from correct to expert.

## Principles

1. One focal point per frame, at most three levels of emphasis.
2. A grid you can name (columns, baseline, margins) and break once, on purpose.
3. Type is image: scale contrast carries the voice.
4. Negative space is a shape, not leftover.

## Do

- Squint test: the focal point survives a 4px blur.
- Pair one serif or grotesk family with weights, not many families.
- Align to a baseline; optical-adjust large type (tighten tracking at display sizes).

## Don't

- Kicker above headline above body in a deck layout.
- Stat slide: centred big number over a small label (invented proof).
- Drop shadows and outlines to rescue weak contrast.

## Numeric targets

| Target | Value | Source | Check |
| --- | --- | --- | --- |
| Type steps | ratio 1.25-1.618 for display systems | Thinking with Type; Utopia | `typescale --min-ratio 1.25` |
| Distinct sizes | <= 5 per piece | Refactoring UI | `typescale --max-sizes 5` |
| Margins | >= 5% of short side | Müller-Brockmann grid practice | review |
| Display tracking | -1% to -3% above 48px | Practical Typography | review |
| Contrast | 4.5:1 text, 3:1 large | WCAG 2.2 | `contrast` |

## Anti-patterns

- Everything bold.
- Centred text blocks longer than 3 lines.
- Generated text inside images.

## Rubric

Score each deliverable against these anchors; quote the evidence (frame, check output) for the score. Scores never gate delivery: verified findings do.

- **1 (fail):** Cluttered, no focal point, mixed grids.
- **3 (competent):** Clean grid and hierarchy; reads in 2 s; derivative.
- **5 (expert):** A concept that reads in 1 s, survives at thumbnail size, and rewards a second look.

## Verify

- `design_qc.py image|contrast|typescale <file>`

## References

Twenty verified references with techniques: [research/graphic-design.md](../research/graphic-design.md).
