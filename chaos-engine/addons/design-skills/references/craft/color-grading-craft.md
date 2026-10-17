---
name: color-grading-craft
description: Use when a film or capture needs a look beyond the D14 conform.
---

# C05 Colour grading (craft)

## Use when

A film or capture needs a look beyond the D14 conform. Load after the matching D-card; this card raises the ceiling from correct to expert.

## Principles

1. Balance first (neutral greys, matched shots), then look.
2. Grade in a wide scene space; apply the display transform last.
3. One palette logic per film: a key hue, a complement, and controlled saturation.
4. Skin and UI whites are sacred: never push them off-neutral.

## Do

- Check waveform and vectorscope before eyes.
- Match captures to the same white point before stylising.
- Build looks as small, reversible nodes.

## Don't

- Teal-orange on screen recordings.
- Crushed blacks that kill UI detail.
- Untagged output (wrong BT.709 tags shift colours on the web).

## Numeric targets

| Target | Value | Source | Check |
| --- | --- | --- | --- |
| Output standard | BT.709 primaries, transfer, matrix tags | ITU-R BT.709 | `colortags` |
| Legal range | 16-235 luma for limited | ITU-R BT.709 | `levels` |
| UI white | delta-E < 3 vs source capture | colour-science practice | review |
| Saturation on UI | <= source + 10% | colourist practice (Cullen Kelly) | review |

## Anti-patterns

- A LUT dropped on everything.
- Shot-to-shot white-point drift.
- Banding from 8-bit grading of gradients.

## Rubric

Score each deliverable against these anchors; quote the evidence (frame, check output) for the score. Scores never gate delivery: verified findings do.

- **1 (fail):** Mismatched shots, broken whites, untagged.
- **3 (competent):** Balanced, matched, tagged; neutral.
- **5 (expert):** A deliberate look that serves the story and never harms legibility.

## Verify

- `design_qc.py colortags|levels <file>`

## References

Twenty verified references with techniques: [research/color-grading-craft.md](../research/color-grading-craft.md).
