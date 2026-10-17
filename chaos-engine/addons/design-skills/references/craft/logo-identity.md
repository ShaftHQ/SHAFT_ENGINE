---
name: logo-identity
description: Use when a mark, wordmark, or identity system is designed or applied.
---

# C03 Logo design and identity (craft)

## Use when

A mark, wordmark, or identity system is designed or applied. Load after the matching D-card; this card raises the ceiling from correct to expert.

## Principles

1. Simple, distinctive, appropriate, and enduring: the mark works at 16px and in one colour.
2. Identity is a system: mark, type, colour, motion behaviour, voice.
3. Official marks only for third parties; never redraw someone else's logo.
4. The mark's motion comes from its construction (how it is drawn), not from a preset.

## Do

- Test at 16, 32, 64px, mono, reversed, and on photo.
- Define clear space (often the height of a key stroke) and minimum size.
- Write misuse examples next to usage.

## Don't

- The name's first association (rocket for launch, bolt for fast).
- Gradients that break in one colour.
- A dot that becomes the logo as the default reveal.

## Numeric targets

| Target | Value | Source | Check |
| --- | --- | --- | --- |
| Minimum size | legible at 16px favicon | CGH test; platform icon specs | render test |
| Clear space | >= 1 x key-stroke height | NASA Graphics Standards Manual | review |
| Colours in mark | <= 2 plus mono version | identity practice (Brand New critiques) | manifest |
| Mono contrast | 3:1 against ground | WCAG 1.4.11 | `contrast` |

## Anti-patterns

- Swoosh-and-globe generic tech marks.
- Wordmark in a default system font with no adjustment.
- Logo held on an end card past the brief.

## Rubric

Score each deliverable against these anchors; quote the evidence (frame, check output) for the score. Scores never gate delivery: verified findings do.

- **1 (fail):** Generic or derivative; fails at 16px.
- **3 (competent):** Clean, scalable, consistent; forgettable.
- **5 (expert):** Ownable idea, works at every size and in one colour, with a motion behaviour born from its form.

## Verify

- `design_qc.py contrast; render at 16/32/64px and inspect`

## References

Twenty verified references with techniques: [research/logo-identity.md](../research/logo-identity.md).
