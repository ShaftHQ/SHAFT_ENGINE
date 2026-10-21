---
name: training-decks
description: Use when designing, re-theming or reviewing a training or course deck (Google Slides or a .pptx headed there).
---

# C12 Training decks (craft)

## Use when

A teaching deck needs a visual system, a layout set, proof mocks or a
chapter review. Load [brand system](../brand-system.md) and
[typography and layout](../typography-layout.md) first; an existing brand
card is binding, and a stock or teammate template is never a brand source.

## Principles

1. Continuity beats novelty: keep what the existing masters already do well.
2. Proof before build: mocks pass before builders start.
3. The owner's sources are never edited; revisions are new files.
4. Every chapter reads as one deck.

## Font pairing

- Hosted fonts only (for Google Slides, check Font > More fonts); at most 2
  families plus 1 mono.
- Display face echoes the logo's letterform class; text face is a humanist
  sans that holds at 18-22 pt; mono has a tall x-height and clear 0/O, 1/l.
- Another script gets a companion family and its own paragraph direction.
  Use only glyphs the fonts cover, verified in the platform's PDF export.

## Numeric targets

| Target | Value | Check |
| --- | --- | --- |
| Body size (13.33 x 7.5 in) | 28 pt, floor 24; code floor 26 | render review |
| Body size (10 x 5.625 in) | 22 pt, floor 18; code floor 20 | render review |
| Sizes per slide | <= 5 | review |
| Margins | ~5% sides, ~7% top, ~9% bottom; 12 columns | mock overlay |
| Content fill | 75-85% of content height | contact sheet |
| Code panel | <= 8 lines, ~50 characters | render review |
| Checkpoints | one per 7-10 slides, a recap per section | outline |
| Same geometry in a row | <= 2 slides | contact sheet |

## Layout set

Build as theme layouts, each with purpose, grid position and maximum text:
title, section divider with progress tracker, claim plus visual (default),
big stat (source mandatory), native chart with a takeaway title,
myth/fact or before/after, process (3-7 nodes), code panel with reveal by
duplicated slides, checkpoint (answer on the next slide), recap (3
takeaways), reference. Ship it as a reusable template and audit the
platform's own render, not the local one.

## Source safety

- In the owner's files: hide, don't delete; note "Original: skipped slide N".
- In a revision copy, an original with a visible replacement may go, its
  text moved verbatim into the replacement's notes. Originals without a
  replacement stay.
- Private data (names, employers, past cohort answers) never reaches new
  slides. Rewording a claim about the presenter needs the owner's OK.
- Everything stays human-editable: real text boxes, native charts and
  shapes, no text baked into images.

## Proof mocks

Author title, body, code panel and big stat slides as HTML at the canvas
size with the real font files and logo, render headless at device scale 2,
and review each at full size and as a contact sheet until all pass a glance
test.

## Rubric

Score each deliverable against these anchors; quote the evidence (render, check output). Scores never gate delivery: verified findings do.

- **1 (fail):** Template look, size floors broken, text in images, sources edited.
- **3 (competent):** Layout set used, floors and contrast hold, but chapters drift.
- **5 (expert):** One system across all chapters, every fact sourced, mocks and Google render match.

## Verify

- Chapter coherence: lay ALL chapters side by side (title style, footer,
  palette roles, layout geometry, icons, charts, checkpoints, trackers,
  numbering); fix drift in the newer deck or log it as an exception.
- Export the converted deck to PDF: `pdffonts` shows no stray default
  fonts; check every slide at full size for overflow.
- Fold each review ruling into the spec as a numbered addendum.

## References

None yet; sources are platform documentation and WCAG 2.2.
