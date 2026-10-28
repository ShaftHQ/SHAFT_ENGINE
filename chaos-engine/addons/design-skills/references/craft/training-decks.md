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
5. Refresh doctrine: a deck refresh is pure visual polish on a copy of the
   approved base. Never rewrite teaching content, invent lines to clear a
   QC flag, or restyle for novelty. Every change must improve a named Spec
   row, gap or pixel diff; drop unmeasurable changes.

## Numeric targets

Absolute pt tables, so builders never scale by hand:

| Role | 10 x 5.625 in | 13.33 x 7.5 in |
| --- | --- | --- |
| Stat | 140-160 | 190-210 |
| Section | 60 | 80 |
| Hero | 54 | 72 |
| Claim | 32 | 40-43 |
| Body | 22, floor 18 | 28, floor 24 |
| Code | 22, floor 20 | 28, floor 26 |
| Label | 14 | 18 floor (must-read) |
| Footer / meta | 12 | 16 |

| Target | Value | Check |
| --- | --- | --- |
| Sizes per slide | <= 5 | review |
| Margins | ~5% sides, ~7% top, ~9% bottom; 12 columns | mock overlay |
| Content fill | 75-85% of content height | contact sheet |
| Code panel | <= 8 lines, ~50 characters | render review |
| Checkpoints | one per 7-10 slides, a recap per section | outline |
| Same geometry in a row | <= 2 slides | contact sheet |

Footer grid: breadcrumb (`01 · Chapter`) bottom-left; slide number plus a
clean wordmark bottom-right. Recorded sessions keep a bottom-right camera
zone free of essential content.

## Layout set

Build as theme layouts, each with purpose, grid position and maximum text:
title, section divider with progress tracker, claim plus visual (default),
big stat (source mandatory), native chart with a takeaway title,
myth/fact or before/after, process (3-7 nodes), code panel with reveal by
duplicated slides, checkpoint (answer on the next slide), recap (3
takeaways), reference. Ship it as a reusable template and audit the
platform's own render, not the local one.
- One key word per title in the primary colour; one exception: a single
  parallel set (a column of valued items, or one node plus its title word).
- Repeated poll or data slides all use one chart style.
- Kickers only inside a part with a divider tracker; the claim's y is fixed.

## Hard-won rules

- Symbols are icons, not glyphs; arrow glyphs need a cmap check, a
  glyph-only fallback face and a real-upload check.
- Dark-slide photos: cut-out or gradient melt, never a light rectangle.
- Screenshots carry a version or date caption.
- Palette from the logo, Font pairing, proof mocks, icons, images, humour
  and pptx import traps: [deck assets and import](../slides/deck-assets-and-import.md).

Multi-chapter system and review order: [deck system](../slides/deck-system.md).
Arabic or bilingual PPTX (native RTL, no bidi controls): [S03](../slides/arabic-pptx-native-bidi.md).
Generated decks (pptxgenjs, template slots, notes): [S04](../slides/pptxgenjs-gotchas-notes.md).
Measurable acceptance, pixel-only bugs, padding chains: [S05](../slides/deck-qc-measurable.md).
Re-upload risks and two-agent review rounds: [S06](../slides/deck-roundtrip-review.md).

## Source safety

- In the owner's files: hide, don't delete; note "Original: skipped slide N".
- In a revision copy, an original with a visible replacement may go, its
  text moved verbatim into the replacement's notes. Originals without a
  replacement stay.
- Private data (names, employers, past cohort answers) never reaches new
  slides. Rewording a claim about the presenter needs the owner's OK, and
  the original wording stays in the notes. A missing closer, claim or line
  absent from the approved base stays as a written waiver; do not author
  new wording to make a detector go green.
- Every visible fact has a source in the notes (URL plus checked date), or
  moves to the notes as "Unverified, not shown".
- Everything stays human-editable: real text boxes, native charts and
  shapes, no text baked into images.

## Rubric

Score each deliverable against these anchors; quote the evidence (render, check output). Scores never gate delivery: verified findings do.

- **1 (fail):** Template look, size floors broken, text in images, sources edited.
- **3 (competent):** Layout set used, floors and contrast hold, but chapters drift.
- **5 (expert):** One system across all chapters, every fact sourced, mocks and Google render match.

## Verify

- Chapter coherence: lay ALL chapters side by side (title style, footer,
  palette roles, layout geometry, icons, charts, checkpoints, trackers,
  numbering); fix drift in the newer deck or log it as an exception.
- Rule each listed deviation approve, partial or fix, with concrete fixes;
  flag content changes separately from design calls. Fold each ruling into
  the spec as a numbered addendum.
- Brand self-audit before delivery: a PASS/FAIL table, one row per brand
  rule with measurement and evidence file.
- Mixed-script lines measured on the rendered output, per
  [typography and layout](../typography-layout.md).
- Export the converted deck to PDF: `pdffonts` shows no stray default
  fonts; check every slide at full size for overflow. On a polish, set
  typeface, size and colour explicitly on every visible run from the base
  render so theme inheritance cannot drift.
- Grid-view glance test plus one projected run-through.
- Every number sourced and confirmed by the owner.

## References

None yet; sources are platform documentation and WCAG 2.2.
