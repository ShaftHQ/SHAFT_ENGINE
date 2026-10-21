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

## Palette from the logo

Token format and contrast checks: [brand system](../brand-system.md).
- Measure logo colours by pixel count or k-means, never by eye; map them to
  roles: primary, signal/negative, neutral dark, neutral light.
- Add only what roles need: ink text (~#1F2328), muted caption grey
  (>= 4.5:1 on white), a dark surface (a primary shade) for code and
  dividers, light tints, a checkpoint accent with a dark-surface variant.
- Compute WCAG contrast for every allowed text/background pair; publish an
  allowed and a banned list. Dimmed text is never lighter than muted.
- Simulate colour-vision deficiency on chart colours; colour is never the
  only signal (pair it with a word or icon).
- Map roles to theme slots; if hidden originals depend on old theme fonts,
  set fonts explicitly on every visible run and QC for legacy font runs.

## Font pairing

- Hosted fonts only (for Google Slides, check Font > More fonts); at most 2
  families plus 1 mono.
- Display face echoes the logo's letterform class; text face is a humanist
  sans that holds at 18-22 pt; mono has a tall x-height and clear 0/O, 1/l.
- Another script gets a companion family and its own paragraph direction.
  Use only glyphs the fonts cover, verified in the platform's PDF export.

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
- Kickers only inside a part with a divider tracker, at a fixed position;
  the claim's y never moves.

## Icons, images, humour

- One outline icon set (open licence) at one stroke width, one colour.
  Symbols are icons, not glyphs: check, x and warning marks become icons
  at cap height, coloured by role, next to their word.
- Arrow glyphs are missing from many hosted fonts: check the cmap
  (fontTools `getBestCmap()`), set a fallback face for that glyph only,
  confirm in a real upload; diagrams use native arrow shapes.
- Images only for photos, logos, memes and screenshots, each with alt text.
- Logo walls: uniform grid, one optical height, equal-area sizing
  (`w = sqrt(A*aspect)`, `h = sqrt(A/aspect)`), padding trimmed.
- Photos on dark slides: a cut-out bleeding off the edge or a full-bleed
  panel with a gradient melt; never a light rectangle.
- Screenshots show today's tool and carry a version or date caption.
- Legacy decorations stay on kept slides and go on rebuilt ones.
- At most one designed humour aside per slide, never at a learner's expense.

## Source safety

- In the owner's files: hide, don't delete; note "Original: skipped slide N".
- In a revision copy, an original with a visible replacement may go, its
  text moved verbatim into the replacement's notes. Originals without a
  replacement stay.
- Private data (names, employers, past cohort answers) never reaches new
  slides. Rewording a claim about the presenter needs the owner's OK, and
  the original wording stays in the notes.
- Every visible fact has a source in the notes (URL plus checked date), or
  moves to the notes as "Unverified, not shown".
- Everything stays human-editable: real text boxes, native charts and
  shapes, no text baked into images.

## Proof mocks

Author title, body, code panel and big stat slides as HTML at the canvas
size with the real font files and logo, render headless at device scale 2,
and review each at full size and as a contact sheet until all pass a glance
test.

## pptx to Google Slides traps

- Title placeholders import centred unless every placeholder sets `align`.
- `bullet:true` runs leave a default-font bullet glyph: use paragraph
  spacing or set the bullet font.
- Give each tracker label its own placeholder (spaced labels drift).
- Run the office validator with `--repair` before upload.

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
  fonts; check every slide at full size for overflow.
- Grid-view glance test plus one projected run-through.
- Every number sourced and confirmed by the owner.

## References

None yet; sources are platform documentation and WCAG 2.2.
