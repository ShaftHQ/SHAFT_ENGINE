---
name: deck-qc-measurable
description: Use when writing or reviewing the acceptance criteria and QC tooling for a deck set, or when a deck fix passes the analyzer but still looks wrong.
---

# Deck QC, measurable (S05)

Companion to [C12 training decks](../craft/training-decks.md) and
[deck system](deck-system.md). Field rules from a nine-deck polish loop
(212 slides, three review rounds). Round-trip and review process:
[S06](deck-roundtrip-review.md).

## Criteria that worked

1. **One standard table, in points, with a tolerance** for grid, margins,
   claim size and y, note and reference y, footer, wordmark, divider
   geometry. Every row is an analyzer code or a named manual check; "looks
   aligned" is never acceptance. Each requirement gets an ID.
2. **Zero rows**: acceptance is zero rows for the listed codes on a fresh
   export of every deck, plus signed manual checks by the implementer and
   an independent reviewer. The implementer's own tool output is not
   evidence.
3. **Measure rendered text, not boxes.** Source geometry (python-pptx)
   states intent; rendered spans from the PDF (PyMuPDF bbox) are truth for
   overflow, wraps, padding and gaps. Use both.
4. **Spacing contracts**: padding inside containers on rendered text
   (example: L/R >= 12, T/B >= 7), content to note/reference gap, text to
   image gap >= 6, opposite sides within +-2. Chips and pills get their own
   smaller numbers; say so in the table. State the trio as rows: gap >= 6,
   padding >= 12, and the note and reference clearances.
5. **Cross-deck sameness**: the same component renders the same pixels in
   every deck. Diff a reference crop of one deck against every instance
   (art region diff = 0.00 %). Divider art is one shared slide-level
   construction, not layout-driven art that drifts across decks.
6. **Type floors**: minimum sizes, at most N sizes per slide, no mid-word
   breaks, no orphan word, code lines never soft-wrap (rendered line count
   equals paragraph count). Soft-wrap fixes use an explicit newline plus
   hanging indent; mid-token breaks: widen or regrid before rewording
   (teaching meaning unchanged). If it does not fit, restructure; never
   shrink below the floor.
7. **Connectors**: glued at both ends, one width, colour and head, no tiny
   final leg, no diagonals unless they are glued straight spokes. If an
   elbow cannot be made clean and the steps are already ordered, remove the
   arrow.
8. **Waivers** are written per slide in the brief and in the review file, so
   they are not rediscovered every round. Never invent a closer CTA, claim
   or line absent from the approved base just to clear a detector; leave
   the waiver written.

## Pixel-only bugs

Geometry and row analyzers cannot see these; pixel tests or a full-size
look find them.

- **Ghost layout art**: a leftover panel or second copy of an image that is
  absent from the slide's shape tree. Test a band that must be blank and
  diff the art region against a reference. Quiz and cream-header slides
  use a single-panel content layout; a multi-panel leftover leaves a ghost
  band visible only as pixels.
- **Text touching a picture**: descenders a point or two from a
  screenshot is not a text-in-container fault, so the padding check is
  silent. Gate the text-box bottom to picture top gap (>= 6 pt) and zoom
  every picture card at 200-300 dpi.
- **Layout switches and re-imports** can change other things. Pixel-diff
  against the previous round to prove only the intended slides moved.
- **Emoji and mixed-script runs, rotated text, RTL**: analyzers over-report
  (orphan titles, "overflowing" rotated labels, Arabic overlaps). Check each
  finding against geometry before calling it real; record each filter.
- **Hanging indents and manual breaks** survive a round-trip only when
  explicit; compare text frames, not only pixels.

## Padding regressions

A padding fix is the usual way to create the next defect: shifting content
inside a fixed container pushes it into its neighbours.

- Change one gap, then recompute every gap in the chain in one table
  (title, description, image, caption, pill, card edge, note, reference).
- Container height is the sum of padding, element heights and gaps. If it
  does not sum, change the container, not one element.
- Keep image aspect (display versus source, crops included). When space is
  short, shrink the image or caption before the type or the padding.
- Verify on rendered ink, descenders included, not font-box bottoms.

## Tool gaps

- Detectors that read layout names miss slides on a blank layout; identify
  components by shape signature or pixels.
- Rotation: swap width and height for 90/270 degrees before judging legs.
- "Unglued" must read the connection attributes, not geometry; colour and
  width rules compare to the spec constant, not a computed one. A false
  positive is a tool bug: fix it, rerun everything, note it.
- Every manual find becomes a detector or a backlog item, or it recurs.

## Checks

- `design_qc.py cxnglue deck.pptx [--min-leg 10] [--min-length PT]
  [--min-straight PT]`: glued at both ends; every elbow leg (from ext and
  adj*, rot/flips ignored) >= 10 pt; `--min-length` (e.g. 36) is an opt-in
  floor on the elbow final leg, straights opt-in. Waiver: accepted short
  finals and 23-34 pt straight spokes pass the defaults.
- Arrow colour, width and head are a whitelist of spec tokens (example: one
  neutral grey token); any other value fails.
- `design_qc.py deckgap measured.json [--pad l,r,t,b] [--sym PT]`: padding,
  gaps and gap chains over measured boxes; prints the whole gap table.
- `design_qc.py slidediff round1/ round2/ [--expect s12 ...]`: per-slide PNG
  diff (summed RGB > 30 on > 0.02 % of pixels); an unexpected change or an
  expected one that did not land fails.
