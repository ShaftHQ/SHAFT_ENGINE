---
name: deck-system
description: Use when building or reviewing a multi-chapter deck set, or running a two-agent review of decks or documents.
---

# Deck system

Companion to [C12 training decks](../craft/training-decks.md) and
[D20 delivery QC](../delivery-qc.md).

## Multi-chapter system

One system across every chapter of a course:

- Title kicker on one line: `CHAPTER 0N · Name`; zero-padded numbering
  (00-08) everywhere.
- `PART N` dividers; `CHECKPOINT N.x` slides each followed by an answer
  slide; a `RECAP · CHAPTER N` slide; a closing slide with questions and
  what comes next.
- One footer line `Presenter · Course · Year`; slide numbers on every slide
  except title, dividers, and closing.
- A question emoji only at the end of a title; sources on one line in the
  body font at 14 pt.
- At least one visual per part and at most two text-only slides in a row;
  keep a layout-per-slide-type map so each type always uses one layout.
  Title, course-map, agenda, recap and checkpoint slides follow the same
  grid, footer, type-floor and inset rules as body slides; they are not
  exempt. Quiz and cream-header checkpoints use a single-panel content
  layout so no ghost panel sits under the header band.

## Slide craft

- Connectors: one arrow style per deck (2.25 pt, medium triangle heads),
  at least 0.5 in long; shift or scale the row, never shorten an arrow.
- Text in shapes: one inset set per deck; bullet-less paragraphs indent 0
  so text aligns to the shape edge.
- Footer lines start with a label (`Reference:`, `Next:`, `Note:`); a Note
  fits on one line. Never cite your own slides or chapters as a source.
- Activities match the delivery medium: online decks use reactions, polls,
  chat, raised hands; no physical-room actions in slides or notes.
- After a branded document conversion, check every header: conversions
  can drop header images.

- Picking a style: show rendered previews of 2-3 candidate styles (one
  real slide each) and let the owner pick; never pick from adjectives.
  Adapted from [frontend-slides](https://github.com/zarazhangrui/frontend-slides)
  @9906a34 by Zara Zhang, MIT.

## Section trackers on dividers

- One tracker per part or chapter level; drop sub-topic trackers inside a
  part.
- Labels stay on one line at one size: shorten the label, never shrink the
  type. Same x, y and width on every divider, equal segments, and the
  active index equals the part number.
- The active marker (bar) matches the active label's own text box: same x
  and width, centre offset under 0.02 in. Assert this by script on every
  divider, not by eye.
- Choose the active and inactive colours with a contrast check on the real
  background first (see `color-contrast.md`).
- Decorative image bleed at the slide edge is fine if it stays clear of
  the tracker, title and footer bands. Divider art itself is one shared
  slide-level construction across decks, not layout-driven art; prove with
  art-region pixel diff = 0.00 % vs the reference divider.
- Evidence must match the claim: re-render after every fix and cite a
  capture of the exact slide and state; never reuse an old capture.

## Review order

When an orchestrator and a reviewer agent deliver together, follow the
multi-agent PDCA loop in the harness references (`multi-agent-pdca.md`).
Reviewer replies are either an issue list per output and slide/page number
(each with the measured and the expected value, see
[S05](deck-qc-measurable.md)), or `SIGN-OFF`. Review order, cheapest first:

1. Automated: `pdffonts` (off-brand fonts, including empty runs), a
   stale-figure grep, footer and slide-number counts, `design_qc.py
   code-parse` on extracted code, and section-number sequence.
2. Contact sheets of every page.
3. 1:1 crops for anything small in the contact sheet.
