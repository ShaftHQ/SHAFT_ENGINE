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

## Review order

When an orchestrator and a reviewer agent deliver together, follow the
multi-agent PDCA loop in the harness references (`multi-agent-pdca.md`).
Reviewer replies are either an issue list per output and slide/page number,
or `SIGN-OFF`. Review order, cheapest first:

1. Automated: `pdffonts` (off-brand fonts, including empty runs), a
   stale-figure grep, footer and slide-number counts, `design_qc.py
   code-parse` on extracted code, and section-number sequence.
2. Contact sheets of every page.
3. 1:1 crops for anything small in the contact sheet.
