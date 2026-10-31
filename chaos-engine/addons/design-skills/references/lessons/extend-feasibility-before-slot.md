---
name: extend-feasibility-before-slot
description: Use before scheduling an extend/recut unit; prove dup-multiple and G6 block gates before assigning a slot.
---

# L51 Extend feasibility before slot

## Use when

Scheduling an extend, LQ, or recut that shares source with Shorts / Q clips.
Extends [L43](lq-vs-short-dup.md).

## Rules

1. **Feasibility before slotting.** Run the extend-feasibility check before
   assigning a calendar/unit slot.
2. **Dup multiple ≥3×** when the long cut overlaps a Short window (L43
   superset rule). Below 3× → do not schedule.
3. **G6 block column** (or the current sheet's block column) must be clear; a
   blocked unit fails closed.
4. Gate: `design_qc.py extendfeas` with the inventory sheet / JSON.

## Verify

- [ ] `extendfeas` fails when dup multiple <3 or G6 blocked.
- [ ] Pass prints the compared titles, span math, and G6 state.

## Why

Slots assigned before feasibility burned render time on units that L43 would
have rejected.

## Sources

Field retro 2026-10-31 (LQ17 extend feasibility). Companions: L43.
