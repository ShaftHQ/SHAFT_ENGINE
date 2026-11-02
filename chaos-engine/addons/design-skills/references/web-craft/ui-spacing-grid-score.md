---
name: ui-spacing-grid-score
description: Use when laying out or scoring a web UI: spacing scale, grid columns, gutters, breakpoints, neutrals, and a scored diagnostic instead of a vibe.
---

# W07 Spacing, grid and score

## Use when

A page or component is laid out, or a UI needs a review score. Pairs with
[D05 typography and layout](../typography-layout.md) and [W06](web-typography-measured.md).

## Rules

1. **Spacing scale only:** 4, 8, 16, 24, 32, 48, 64 px. Icon to label 4,
   card sections 24, page sections 48-64. Forms 300-500 px wide; body
   text is never full width.
2. **Neutrals:** the darkest neutral is not `#000` (about `#111827`);
   greys are tinted toward the brand temperature.
3. **Grid:** 4 / 8 / 12 columns at mobile / tablet / desktop; gutters
   16 / 24 / 32; margins 16 on mobile, 24-48 on desktop; breakpoints
   375 / 768 / 1024 / 1440.
4. **Score, don't vibe:** run the 8-check diagnostic (hierarchy, spacing,
   type, colour, depth, alignment, consistency, states). Score =
   `round(pass / 8 × 10)`; report the failing checks by name.
5. **One token source:** each project keeps one DESIGN.md that the agent
   reads first (inspiration only).

## Verify

- [ ] `design_qc.py spacing src/` (margin, padding and gap px on the scale)
- [ ] The review lists the 8 checks with pass or fail and the score.
- [ ] Checked at 375, 768, 1024 and 1440 px.

## Sources

- [wondelai/skills refactoring-ui](https://github.com/wondelai/skills/tree/c172996495bed0fcd26896a9416b2093fd7073f0)
  @ `c172996495bed0fcd26896a9416b2093fd7073f0` (MIT, adapted).
- [owl-listener/designer-skills layout-grid](https://github.com/owl-listener/designer-skills/tree/9a6930cf84a822eb458624bd11c61aac5bbdf224)
  @ `9a6930cf84a822eb458624bd11c61aac5bbdf224` (MIT, adapted).
- DESIGN.md idea: google-labs-code/stitch-skills design-md (Apache-2.0, inspiration).
