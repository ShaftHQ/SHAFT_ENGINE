---
name: css-cascade-safety
description: Use when editing grouped CSS rules, border or mask declarations, or theme overrides on an existing page.
---
# W15 CSS cascade safety

Style-only edits break in the cascade, not in the rule you changed.

## Rules

1. **Grouped selectors:** removing `.x` from a shared rule (e.g. a dark
   `border-color` group) drops every declaration it got there. Diff the
   computed styles of the touched elements in every theme before and after.
2. **One edge differs:** give the edge colour a custom property
   (`--card-edge`) and let the theme rule set only the variable. Never add a
   second override rule.
3. **Shorthand clobbers longhand:** a later `border-color` or `border`
   resets an earlier `border-top` colour. Keep accent edges out of grouped
   colour resets, or re-declare the accent edge after
   the reset, or use the rule 2 variable.
4. **Masks:** pair `-webkit-mask` with `mask` and `-webkit-mask-image`
   with `mask-image` in the same rule (older Safari needs the prefix).

## Checks

- [ ] Computed-style diff per theme is empty except the intended change.
- [ ] `design_qc.py maskprefix <css-or-dir>` passes.
- [ ] `design_qc.py hovergate <css-or-dir>` passes.
