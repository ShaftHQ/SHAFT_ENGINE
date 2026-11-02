---
name: plan-first-multi-axis
description: Use when owner feedback spans colour, identity, layout, and new media together; write and accept a plan before building.
---

# L29 Plan-first for multi-axis visual change

## Use when

Feedback touches two or more of: colour system, identity treatment, layout
consistency, new media embeds. Extends
[design-brief-storyboard](../design-brief-storyboard.md) and core
[design-loop](../../../../references/design-loop.md).

## Rules

1. **Plan before build.** Write a signed plan: goals, non-goals, shared
   page system, token table, gate list, risks, definition of done.
2. **Acceptance first.** Design acceptance (👍 or SIGN on the plan) before
   building vN. Do not patch live across axes.
3. **Cite the plan.** Build PR/branch cites the plan path/SHA.

## Verify

- [ ] Plan artifact exists and is cited in the build PR body.
- [ ] Inventory / craft index still needles `plan-first` / `V2-PLAN`-style
      path language for redesign PRs (doc harness, not a hard CI block).

## Why

Multi-axis live patches thrash. A short shared system plan unblocks a
clean SIGN.

## Sources

Field lesson from a hosted-CMS embed redesign: a written multi-axis plan
before the next major version prevented scattered live fixes.
