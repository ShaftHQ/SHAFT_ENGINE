---
name: web-visual-direction
description: Use when a web page or component needs a deliberate aesthetic direction instead of a generic template look.
---

# D03 Web visual direction

## Use when

A surface (landing page, docs page, dashboard, component) needs a point of
view. Load core [ui-delivery](../../../references/ui-delivery.md) first; this
card adds judgment, ui-delivery keeps the test discipline.

## Inputs

Brand tokens (D02), the audience and the single job of the page, existing
design-system components, and screenshots of the current state.

## Design plan (write before editing)

- Audience and the one job the page must do.
- One aesthetic idea in a sentence (for example "engineering notebook:
  ruled grid, monospace annotations, one signal color").
- Four to six color roles mapped to tokens; one accent role only.
- Type roles: display, body, mono; hierarchy by size, weight, and contrast.
- One signature element tied to the product (a real terminal transcript, a
  live status badge, a diagram), not decoration.

## Unslop list

Avoid unless the brief justifies it in writing:

- gradient-filled text and rainbow gradients;
- glassmorphism and frosted blur panels;
- equal three-card grids with icon, title, two lines;
- ghost cards with faint borders and no content hierarchy;
- radius above 24 px on containers; pill everything;
- pure black or pure white page surfaces;
- stock illustrations of people at laptops; emoji as icons.

## Workflow

1. Capture the before matrix from ui-delivery.
2. Write the design plan in the PR body.
3. Apply tokens, hierarchy, and the signature element; reuse existing
   components before adding new ones.
4. Capture the after matrix and compare with the plan.

## Verify

- `design_qc.py unslop <src>` reports 0 hits, or each hit carries a
  `design-qc: allow` comment that cites the brief.
- ui-delivery matrix screenshots (every theme, five viewports) attached.
- `design_qc.py contrast` passes for the new color roles.

## Sources

Re-expressed with attribution from Anthropic `frontend-design` (Apache-2.0),
pbakaus/impeccable (Apache-2.0, NOTICE kept), and leonxlnx/taste-skill (MIT).
