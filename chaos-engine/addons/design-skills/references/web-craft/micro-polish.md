---
name: micro-polish
description: Use when a web UI, slide or still is functionally done but feels off, before final QC.
---

# W04 Micro-polish

## Use when

The last pass before delivery on a web UI, a slide or a still. It pairs
with [D04 web interface audit](../web-interface-audit.md) and
[W01 anti-generic lint](anti-generic-lint.md).

## Rules

1. Concentric radius: inner radius = outer radius - padding. Nested
   corners share one centre.
2. Optical alignment beats geometric alignment: nudge play triangles,
   icon-plus-label buttons and asymmetric glyphs until they look centred.
   Optical alignment mirrors in RTL: directional icons (arrows, play,
   chevrons) flip; non-directional icons do not.
3. Layered shadows for elevation: two or more soft, transparent shadows
   rather than one hard shadow or a depth-only border. Keep borders for
   structure and state.
4. Hit target >= 44px on touch (at least 40px in dense desktop UI).
   Extend small controls with a pseudo-element, and never let two hit
   areas overlap.
5. Hover and focus are designed states: every interactive element has a
   visible hover and a visible `:focus-visible` ring.
6. Surfaces: on the web apply all of the rules. On slides and stills
   apply concentric radius, optical alignment and layered shadows.

## Image hover-reveal

A hero image that reveals a second state under the pointer.

1. Decorative only: no text, link or CTA is hover-gated. The base image
   stands alone and carries the message.
2. The two states are pixel-registered: derive state B from state A (same
   seed and frame, or inpainting). Verify with a 50% overlay or a diff.
3. Native first: CSS `mask-image` or `clip-path` driven by pointer CSS vars
   (`--x`, `--y`) set on `pointermove`. Use WebGL only when a shader is
   needed and its licence is commercial and unbranded.
4. Performance: animate mask, transform and opacity only; rAF-throttled,
   lerped, paused offscreen; durations from W02 tokens.
5. Fallbacks: under `(hover: none)` play one slow auto reveal or show the
   static image, never tap-gated. Reduced motion shows the static image.
   The reveal layer is not focusable; the CTA keeps `:focus-visible`. The
   sweep direction mirrors in RTL.
6. Other media: video and Shorts use a scripted cursor-path HTML motion
   hero; decks and stills use a before/after split or wipe.

## Verify

- [ ] Hover-reveal: 50% overlay shows no drift; the page works with the
      reveal removed; captures under `(hover: none)`, reduced motion and
      `dir="rtl"`.

- [ ] For each nested rounded element, inner = outer - padding.
- [ ] Crops of icon buttons look centred at 2x zoom, in LTR and RTL, and
      only directional icons are flipped.
- [ ] Every control measures >= 44px on touch, with no overlapping hit
      areas.
- [ ] Tabbing through the page shows a focus ring on every control.

## Sources

- Adapted with attribution from
  [make-interfaces-feel-better](https://github.com/jakubkrehel/make-interfaces-feel-better)
  @35545ea by Jakub Krehel, MIT.
- Hover-reveal pattern studied from a no-code tool's cursor-mask effect
  (proprietary, reference only; no script or asset used).
- [WCAG 2.2 target size](https://www.w3.org/WAI/WCAG22/Understanding/target-size-minimum.html)
