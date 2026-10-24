---
name: web-interface-audit
description: Use when a web interface needs a rule-by-rule audit for focus, forms, touch, motion, typography, images, and locale.
---

# D04 Web interface audit

## Use when

Reviewing a UI change or an existing page for interaction quality, before
merge or as a cleanup task. The output is a findings list, not a redesign.

## Inputs

The pinned rule set (vercel-labs/web-interface-guidelines at the SHA in
INVENTORY.md, cloned locally; never fetched live during an audit), WCAG 2.2,
and the running page.

## Output format

One line per finding: `path:line rule-id message`. Group by file. Each finding
is fixed in the same PR or filed as an issue; nothing stays only in chat.

## Audit areas

- Focus: visible `:focus-visible` with 3:1 contrast, logical order, no traps.
- Forms: labels bound to inputs, inline errors announced, correct
  `autocomplete` and input types, submit never disabled without a reason.
- Touch: targets at least 44 by 44 CSS px; spacing between adjacent targets.
- Motion: `prefers-reduced-motion` honored; no autoplay longer than 5 s
  without pause. A global override uses `0.01ms`, never `0s`: collapsibles
  (`<details>`, sidebar categories) finish on `transitionend`, which a zero
  duration never fires, so they stay open at 0 px with focusable content.
  Autoplay is re-evaluated on a `matchMedia` change.
- Names: an `aria-label` contains the visible text, starting with it (WCAG
  2.5.3 label in name).
- Typography: no viewport-only font sizes; tabular numbers in tables.
- Images: explicit width and height, `alt` text, lazy loading below the fold.
- Locale: dates, numbers, and plurals through `Intl`; no text in images.
- Performance: no layout shift on load; fonts with `font-display: swap`.
- Theming: every state readable in every theme.
- Overscroll: modals and drawers use `overscroll-behavior: contain`; to
  stop page bounce, set `none` on `<html>` only inside
  `@media (pointer: fine)` and test hybrid touch devices.
- Coverage: every touched page at 1440 and 390 px and under reduced motion,
  with mid-animation and end-state screenshots. Full-page headless captures
  exaggerate viewport-scaled spacing; confirm gaps at a real viewport height.

## Workflow

1. Clone the pinned rules; note the SHA in the report header.
2. Run axe-core and Lighthouse on every theme.
3. Walk the page with the keyboard only, then at 390 px width with touch
   emulation.
4. Write findings in the output format; fix or file each.

For generic tells and final polish, see [W01](web-craft/anti-generic-lint.md)
and [W04](web-craft/micro-polish.md).

## Verify

- axe-core: 0 violations in every theme.
- Lighthouse accessibility score of 95 or higher.
- Touch target sizes measured with `getBoundingClientRect()` in an e2e test.
- Under reduced motion, collapsing a `<details>` leaves `open === false`.
- Report header records the rule-set SHA; a newer upstream SHA is noted as
  drift, not silently adopted.
- Alt audit: list `img[alt=""]`; write alt for meaningful images (wordmark,
  video posters, facade thumbnails); keep decorative `alt="" aria-hidden`.
- Lighthouse: median of 3+ runs before calling a regression (mobile LCP is
  bimodal). A/B against a saved pre-change snapshot via request
  interception in one session; read LCP phases (TTFB, render delay, fonts).
- PSI API without a key hits quota: run the local Lighthouse CLI (pin the
  major, e.g. `lighthouse@12`) mobile and `--preset=desktop`, save the JSON.
- Semantic-only changes get a pixel diff: reduced motion,
  `document.fonts.ready`, full-page screenshot; the pixel diff (bbox diff) must be
  empty (e.g. PIL `getbbox()` is None).

## Sources

vercel-labs/web-interface-guidelines (MIT, referenced by pin); WCAG 2.2;
NN/g usability heuristics.
