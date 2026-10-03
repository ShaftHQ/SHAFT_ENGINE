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
  without pause.
- Typography: no viewport-only font sizes; tabular numbers in tables.
- Images: explicit width and height, `alt` text, lazy loading below the fold.
- Locale: dates, numbers, and plurals through `Intl`; no text in images.
- Performance: no layout shift on load; fonts with `font-display: swap`.
- Theming: every state readable in every theme.

## Workflow

1. Clone the pinned rules; note the SHA in the report header.
2. Run axe-core and Lighthouse on every theme.
3. Walk the page with the keyboard only, then at 390 px width with touch
   emulation.
4. Write findings in the output format; fix or file each.

## Verify

- axe-core: 0 violations in every theme.
- Lighthouse accessibility score of 95 or higher.
- Touch target sizes measured with `getBoundingClientRect()` in an e2e test.
- Report header records the rule-set SHA; a newer upstream SHA is noted as
  drift, not silently adopted.

## Sources

vercel-labs/web-interface-guidelines (MIT, referenced by pin); WCAG 2.2;
NN/g usability heuristics.
