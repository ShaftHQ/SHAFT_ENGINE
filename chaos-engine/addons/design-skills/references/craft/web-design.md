---
name: web-design
description: Use when a page, landing, or docs surface needs senior-level layout, type, and interaction craft.
---

# C01 Web design (craft)

## Use when

A page, landing, or docs surface needs senior-level layout, type, and interaction craft. Load after the matching D-card; this card raises the ceiling from correct to expert.

## Principles

1. One claim per viewport; the product itself (real UI, real code, real output) is the hero.
2. Hierarchy by de-emphasis: grey the secondary before enlarging the primary.
3. Intrinsic layout: content sets the measure, the grid follows.
4. Motion explains state change; it never decorates idle content.

## Do

- Show code beside its result above the fold.
- Use one accent colour with one meaning (action).
- Design the empty, loading, and error states first.

## Don't

- Centre-everything hero with a gradient blob and three icon cards.
- Glassmorphism, gradient text, 40px+ radii on every surface (`unslop`).
- Scroll-jacking that fights the user.

## Numeric targets

| Target | Value | Source | Check |
| --- | --- | --- | --- |
| Line length | 45-75 ch body (66 ideal) | Butterick, Practical Typography | `measure` |
| Body line height | 1.4-1.6 | Butterick; WCAG 1.4.12 | review |
| Type scale | ratio >= 1.125 per step, <= 7 sizes | Utopia; Refactoring UI | `typescale` |
| Text contrast | 4.5:1 body, 3:1 large and UI | WCAG 2.2 1.4.3/1.4.11 | `contrast` |
| Target size | 24x24 CSS px min, 44 preferred | WCAG 2.2 2.5.8; Apple HIG | review |
| LCP / INP / CLS | < 2.5 s / < 200 ms / < 0.1 | web.dev Core Web Vitals | Lighthouse |
| README CTAs | both in the first screen at 1280x800 and 390x844 | GitHub README fold | render matrix |
| UI transition | 150-300 ms; enter decelerates | Material 3 motion | `motiondur`, `easing` |

## Anti-patterns

- Hero with no product in it.
- More than one primary button per viewport.
- Body text under 16px or wider than 80ch.

## Rubric

Score each deliverable against these anchors; quote the evidence (frame, check output) for the score. Scores never gate delivery: verified findings do.

- **1 (fail):** Generic template; no product visible; fails contrast or measure.
- **3 (competent):** Clear hierarchy, product shown, checks pass, but no idea a competitor could not reuse.
- **5 (expert):** An idea only this product could own, real proof above the fold, every check green, delight in one deliberate interaction.

## Verify

- `design_qc.py measure|typescale|contrast|unslop|easing <path>`
- `core ui-delivery viewport x theme matrix`

## GitHub README surfaces

- Render with the Markdown API `{"mode": "markdown"}`; `gfm` is comment mode
  and turns every newline into `<br>`.
- The API pulls a relative image out of `<a><picture>`; github.com keeps the
  nesting. Check hero links on the live page, not the API output.
- Headless Chrome media emulation does not pick `<picture>` sources: launch
  with `--force-prefers-reduced-motion`, and swap dark srcsets yourself.

## References

Twenty verified references with techniques: [research/web-design.md](../research/web-design.md).
