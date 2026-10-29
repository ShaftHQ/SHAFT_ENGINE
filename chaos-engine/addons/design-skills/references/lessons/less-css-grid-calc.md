---
name: less-css-grid-calc
description: Use when writing Custom CSS for a hosted builder that compiles with LESS 1.3.3 (e.g. Squarespace), especially grid span and calc().
---

# L33 LESS Custom CSS: longhand grid, escaped calc

## Use when

Custom CSS for a hosted site builder is compiled by **LESS 1.3.3** (Squarespace
Custom CSS and similar). Companion detail lives in
[squarespace-7-1](../site-builders/squarespace-7-1.md). Phone gutter parity
after a CSS paste is [L27](full-width-embed.md); re-verify live is
[L31](paste-then-reverify.md).

## Rules

1. **No slash grid shorthand.** Write `grid-column-start` / `grid-column-end`
   (and the row equivalents). LESS mangles `grid-column: 1 / -1` down to
   `-1`, so the item never spans.
2. **Escape `calc()` for LESS.** Prefer `~"calc(...)"` (or split the sum
   across two properties / elements). Bare `calc(a + b)` arithmetic is
   mangled by the same compiler.
3. **After every Custom CSS paste,** grep the **live** served CSS for slash
   grid shorthand, then re-check phone gutters live (`cmswidth` / L27) on
   the public URL — editor preview is untrusted.

## Verify

- [ ] `design_qc.py lessgrid <css>` fails on `grid-column: 1 / -1` and bare
      `calc(1px + 2px)`; passes longhand start/end and `~"calc(...)"`.
- [ ] Live CSS after paste has no slash grid shorthand.
- [ ] Phone gutter receipt re-run after the CSS change (L27 / L31).

## Why

LESS 1.3.3 treats `/` inside property values as division. The span you wrote
quietly becomes a single track end, and calc math collapses. Longhand and
escaped calc survive the minify; live CSS + gutter re-check catch drift the
editor hides.

## Sources

- Field lesson from hosted-builder Custom CSS: slash grid shorthand and bare
  calc mangled under LESS 1.3.3 until longhand + `~"calc(...)"` shipped.
- Extends [squarespace-7-1](../site-builders/squarespace-7-1.md) Custom CSS
  (LESS 1.3.3) into a fail-closed `lessgrid` gate.
