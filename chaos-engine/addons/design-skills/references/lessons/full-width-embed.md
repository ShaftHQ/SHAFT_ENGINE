---
name: full-width-embed
description: Use when a hosted-builder section holds a product Code block; span the content grid and keep phone gutter parity with the hero.
---

# L27 Full-width embed section

## Use when

Fluid Engine / builder sections wrap a product Code block. Sticky header
offset for the first section is owned by [L23](hosted-cms-no-dup-chrome.md);
this card covers width and phone gutter parity.

## Rules

1. **Span the content grid.** The code-container content box must be full
   width of the page content column (minus gutter).
2. **Ratio gate.** Fail when `code_width / column_width < 0.92`
   (configurable) at **1440** and **390**.
3. **No nested squeeze.** Half-width or nested columns that squeeze code
   or video are a FAIL.
4. **Gutter parity (phone).** Embed content inset must sit within **±4px**
   of the hero container inset at **390** (and other phone widths). Fluid
   Engine grid inset plus nested `padding-inline` must not stack into the
   ~72–80px double-pad band; keep one phone gutter (~16–20px). Nested
   wrap+section both padding is a FAIL (`no-double-pad`).

## Verify

- [ ] `design_qc.py cmswidth --code W --column C` fails when W/C < 0.92;
      passes at or above.
- [ ] `cmswidth --hero-gutter H --embed-gutter E` fails when |E−H| > 4.
- [ ] `cmswidth --outer-pad A --inner-pad B` fails when both A>0 and B>0;
      `--content-inset` near 72–80 at viewport 390 fails as double-pad.
- [ ] Live measure at 1440 and 390 recorded in the livecms receipt ([L28](live-verify-cms-embed.md)).

## Why

A narrow Code-block column makes product embeds look broken. Stacked FE
inset + nested padding pushes phone content far past the hero gutter.

## Sources

Field lesson from a hosted-CMS embed redesign: narrow code columns and
phone double-pad until the section spanned the grid with one shared gutter.
