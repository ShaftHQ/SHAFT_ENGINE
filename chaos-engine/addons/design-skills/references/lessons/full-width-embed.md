---
name: full-width-embed
description: Use when a hosted-builder section holds a product Code block; the embed must span the content grid.
---

# L27 Full-width embed section

## Use when

Fluid Engine / builder sections wrap a product Code block. Sticky header
offset for the first section is owned by [L23](hosted-cms-no-dup-chrome.md);
this card covers width only.

## Rules

1. **Span the content grid.** The code-container content box must be full
   width of the page content column (minus gutter).
2. **Ratio gate.** Fail when `code_width / column_width < 0.92`
   (configurable) at **1440** and **390**.
3. **No nested squeeze.** Half-width or nested columns that squeeze code
   or video are a FAIL.

## Verify

- [ ] `design_qc.py cmswidth --code W --column C` fails when W/C < 0.92;
      passes at or above.
- [ ] Live measure at 1440 and 390 recorded in the livecms receipt ([L28](live-verify-cms-embed.md)).

## Why

A narrow Code-block column makes product code and Watch embeds look broken
even when the HTML is correct.

## Sources

Field lesson from a hosted-CMS embed redesign: narrow code columns on
product pages until the builder section spanned the content grid.
