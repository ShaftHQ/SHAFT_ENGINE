---
name: hosted-cms-owns-meta
description: Use when writing Code-block paste HTML; never emit Open Graph, Twitter card, or document html lang — the CMS SEO panel owns them.
---

# L25 Hosted-CMS head: CMS owns meta

## Use when

Authoring paste HTML for a hosted builder that already manages SEO and
social tags. Extends [hosted-cms-embed](../site-builders/hosted-cms-embed.md)
and [Squarespace 7.1](../site-builders/squarespace-7-1.md) Source vs rendered.

## Rules

1. **No paste social meta.** Zero `<meta property="og:*">` / `twitter:*` in
   the paste file.
2. **No document lang in paste.** Do not emit `<html lang>`; set language
   per block when needed.
3. **CMS SEO panel owns title.** Use the builder title pattern (for example
   `%p | %s`) so the page name appears once.
4. **UNFIXABLE http social images.** If the plan cannot override http
   `og:image` / `twitter:image`, record UNFIXABLE and stop — do not fight
   it from paste.

## Verify

- [ ] `design_qc.py cmsmeta <paste.html>` fails when any og/twitter meta
      remains; passes on a clean paste.
- [ ] Live page shows a single CMS `og:title` matching the SEO title.

## Why

Paste meta duplicates the CMS tags: double titles and fighting cards.
Personal / no-injection plans cannot fix http social images from a Code
block.

## Sources

Field lesson from a hosted-CMS embed redesign: paste OG/Twitter stripped;
http social image left as an accepted plan limit.
