---
name: paste-robots-noindex
description: Use when authoring hosted-CMS Code-block paste; never emit robots noindex/nofollow or X-Robots-Tag — a pasted block can hide product pages from search.
---

# L34 Paste robots: never noindex in Code blocks

## Use when

Authoring paste HTML/CSS for a hosted builder whose Code block lands on a
public product or landing URL. Extends [L25](hosted-cms-owns-meta.md) and
pairs with live crawl checks in [W16](../web-craft/seo-basics.md).

## Rules

1. **No paste robots block.** Zero `<meta name="robots" …>` (or googlebot /
   bingbot) whose content includes `noindex`, `nofollow`, or `none`.
2. **No X-Robots-Tag in paste.** Do not emit `<meta http-equiv="X-Robots-Tag">`
   or header-style `X-Robots-Tag: …` text inside the paste file or nested
   code blocks.
3. **CMS SEO owns crawl.** Indexing and follow live in the builder SEO panel
   / response headers — never in the Code-block body.
4. **Re-verify live after publish.** Public product/landing URLs must not
   carry noindex (`design_qc.py liveseo`); editor preview is untrusted.

## Verify

- [ ] `design_qc.py cmsmeta <paste.html>` fails when any robots noindex /
      nofollow or X-Robots-Tag-style directive remains; passes on a clean paste.
- [ ] `design_qc.py liveseo <https-url>` (or `--fixture`) fails when the
      published page still has noindex meta or `X-Robots-Tag`.

## Why

A pasted code block that carried noindex hid product pages from search.
Paste robots directives publish with the page; the CMS SEO panel and live
headers are the only safe place to set crawl policy.

## Sources

Field lesson from a hosted-CMS embed: paste HTML included a robots noindex
directive and the public product URL stopped appearing in search until it
was stripped and re-verified live.
