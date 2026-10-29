---
name: seo-basics
description: Use when shipping or auditing a public vibecoded, hosted-CMS, or static landing for search engines and AI answer engines.
---

# W16 SEO + AI-answer-engine basics

## Use when

A public site ships, merges pages, or needs crawl / answer-engine visibility.
Load after C01, [public-landing](../../../../references/public-landing.md),
and [hosted-CMS owns meta](../lessons/hosted-cms-owns-meta.md) when relevant.
Do **not** paste `og:` / `twitter:` / `<html lang>` into CMS Code blocks
(L25 / `cmsmeta`); set those in the CMS SEO panel and verify live.

## Crawl

1. **`/sitemap.xml`** returns 200 with `<urlset>` or `<sitemapindex>`; list it in `robots.txt`.
2. **`/robots.txt`** returns 200; `User-agent: *` must not `Disallow: /`.
3. **No `noindex`** in meta robots or `X-Robots-Tag` on public URLs.
4. **One absolute `https` canonical** per page.
5. **HTTPS** final URL (and 301 targets after merges — see
   [site-merge-redirects](../site-builders/site-merge-redirects.md)).
6. **True 404:** a missing path returns HTTP 404 (not soft-200), when the builder allows a custom 404.
7. **Clean slugs:** lowercase hyphen paths; no spaces, `_`, or `%20`.

## On-page

8. Non-empty **`<title>`** (aim ~30–60 chars); keyword-first home; "Page | Site".
9. Non-empty **meta description** with verified figures only (aim ~70–160).
10. **Exactly one `<h1>`**; heading levels do not skip (h1→h2→h3).
11. Meaningful **`alt`** on content images (empty only if decorative).
12. Sample same-origin **internal links** resolve (2xx/3xx).
13. **`<html lang>`** set in the CMS / site language setting (not via paste).

## Social / plan limits

14. One CMS **`og:title`**; **`og:image`** https 200 (or record **UNFIXABLE** on plans that lock http social images). Asset craft: [W14](og-image.md).
15. Schema / JSON-LD only when the plan allows injection; skip on Personal / no-injection plans ([Squarespace 7.1](../site-builders/squarespace-7-1.md)). Prefer FAQ / Organization entity blocks when allowed.
16. Core Web Vitals: follow C01 + D04 Lighthouse; do not restate thresholds here.

## AI answer engines

17. **`robots.txt` allows** AI crawlers (do not blanket-block `GPTBot`,
    `OAI-SearchBot`, `ChatGPT-User`, `Google-Extended`, `PerplexityBot`,
    `ClaudeBot`, `anthropic-ai`, `Applebot-Extended`).
18. Ship **`/llms.txt`** (plain-text site summary + key URLs for answer engines).
19. Critical answers readable **without JavaScript**; put a short quotable
    paragraph near the top of each key page.
20. **Consistent entity names** (product / org) across title, H1, schema, and llms.txt.

## Human account steps

- Google Search Console + Bing Webmaster: verify property, submit sitemap after redirects/merges.
- Optional `google-site-verification` / Bing meta when the CMS SEO panel supports them.

## Verify

- [ ] `design_qc.py liveseo <https-url>` (or `--fixture <dir>` offline); gate: [`liveseo_gates.py`](../../scripts/liveseo_gates.py)
- [ ] `design_qc.py cmsmeta <paste.html>` still clean (no social meta in paste)
- [ ] `design_qc.py livecms` / `ogimage` when embed or OG asset applies

## Sources

- Vibecoded-site SEO checklist (studied 2026-10-29); AI crawler / llms.txt practice.
