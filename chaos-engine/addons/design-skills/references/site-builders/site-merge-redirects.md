---
name: site-merge-redirects
description: Use when folding one site page into another, retargeting navigation anchors, or adding 301 redirects and SEO titles after a merge.
---

# Single-page merge, redirects and SEO

Builder-neutral; examples fit hosted builders such as
[Squarespace 7.1](squarespace-7-1.md). Layout craft:
[C01 web design](../craft/web-design.md).

## Merge

1. Inventory what is unique on the page being folded (stats, one note) and
   move only that; everything else is a duplicate.
2. Veto thin sections: if a moved section restates others, reads thin or
   leaves empty bands, drop it and point the nav at an existing section
   that answers the question.
3. Anchors: `id` on the section wrapper plus `scroll-margin-top` equal to
   the sticky header height (desktop and mobile values). After the jump the
   target top sits at or below the header bottom.
4. The mobile menu closes on an anchor tap: menu hidden, same-document
   scroll, no reload. Add a fix only if the test fails.
5. Self-links: grep every CTA and link for the old page URL and for anchors
   in the same section. Remove or retarget them (to the primary CTA); a
   section never links to itself. Re-check the hero CTA.
6. Disable, don't delete, the old page (unlink, then disable), then add the
   301. Write the rollback in the apply notes.

## Redirects and SEO

- A 301 redirect needs an absolute https target (Squarespace URL Mappings
  example: `/old -> https://www.example.com/ 301`); a relative or bare
  target can land on http. Verify with `curl -sI`: status and the `location` scheme.
- Page title format "Page | Site" (Squarespace example: `%p | %s`) with a
  keyword-first home title; descriptions use verified figures only. Re-scrape Open Graph and
  resubmit the sitemap after changes.

## Verify

- [ ] `design_qc.py liveseo <https-url>` after redirects/merges ([W16](../web-craft/seo-basics.md)).
- [ ] Resubmit sitemap in Search Console / Bing Webmaster.
