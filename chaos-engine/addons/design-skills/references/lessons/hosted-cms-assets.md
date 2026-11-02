---
name: hosted-cms-assets
description: Use when building a hosted-CMS paste kit; rewrite relative assets to https or inline data URIs after a CSP probe.
---

# L24 Hosted-CMS assets: resolve or inline

## Use when

A Code-block paste references images, posters, or logos. Extends
[hosted-cms-embed](../site-builders/hosted-cms-embed.md) and
[Squarespace 7.1](../site-builders/squarespace-7-1.md) Media.

## Rules

1. **No relative local assets.** `../assets/…` (and other designer-relative
   paths) resolve against the site origin → 404 → blank marks. Fail the
   build if any remain in the paste.
2. **Probe CSP first.** Read live `Content-Security-Policy` `img-src` before
   choosing `data:` inlines. Do not assume `data:` is blocked or allowed.
3. **https or data.** Prefer absolute `https://` URLs that return 200; inline
   small brand marks as `data:` only when the probe allows it.
4. **Natural size gate.** Every identity-tile `img` must have
   `naturalWidth > 0` at 390 and 1440 after paste.

## Verify

- [ ] `design_qc.py cmsassets <paste.html>` fails on relative local `src`.
- [ ] Build rewriter leaves zero relative asset paths in paste output.
- [ ] Live marks load (`complete && naturalWidth > 0`) at 390/1440.

## Why

Relative paths look fine in local preview and go blank on the public origin.
A CSP guess without a probe either blocks inlines or ships fragile URLs.

## Sources

Field lesson from a hosted-CMS embed redesign: blank identity tiles until
the paste build inlined or absolutized brand marks after a CSP probe.
