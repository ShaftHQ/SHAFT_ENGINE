---
name: cms-paste-fragment-only
description: Use when authoring hosted-CMS Code-block paste; emit a fragment only — never a full HTML document — and keep dir/lang plus CSS on an embed wrapper.
---

# L36 CMS paste: fragment only (no document shell)

## Use when

Authoring paste HTML/CSS for a hosted builder Code block under existing site
chrome. Extends [L23](hosted-cms-no-dup-chrome.md), [L25](hosted-cms-owns-meta.md),
and [hosted-cms-embed](../site-builders/hosted-cms-embed.md).

## Rules

1. **Fragment only.** Paste must not contain `<!doctype`, `<html`, `<head`, or
   `<body` (any case). A full document's `<html dir=…>` can be adopted onto
   `documentElement` and flip the host sticky header.
2. **dir/lang on the wrapper.** Put `dir` and `lang` on the embed root (for
   example `<div class="*-embed" dir="rtl" lang="ar">`), never on `html`.
3. **CSS scoped to the wrapper.** Selectors target the embed class (or its
   descendants). Do not style bare `html`, `body`, or `:root` from paste.
4. **Live header parity.** After publish, site header logo and nav positions
   on the paste URL must stay within **≤1px** of the homepage `/`
   (`design_qc.py cmsheader` + `livecms` rows).

## Verify

- [ ] `design_qc.py cmsmeta <paste.html>` fails on document-shell tags or
      global `html`/`body`/`:root` CSS; passes on a wrapper fragment with
      `dir`/`lang` on the embed root.
- [ ] `design_qc.py cmsheader <receipt.json>` (or livecms
      `header_logo_delta_ok` / `header_nav_delta_ok`) fails when logo or nav
      delta vs `/` exceeds 1px.

## Why

A pasted full document carried `<html dir="rtl">`. The host adopted it onto
`documentElement`, which mirrored the Squarespace sticky header. Fragments
with direction on their own wrapper leave host chrome alone.

## Sources

Field lesson from a hosted-CMS embed: full-document paste flipped the live
site header; fragment + wrapper `dir`/`lang` restored parity with `/`.
