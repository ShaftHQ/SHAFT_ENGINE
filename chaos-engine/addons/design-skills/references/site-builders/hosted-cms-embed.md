---
name: hosted-cms-embed
description: Use when pasting a product or landing body into a hosted CMS Code block under existing site chrome.
---

# Hosted CMS embed playbook

Companion to [C01 web design](../craft/web-design.md) and
[Squarespace 7.1](squarespace-7-1.md). Applies when a self-contained product
or landing snippet is pasted into a hosted builder Code block (or equivalent
embed) that already owns the site header, nav, SEO, and sticky chrome.

Field lessons: [L23](../lessons/hosted-cms-no-dup-chrome.md) through
[L31](../lessons/paste-then-reverify.md). Sticky header offset lives on **L23**.

## Body only (no duplicate chrome)

1. Paste product body only (hero → product footer). Drop in-page topbar, nav,
   and skip that duplicate the host.
2. Keep a separate **preview** document with full chrome for local review.
3. Mark the paste root with an embed class (for example `*-embed`) so sticky
   and reset rules apply only to the paste kit.
4. Skip links: visually hidden until `:focus`; never consume layout when
   unfocused.

## Sticky site header offset

The first section of a pasted/embedded block must clear the host sticky
header. Set `--header-height` (or the host's documented equivalent) on the
embed root and apply `scroll-margin-top` / first-section padding from that
variable. Do not hard-code a pixel pad that ignores the live sticky height.
Owned by [L23](../lessons/hosted-cms-no-dup-chrome.md).

## Assets: resolve or inline

Code-block relative paths resolve against the **site origin**, not the
designer folder. A `../assets/…` mark becomes a 404 blank tile.

1. Scan the paste for relative `src`/`href` to local assets → fail the build.
2. Probe the live origin `Content-Security-Policy` `img-src` before choosing
   an inline strategy.
3. If `data:` is allowed, inline small brand marks; otherwise absolute
   `https://` URLs that return 200 only.
4. Gate: every identity mark `img.complete && naturalWidth > 0` at 390 and
   1440.

## Head: CMS owns meta

Do not emit `<meta property="og:*">`, `twitter:*`, or document `<html lang>`
inside Code-block paste. Configure title (`%p | %s` or builder equivalent),
description, and social image in the CMS SEO panel. Duplicate paste meta
produces double titles and fighting tags. If the plan cannot override an
http social image, record UNFIXABLE and stop — do not “fix” it in paste.

## Layout: full width + gutter parity

Fluid Engine / builder sections that hold the product Code block must span
the content grid. Fail when the code-container width ratio vs the page
content column is below **0.92** at 1440 and 390. On phone, embed inset
must stay within **±4px** of the hero gutter; FE grid inset + nested
`padding-inline` must not stack into the ~72–80px double-pad band.
Details: [L27](../lessons/full-width-embed.md).

## Hostile CSS and identity tile

Before SIGN, inject hostile global CSS (heading colour/font hijack,
`img{width:100%}`, visible skip spoiler). Headings on dark surfaces must
still compute near-white (relative luminance ≥ 0.85). Identity mark sits in
a light squircle / soft-superellipse tile with the dark mark inset; lock
tile and img boxes with `!important` against hostile sizing. Accent colour
only on pill/CTA chrome — never on dark-surface headings/body. See
[L26](../lessons/hostile-cms-resets.md).

## Paste then re-verify

Always paste from the **build artifact** full file. After Save, run the
headless live matrix on the **public** URL at **390 and 1440** ([L28](../lessons/live-verify-cms-embed.md),
[L31](../lessons/paste-then-reverify.md)). Nested code-container count must
stay 0. Editor success toasts are untrusted.

## Checks

| Check | Command / idea |
| --- | --- |
| No paste OG/Twitter meta | `design_qc.py cmsmeta <paste.html>` |
| No paste robots noindex/nofollow (L34) | `design_qc.py cmsmeta <paste.html>` |
| Live product URL not noindex | `design_qc.py liveseo <https-url>` |
| No relative local assets | `design_qc.py cmsassets <paste.html>` |
| Sticky header tokens present | `design_qc.py cmssticky <paste.html\|.css>` |
| Full-width + gutter | `design_qc.py cmswidth --code W --column C [--hero-gutter H --embed-gutter E] [--outer-pad/--inner-pad]` |
| Hostile heading + tile | `design_qc.py hostilecms …` |
| Duration band (sibling Watch) | `design_qc.py cmsduration --dur …` |
| Live matrix receipt | `design_qc.py livecms <report.json>` |

## Scripts

- Python gates (loaded by `design_qc.py`): [`hosted_cms_gates.py`](../../scripts/hosted_cms_gates.py)
- Hostile CSS fixture: [`hostile_cms_css.py`](../../scripts/hostile_cms_css.py)

