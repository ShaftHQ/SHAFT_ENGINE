---
name: hosted-cms-no-dup-chrome
description: Use when embedding a product body into a hosted CMS under existing site chrome; no duplicate topbar; sticky header offset on first section.
---

# L23 Hosted-CMS embed: no duplicate chrome

## Use when

Pasting a self-contained product or landing snippet into a hosted builder
Code block (or equivalent) that already has site chrome. Playbook:
[hosted-cms-embed](../site-builders/hosted-cms-embed.md).

## Rules

1. **Body only.** Paste product body (hero → product footer). Drop in-page
   topbar, nav, and skip that duplicate the host.
2. **Preview vs paste.** Keep a separate preview document with full chrome;
   the paste kit strips chrome and uses an embed class.
3. **Sticky site header offset.** First section of the pasted/embedded
   block must clear the host sticky header. Set `--header-height` (or the
   host equivalent) on the embed root; apply `scroll-margin-top` and
   first-section padding from that variable. Hard-coded pads that ignore
   live sticky height are a FAIL.
4. **Skip until focus.** Skip links are visually hidden until `:focus`;
   unfocused computed height ≤ 1 px (or equivalent clip).

## Verify

- [ ] Live/paste DOM: zero in-page topbar/nav under the code container.
- [ ] `design_qc.py cmssticky` passes when `--header-height` and
      `scroll-margin-top` appear on the embed root; fails when absent.
- [ ] First content clears the sticky header after paste.
- [ ] Unfocused skip does not consume layout.

## Why

A second topbar under the host header is the most common post-paste defect.
Sticky overlap hides the first heading; only a variable tied to the host
sticky height stays correct across themes.

## Sources

Field lesson from a hosted-CMS product-page embed redesign: duplicate
in-page nav under the site header, sticky overlap, and skip spoiler under
hostile CSS.
