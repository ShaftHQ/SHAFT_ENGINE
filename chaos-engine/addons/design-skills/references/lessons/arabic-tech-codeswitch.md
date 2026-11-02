---
name: arabic-tech-codeswitch
description: Use when writing Arabic UI or product copy for developers that mixes English tech terms, especially card titles and headings.
---

# L32 Arabic tech copy: code-switch, not Fusha

## Use when

Arabic UI, landing or product copy for a developer audience mixes English
tech terms. Extends [L02](mixed-script-captions.md) (isolates) and
[L07](arabic-rtl-type-mirroring.md) (Arabic type) for **headings and card
titles**, not only burned captions.

## Rules

1. **Prefer the English tech phrase the audience already says** in card
   titles and section heads (`Fluent API`, `Evidence first`, `Quick start`).
   Pure formal Fusha for those labels reads stiff; Arabic only when no
   common English label exists.
2. **Bodies stay spoken Arabic** (short, dialect-friendly). Keep every
   Latin tech token inside `<bdi lang="en">…</bdi>` (or LRI…PDI in
   captions).
3. **No bare Arabic `و` next to a `<bdi>` Latin island in headings or card
   titles.** Use Arabic comma `،` between Latin items, or one island that
   already contains `and` (`Selenium and Appium`). Bare `و` between two
   Latin `<bdi>` runs flips under RTL.
4. **Hero Latin titles stay Latin** when the product already ships that
   line in English.

## Verify

- [ ] Card titles use the English tech phrase where the audience says it;
      bodies keep Latin terms in `<bdi>`.
- [ ] `design_qc.py bidiwao <path>` passes on headings and card titles
      (fails on `و` adjacent to `<bdi>` Latin; `،` or one island passes).
- [ ] A bad `<h3>…و <bdi>…` fixture fails; the same pattern only in a body
      `<p>` does not fail the default heading scope.

## Why

Developer readers already code-switch. Translating every label into formal
Fusha hides the terms they search and say. Separately, `و` between Latin
`<bdi>` islands in a heading reorders under `dir=rtl` and looks like a
glyph bug; `،` (or one island) keeps the list readable.

## Sources

- Field lesson from a developer-product Arabic UI rewrite: English card
  titles + `،` between Latin `<bdi>` islands in headings; ported from a
  site `bidi-wao` gate (conjunction `و` next to `<bdi>`).
- Extends [L02](mixed-script-captions.md) rule 8 («،» between Latin terms
  in captions) to HTML headings and card titles.
