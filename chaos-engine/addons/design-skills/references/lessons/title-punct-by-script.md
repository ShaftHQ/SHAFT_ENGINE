---
name: title-punct-by-script
description: Use when checking title and description question marks match the script (Latin ASCII ? vs Arabic ؟).
---

# L11 Title punct by script

## Use when

Metadata title or description (or a burned title band) asks a question and
mixes Latin and Arabic scripts. Extends [L02](mixed-script-captions.md)
rule 1 ("match question marks to the script") for **metadata**, not only
burned captions.

## Rules

1. **Latin-script string → ASCII `?` (U+003F).** An Arabic `؟` on an
   all-Latin title fails (example: `How to test this؟` → use `?`).
2. **Any Arabic letter → Arabic `؟` (U+061F).** An ASCII `?` on an Arabic
   or RTL title fails and also reorders under `dir=rtl`.
3. **Strip URLs before judging.** `watch?v=` must not trip the Latin `?`
   rule. Gate on title/desc body after URL removal.
4. **Description lines that embed the title** must carry the **title's**
   expected mark on that substring, even when the line starts with an
   Arabic prefix (G16-style templates).
5. **Bands already follow** [L02](mixed-script-captions.md) /
   script-aware prepare; this gate covers METADATA title + description.

## Verify

- [ ] `design_qc.py titlepunct --title 'How to test this؟'` fails; the
      same with ASCII `?` passes.
- [ ] `design_qc.py titlepunct --title 'ازاي نختبر ده?'` fails; with `؟`
      passes.
- [ ] `design_qc.py titlepunct --metadata METADATA.md` passes when title
      and embedded title-in-desc share the script-correct mark; URLs with
      `?` do not fail.

## Why

Viewers and platforms show the metadata string, not the band PNG. A Latin
title with `؟` looks like a font bug; an Arabic title with `?` jumps to
the wrong side under RTL. Matching the mark to the script keeps title,
band and description consistent.

## Sources

- Field lesson from a content QA recut batch (Coach SIGN r4 F7): Latin
  ASCII `?` / Arabic `؟` on title + description; URL `?` ignored.
