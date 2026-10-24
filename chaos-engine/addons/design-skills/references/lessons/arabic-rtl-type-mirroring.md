---
name: arabic-rtl-type-mirroring
description: Use when setting Arabic text on any surface, or mirroring a layout, icons or arrows for right-to-left reading.
---

# L07 Arabic text and mirroring

## Use when

Arabic text is set on a page, slide, still or video, or a layout is
mirrored for RTL. Extends [L02](mixed-script-captions.md) and
[W06](../web-craft/web-typography-measured.md).

## Rules

1. **Letter-spacing stays normal on Arabic:** tracking breaks the letter
   joins. No uppercase-style tracking, no negative tracking on headings.
2. **Size** one step above the Latin text beside it.
3. **Mirror the visual result, not the code:** use logical properties
   (`margin-inline`, `inset-inline`) so the layout follows `dir`.
4. **Directional icons follow reading direction:** back and next arrows,
   progress chevrons and sliders flip; logos, media play icons and
   clocks do not.
5. **No italic on Arabic;** load an explicit Arabic font stack with the
   weights used (400 and 700 at least).
6. **No article on a Latin word (web and UI copy):** never put الـ, بالـ
   or للـ straight before a Latin word; rephrase. Burned captions keep
   the [L02](mixed-script-captions.md) form «الـ X» (tatweel, then a space, then the isolated Latin
   run); only an article glued to the Latin letters fails there. A floating ? or ، means a Latin token
   straddles the line: isolate it (captions and HTML, [L02](mixed-script-captions.md)).
7. **Line-height:** display 1.3 or more, body 1.5-1.7; on 1080 px social
   stills body 36-40 px or more, display weight 600-800.
8. **No `direction: ltr` ancestor** leaking into an Arabic block.
9. **Microcopy:** errors say what happened and how to fix it; one numeral
   and time format throughout (ص/م).
10. **Sentinel first:** before fixing rows, set one full-width Arabic
   sentence and confirm it reads right to left end to end.

## Verify

- [ ] `design_qc.py arabiccss <path>` (italic, line-height, ltr blocks)
      and `design_qc.py alprefix <copy>` pass.
- [ ] `design_qc.py artrack <path>` passes (no letter-spacing on Arabic
      selectors, inline styles or text specs).
- [ ] The sentinel line renders right to left; arrows point the reading way.
- [ ] Rendered frames show joined letters, no tofu.

## Sources

- [jamalmohafil/claude-skills arabic-design](https://github.com/jamalmohafil/claude-skills/tree/ca8c888abeffa8616b611032510c631d43a263ef)
  @ `ca8c888abeffa8616b611032510c631d43a263ef` (MIT, adapted).
- itady74/ux-writing-arabic @ `412f6318084d33b4946f31f06849a34bbb49051a`
  (no licence: cite only, no text copied).
- Cite only (low trust; rules re-checked against field use):
  [almubarmij/arabic-ai-skills arabic-rtl-best-practices](https://github.com/almubarmij/arabic-ai-skills/tree/08d3b9d14c2d1719c0b96f703bced8dccfe2076a/arabic-rtl-best-practices)
  @ `08d3b9d14c2d1719c0b96f703bced8dccfe2076a` (MIT).
