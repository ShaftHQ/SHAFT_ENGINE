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
2. **Line-height about 1.7** for Arabic body text, and the size one step
   above the Latin text beside it.
3. **Mirror the visual result, not the code:** use logical properties
   (`margin-inline`, `inset-inline`) so the layout follows `dir`.
4. **Directional icons follow reading direction:** back and next arrows,
   progress chevrons and sliders flip; logos, media play icons and
   clocks do not.
5. **Sentinel first:** before fixing rows, set one full-width Arabic
   sentence and confirm it reads right to left end to end.

## Verify

- [ ] `design_qc.py artrack <path>` passes (no letter-spacing on Arabic
      selectors, inline styles or text specs).
- [ ] The sentinel line renders right to left; arrows point the reading way.
- [ ] Rendered frames show joined letters, no tofu.

## Sources

- Cite only (low trust; rules re-checked against field use):
  [almubarmij/arabic-ai-skills arabic-rtl-best-practices](https://github.com/almubarmij/arabic-ai-skills/tree/08d3b9d14c2d1719c0b96f703bced8dccfe2076a/arabic-rtl-best-practices)
  @ `08d3b9d14c2d1719c0b96f703bced8dccfe2076a` (MIT).
