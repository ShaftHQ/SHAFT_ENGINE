---
name: og-image
description: Use when a page, course or post needs its Open Graph or link-preview image, or a shared link shows a blank or cropped preview.
---

# W14 OG image

## Use when

Any page ships, or a link preview looks wrong.

## Rules

1. **Every page has its own OG image, 1200×630.**
2. **One headline of 6 words or fewer,** a brand mark, and high contrast
   (AA, 4.5:1 or more).
3. **Legible at feed size** (about 600 px wide): headline text at least
   2.6% of the short side at full size; aim far bigger.
4. **Dynamic OGs** per course or post from one template; animated OGs
   only where the platform supports them.
5. Arabic headlines follow [L07](../lessons/arabic-rtl-type-mirroring.md).

## Verify

- [ ] `design_qc.py ogimage og.png --spec og.json` (exists, 1200×630,
      word count, size, contrast, brand mark)
- [ ] Preview checked at 600 px wide.

## Sources

- ogimage.gallery categories (cite only, studied 2026-10-24).
