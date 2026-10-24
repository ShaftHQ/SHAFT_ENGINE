---
name: thumbnails-generated-stills
description: Use when making a video thumbnail, poster, generated image with text or a logo, or a brand-kit template for social post types.
---

# I01 Thumbnails, generated stills and post types

## Use when

A thumbnail, poster or generated still is made, or a brand kit needs post
templates. Extends [D07 image graphics](../image-graphics.md) and
[D23 social post visuals](../social-post-visuals.md).

## Rules

1. **Thumbnail:** brainstorm at least 5 truthful concepts, pick one
   information gap; one focal subject filling 40-60% of the frame; 0-3
   people; headline 2-4 words added as a deterministic overlay, never
   baked by the model; never invent claims.
2. **Style references are analysed, never passed as identity.**
3. **Wrong text in a generated bitmap:** regenerate; never patch text
   over it.
4. **Posters:** write the design philosophy first, then render about 90%
   visual, 10% text.
5. **Logos in generated images:** keep exact shapes, colours and
   letterforms, and keep them away from faces.
6. **Post-type slots:** the brand kit has one template slot per post
   type (launch, update, feature, milestone, hiring, event, partnership,
   award, charts, teaser, contest, offer), not one generic card.

## Verify

- [ ] `design_qc.py thumb thumb.json` (2-4 words, subject 40-60%, 0-3
      people, overlay not baked)
- [ ] `design_qc.py image thumb.png` for size and format.
- [ ] Every claim on the image is true and sourced.

## Sources

- [higgsfield-ai/skills youtube-thumbnail](https://github.com/higgsfield-ai/skills/tree/f83af0bc1d937c8119099a11f8ebbf5e6fb99819)
  @ `f83af0bc1d937c8119099a11f8ebbf5e6fb99819` (MIT, rules only; its paid CLI is not used).
- [jimliu/baoyu-skills](https://github.com/jimliu/baoyu-skills/tree/1567581c26ec29f4216c6e6835415bf30343b0e3)
  @ `1567581c26ec29f4216c6e6835415bf30343b0e3` (MIT, inspiration).
- anthropics/skills canvas-design @ `dbd4588f9e1033efb41dad4bef2f7947c8993d44` (Apache-2.0, inspiration).
- Post types: posts.design public pages (cite only, studied 2026-10-24).
