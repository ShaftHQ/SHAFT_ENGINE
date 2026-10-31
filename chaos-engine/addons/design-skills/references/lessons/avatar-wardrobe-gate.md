---
name: avatar-wardrobe-gate
description: Use when QC'ing an avatar or talking-head talking-head render against a wardrobe reference image.
---

# L48 Avatar wardrobe gate

## Use when

An avatar / I2V / talking-head render claims a locked wardrobe (garment, sleeve,
buttons, colour). Extends [L20](avatar-voice-first.md).

## Rules

1. **Check the REF IMAGE first.** Veo and similar copy the reference; if the
   ref is wrong, every frame is wrong. Gate fails closed when the ref is
   missing or disagrees with the signed wardrobe spec.
2. **Crop the collar / placket** for inspection; a wide face crop alone is not
   enough.
3. **Count each button** open vs closed against the spec (Locked avatar wardrobe = navy
   half-sleeve polo, buttons properly closed, friendly presenter — unless the signed
   script says otherwise).
4. **Sample every 0.5 s** across the speech window (and any held tail). A single
   hero frame is not enough.
5. Gate: `design_qc.py wardrobe` with `--ref`, `--spec`, and the render.

## Verify

- [ ] `design_qc.py wardrobe out.mp4 --ref ref.png --spec wardrobe.json` fails
      when ref is missing, button count drifts, or sample interval >0.5 s.
- [ ] Pass attaches collar crops at 0.5 s steps.

## Why

Wardrobe defects (open buttons, wrong sleeve, colour drift) are viewer-eye
blockers that metric-only QC misses. The reference image is the source of truth.

## Sources

Field retro 2026-10-31 (wardrobe-spec). Companions: L20, L39.
