---
name: endcard-band-concat
description: Use when burning question/title bands on a talk Short and appending a clean end card that must stay band-free.
---

# L10 End-card band concat

## Use when

A talk Short burns an overlay band (question, title) on the content span
and appends a branded end card. Extends [L03](talk-edit-and-layers.md)
rule 8 and [C08](../craft/graphical-overlays.md).

## Rules

1. **Burn bands on the content segment only.** Never bake the Q-band /
   title band into the end-card clip. Render content-with-band, then
   concat a **clean** end-card asset.
2. **Gate live end-card frames against the clean plate.** Grab a frame
   from the final mp4 near the end of the end-card hold (for example
   `duration − 1.5 s`) and compare band-row ink to `endcard.png`. Any
   leftover band text ink fails.
3. **Plate geometry still holds:** end-card top-left stays brand ground
   (not a white slab); the wordmark plate stays opaque white. Reuse the
   project's plate box; CE's gate takes `--band-y` / `--band-h` and
   optional `--plate` coords.
4. **Why not a whole-frame SSIM?** A band is a thin bright strip; global
   SSIM can hide it. Measure ink fraction in the known band row.

## Verify

- [ ] `design_qc.py endcardlive final.mp4 --clean endcard.png` passes
      when the live frame's band-row ink matches the clean plate
      (`ink_delta` and `live_ink` under thresholds).
- [ ] A synthetic live frame with bright text in the band row fails
      against a clean navy plate.
- [ ] Contact sheet of live end-card frames shows no question/title band.

## Why

Burning the band for the whole timeline is the easy ffmpeg path and
leaves readable question text on the end card. Concat after a clean
plate keeps the brand frame; the live-frame ink gate catches the
regression before publish.

## Sources

- Field lesson from a content QA recut batch (Coach SIGN r4 F5): reburn
  on content only, then concat clean end card; gate live vs `endcard.png`.
