---
name: grade-and-loudness-map
description: Use when grading footage (including AI-generated clips), writing grade notes, or picking a loudness target and true-peak ceiling for a delivery platform.
---

# V09 Grade order and loudness map

## Use when

A film is graded or mixed for delivery. Extends [D14 colour grading](../color-grading.md)
and [D15 mix and loudness](../audio-mix-loudness.md).

## Rules

1. **Grade order:** conform and tag → normalise → primary balance → shot
   match (luma and contrast before hue) → look → artifact pass → delivery QC.
2. **AI-generated MP4 is display-referred:** never apply a log-to-709 LUT.
3. **Grade notes in timecode form:** `00:12-00:17 what · change · protect`.
4. **Speech over a bed:** aim for 20 dB separation for instructional VO
   (WCAG 1.4.7 guidance).
5. **True-peak ceiling:** -1 dBTP for web, -2 dBTP before lossy or
   aggressive encoding.
6. **Loudness map** (upstream verified 2026-07-10; re-check before a
   broadcast or platform delivery):

| Profile | Integrated | True peak |
| --- | --- | --- |
| EBU R128 (`r128`) | -23 LUFS ±1 | -1 dBTP |
| ATSC A/85 (`atsc`) | -24 LKFS ±2 | -2 dBTP |
| Netflix | -27 LKFS dialogue-gated | -2 dBTP |
| Apple Podcasts (`podcast`) | -16 LUFS ±1 | -1 dBTP |
| Spotify (`spotify`) | -14 LUFS | -1 dBTP |

## Verify

- [ ] `design_qc.py loudness mix.wav --profile r128` (or the delivery profile)
- [ ] Netflix: dialogue-gated measurement needs a dialogue meter; ebur128
      integrated is not a substitute.
- [ ] `design_qc.py colortags` and `design_qc.py levels` on the export.

## Sources

- [calesthio/generative-media-skills](https://github.com/calesthio/generative-media-skills/tree/8c85352d5d75d4dcbe58480bd138e37b9742bab1)
  @ `8c85352d5d75d4dcbe58480bd138e37b9742bab1` (MIT, adapted; its Python validators were read, not run).
