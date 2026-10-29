---
name: caption-micro-gap-snap
description: Use when remapping or fixing caption cues after a cut; snap sub-frame gaps so no frame is captionless.
---

# L17 Caption micro-gap snap

## Use when

Remapping SRT/cues onto a cut timeline, or after hard cuts leave tiny
gaps between adjacent cues. Extends [L06](caption-video-qc.md) rule 3
(`cuegaps` at splices).

## Rules

1. **Snap gaps under ~80 ms.** At 25 fps, two frames ≈ 80 ms. If
   `next.start − prev.end` is in `(0, ~0.080)` s, set `next.start =
   prev.end` so no frame is captionless.
2. **Close micro-overlaps at the midpoint.** If cues overlap by a tiny
   amount, split at the midpoint rather than leaving a double-burn.
3. **Re-run caption QC after every cue edit.** Micro-snap can change
   line wraps; run `cuegaps` / `boundprefix` again.
4. **Do not snap real pauses.** Gaps ≥ ~80 ms are editorial silence —
   keep them and gate with silence/deadair rules separately.

## Verify

- [ ] `design_qc.py cuegapsnap cues.srt` joins a 0.05 s gap; leaves a
      0.20 s gap untouched; `--check` fails while a micro-gap remains.
- [ ] After snap, `design_qc.py cuegaps cues.srt --join T` passes at each
      splice join.

## Why

Sub-frame gaps fail caption QC and flash empty frames at cuts. Snapping
them is cheaper than re-timing the whole track by hand.

## Sources

Field lesson from a content QA recut batch: remapped cues left <2-frame
gaps that `design_qc captions` / `cuegaps` rejected until snapped.
