---
name: band-coverage-gate
description: Use when a burned overlay band must hide source on-screen text for every content-segment frame.
---

# L14 Band coverage gate

## Use when

A talk Short burns a navy (or brand) band over source burned-in text, and
any leftover source glyph outside the band would publish. Extends
[L10](endcard-band-concat.md) (bands on content only) and
[C08](../craft/graphical-overlays.md).

## Rules

1. **Cover every content-segment frame.** Size the band from measured
   source-text bounds (multi-timestamp sample across the keep window).
   The plate must cover text top-to-bottom with margin; extend to the
   frame floor when text reaches the shelf.
2. **Gate leaks, not intent.** After render, sample several timestamps
   inside the content span (not the end card). Text-like ink / OCR
   outside the band row fails. One leaking frame fails the unit.
3. **Content end is not format duration.** Stop sampling before the
   end-card hold so a clean plate cannot mask a content leak.
4. **No personal denylist in CE.** Leak detection is geometric/ink (and
   optional project OCR). Keep person-name denylists in the project
   gate, not in ChaosEngine source.

## Verify

- [ ] `design_qc.py bandcoverage --frame leak.png --band-y Y --band-h H`
      fails when bright text-like ink sits below the band; a fully covered
      frame passes.
- [ ] Multi-frame / `--mp4` sampling reports `status=fail` if any sample
      leaks; `frames_checked` ≥ configured `n`.
- [ ] Contact sheet of content midpoints shows no source caption under
      the plate.

## Why

A default fixed plate (for example y=500 h=175) looks fine on one frame
and leaks on another when source text sits lower or wraps. Multi-sample
leak ink is the only check that matches what viewers see.

## Sources

Field lesson from a content QA recut batch: burned source text leaked
under a short navy band; multi-timestamp coverage gate required.
