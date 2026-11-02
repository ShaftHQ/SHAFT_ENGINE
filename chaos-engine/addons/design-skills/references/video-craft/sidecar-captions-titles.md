---
name: sidecar-captions-titles
description: Use when shipping a sidecar caption file (SRT or WebVTT), or reviewing on-screen titles and kinetic type in a video.
---

# V10 Caption files and titles

## Use when

Captions ship as a file, or titles and kinetic type are reviewed. Burned
caption style lives in [D18](../captions-subtitles.md) and [V07](shorts-safe-area-captions.md).

## Rules

1. **Prefer a sidecar file;** burn in only when the platform needs it.
2. **Sidecar QC:** UTF-8, at most 2 lines, at most 42 characters a line,
   warn above 20 characters a second. WebVTT starts with `WEBVTT`, cue
   starts are ordered, every end is after its start.
3. **Flashes:** no more than 3 a second in any text or motion (WCAG 2.3.1).
4. **Title QC order:** accuracy → readability on the device → hierarchy →
   timing → motion meaning → safe areas → brand → accessibility → craft.
5. **Hero text is never auto-wrapped;** break lines by hand and review at
   100% on a phone.

## Verify

- [ ] `design_qc.py sidecar captions.vtt` (warnings for CPS)
- [ ] `design_qc.py flash film.mp4`
- [ ] Titles reviewed on a phone at 100%.

## Sources

- [calesthio/generative-media-skills](https://github.com/calesthio/generative-media-skills/tree/8c85352d5d75d4dcbe58480bd138e37b9742bab1)
  @ `8c85352d5d75d4dcbe58480bd138e37b9742bab1` (MIT, adapted; its Python validators were read, not run).
