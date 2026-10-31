---
name: karaoke-human-frag
description: >-
  Use when burning human-corrected ASS cues; fail short karaoke fragments
  inside human-marked cues.
---

# L63 No karaoke fragments in human cues

## Use when

ASS / cue QC after human caption correction, before SIGN. Extends L35
`askaraoke` and [L44](caption-min-cue-independent-sync.md) min duration.

## Rules

1. **Human cues stay phrase plates.** A cue marked human (Style/Name containing
   `human`, or JSON `human: true`) must not contain a karaoke `\k` / `\K` /
   `\kf` fragment shorter than **0.8 s**.
2. **Gate:** `craft_qc.py karaokehumanfrag <file.ass> [--min 0.8]`.
3. **Missing file** → FAIL (`missing-input`).
4. Raw ASR karaoke without human attestation remains an `askaraoke` FAIL.

## Verify

- [ ] Human Dialogue line with `\k40` (0.40 s) → FAIL.
- [ ] Human phrase cue without karaoke tags → PASS.
- [ ] Missing ASS path → FAIL.

## Why

A continuous karaoke fragment inside a human cue (dur &lt; 0.8 s) shipped past
SIGN; template fix followed. Gate makes the miss fail closed.

## Sources

Field retro 2026-10-31. Companions: L35, L44, L39.
