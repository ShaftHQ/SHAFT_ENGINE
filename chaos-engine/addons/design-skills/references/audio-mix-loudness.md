---
name: audio-mix-loudness
description: Use when narration, music, and effects must be mixed with ducking and normalized to a web loudness target.
---

# D15 Audio mix and loudness

## Use when

Any master with sound: narration over music, effects, or a voice-only piece.
Run after D16 (noise removal) and D17 (voice-over).

## Targets

| Stem or output | Integrated | True peak | Range |
| --- | --- | --- | --- |
| Web master | -16 LUFS +/- 1 | -1 dBTP or lower | LRA 11 LU or less |
| Narration stem | -18 LUFS +/- 1 | -1 dBTP or lower | |
| Music under speech | 15 LU or more below speech | | |

## Rules

- Edit dialogue first: remove clicks and breaths that distract, keep room
  tone under cuts so silence never drops to digital zero.
- EQ narration gently: high-pass around 80 Hz, cut mud near 250 to 400 Hz
  only if it is audible.
- Light compression on narration (2:1 to 3:1); no pumping.
- Duck music under speech with `sidechaincompress` (threshold low, ratio 6 to
  8, attack 20 ms, release 300 to 500 ms) instead of manual volume rides.
- Effects are rare and quiet; one per scene at most.
- Normalize with two-pass `loudnorm`: measure first, then apply the measured
  values with `linear=true`.
- Music and effects are CC0 or licensed; the licence goes in the manifest.
  Clipped sources are repaired before normalization.

## Workflow

1. Build the narration stem; normalize it to -18 LUFS.
2. Add music with sidechain ducking keyed from the narration.
3. Measure the mix with `loudnorm=print_format=json`; apply pass two.
4. Export the master audio at 48 kHz and mux in D20.

## Verify

- `design_qc.py loudness master.mp4` passes (-16 +/- 1 LUFS, true peak -1
  dBTP or lower, LRA 11 LU or less).
- `design_qc.py loudness vo.wav --target -18` passes for the narration stem.
- Music licence present in `design_qc.py manifest manifest.json`.

## Sources

EBU R128 with Tech 3341 and 3342; ITU-R BS.1770; AES TD1008 streaming
loudness recommendations.
