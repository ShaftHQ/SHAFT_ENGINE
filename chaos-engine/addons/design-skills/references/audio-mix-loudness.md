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
| Social / streaming master (platforms normalize to -14) | -14 LUFS +/- 1 | -1 dBTP or lower | LRA 11 LU or less |
| Narration stem | -16 to -18 LUFS +/- 1 | -1 dBTP or lower | |
| Music under speech | 15 LU or more below speech | | |

## Rules

- Edit dialogue first: remove clicks and breaths that distract. A human
  recording keeps its own room tone under cuts. Synthetic narration has no
  room tone: never add a drone, hum, or noise bed to fill gaps. Viewers hear
  a constant low bed as background noise. Fill dead air longer than 0.7 s
  with a short, quiet transition sound (high-passed, no echo, 20 dB or more
  under speech) or with real music.
- EQ narration gently: high-pass around 80 Hz, cut mud near 250 to 400 Hz
  only if it is audible.
- Light compression on narration (2:1 to 3:1); no pumping.
- Duck music under speech with `sidechaincompress` (threshold low, ratio 6 to
  8, attack 20 ms, release 300 to 500 ms) instead of manual volume rides.
- Synthetic narration masks easily: start the bed near -30 dB under the
  voice with a deeper duck (threshold about 0.012, ratio 12 to 14, release
  500 ms), then listen at phone volume.
- Endings: the audio fades over the final pad, starting where narration
  ends (never cutting it); the picture fades in over about 0.4 s and out
  over about 1 s. Never end on a hard cut to silence.
- Effects are rare and quiet; one per scene at most.
- Normalize with two-pass `loudnorm`: measure first, then apply the measured
  values with `linear=true`. When the linear gain would push the true peak
  over the ceiling, add a limiter before the measured gain stage. One-pass
  dynamic `loudnorm` rides the gain up in gaps and lifts any bed or noise.
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
- `design_qc.py gapfloor master.mp4 --timeline edl.json` passes (-60 dBFS
  or quieter between lines; deliberate transition sounds go in `--allow`).
- Mean volume over the last 0.5 s is at least 30 dB below the mean over
  the last 3 s (`volumedetect` on both windows).
- Music licence present in `design_qc.py manifest manifest.json`.

## Sources

EBU R128 with Tech 3341 and 3342; ITU-R BS.1770; AES TD1008 streaming
loudness recommendations.
