---
name: noise-removal
description: Use when recorded speech has hiss, hum, room noise, or clicks that must be reduced without damaging the voice.
---

# D16 Noise removal

## Use when

A human recording (a narration take, a screen-share mic) has audible noise.
Synthetic voice from D17 is already clean; skip this card for it.

## Tools

- FFmpeg `afftdn` (spectral denoise, learns a noise profile) and `highpass`,
  `equalizer` notches, `adeclick`.
- DeepFilterNet (MIT or Apache-2.0) for stronger speech enhancement.
- FFmpeg `arnndn` needs an RNNoise model file: use only a model with a
  clear licence. Unlicensed community models are not shipped or downloaded.

## Rules

- Profile first: record or find two seconds of room tone and measure it.
- Remove hum with notches at the mains frequency and harmonics (50 or 60 Hz,
  then 100/120, 150/180); if the peak sits elsewhere, find it with a
  spectrum and notch that.
- High-pass at 70 to 90 Hz for speech.
- Reduce gently: `afftdn=nr=10:nf=-40` as a starting point, raising in small
  steps. Musical noise (watery chirps) means too much.
- Blend dry and wet when a stronger model sounds processed.
- Over-processing fails: speech loudness must not drop by more than 1 LU.

## Workflow

1. Cut a room-tone sample and a speech sample.
2. Apply de-hum, high-pass, then denoise; listen on headphones.
3. Export spectrogram images before and after
   (`showspectrumpic=s=1280x480`).
4. Pass the cleaned stem to D15.

## Verify

- `design_qc.py noisefloor before.wav after.wav` passes (noise floor
  improves by 10 dB or more).
- `design_qc.py loudness` on a speech-only span changes by 1 LU or less
  between before and after.
- Spectrogram images attached; DNSMOS 3.5 or more when that tool is
  installed.

## Sources

FFmpeg audio filter documentation; DeepFilterNet (MIT or Apache-2.0);
RNNoise (BSD-3-Clause engine; model files vary).
