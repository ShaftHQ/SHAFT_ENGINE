---
name: color-grading
description: Use when footage and graphics must be conformed to BT.709, matched to each other and the brand, and tagged correctly.
---

# D14 Color grading

## Use when

Captures, renders, and graphics come from different sources and must match,
or a master shows wrong levels, washed-out blacks, or shifted brand colors.

## Pipeline

1. Identify each source: screen captures are usually sRGB full range;
   renders may be full range; camera footage varies.
2. Correct first: convert to BT.709 limited range with explicit parameters,
   for example
   `zscale=matrixin=709:matrix=709:rangein=full:range=limited,format=yuv420p`.
3. Grade second: small, global adjustments (`eq`, `curves`, or a `.cube` LUT
   via `lut3d`) toward the brand palette.
4. Tag the output: `-color_primaries bt709 -color_trc bt709 -colorspace bt709
   -color_range tv` (for x264 also `-x264-params
   colorprim=bt709:transfer=bt709:colormatrix=bt709`).

## Rules

- Correction before grade; one LUT at most, committed with its licence.
- Keep luma inside 16 to 235 (limited range) on the master; clamp graphics
  that use pure white.
- Brand colors after encode stay close to their tokens; check a sampled frame
  per scene.
- Use scopes, not eyes: `waveform`, `vectorscope`, and `histogram` filters
  produce images for the PR.
- A mis-tagged full-range source is fixed at ingest, never compensated later.

## Workflow

1. Probe each input's color tags with ffprobe; list mismatches.
2. Apply the correction chain per source in the D12 normalize step.
3. Grade the master; export scope images for two or three key frames.

## Verify

- `design_qc.py colortags master.mp4` passes (BT.709 primaries, transfer,
  matrix, limited range).
- `design_qc.py levels master.mp4` passes (YMIN 16 or more, YMAX 235 or
  less).
- Scope images attached; brand swatch frames compared against the tokens.

## Sources

ITU-R BT.709; FFmpeg `zscale`, `colorspace`, `lut3d`, and scope filters;
ACES and DaVinci Resolve documentation (concepts only).
