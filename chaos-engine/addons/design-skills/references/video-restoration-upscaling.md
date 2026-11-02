---
name: video-restoration-upscaling
description: Use when footage needs denoising, deflicker, stabilization, or upscaling, and to decide when re-capturing is the better fix.
---

# D19 Video restoration and upscaling

## Use when

Footage is noisy, flickering, shaky, interlaced, banded, or below the
delivery resolution. Screen recordings are re-captured natively at the
delivery size instead of being upscaled.

## Decision tree

1. Can the source be re-captured at full resolution? Re-capture (D11).
2. Is it noise, flicker, banding, or shake? Use FFmpeg filters:
   `hqdn3d` or `nlmeans` (noise), `deflicker`, `deband`, `bwdif`
   (deinterlace), `vidstabdetect` plus `vidstabtransform` (shake).
3. Is it camera or archival footage below delivery size? Upscale with
   Real-ESRGAN (ncnn Vulkan build) on a GPU machine, with owner approval
   recorded in the manifest.
4. No GPU available: deliver at the native size or re-shoot.

## Rules

- Never upscale text, code, or UI; models invent glyphs. Rejected frames are
  re-captured.
- Restore before grading (D14) and before the edit render.
- Keep the original next to the restored file; restoration is reversible.
- Judge with metrics and crops, not only by eye.

## Workflow

1. Pick a reference: for upscaling, downscale a high-quality clip, restore it,
   and compare with the original (the downscale-then-restore protocol).
2. Tune filters on a 5-second sample; then process the clip.
3. Export side-by-side crops at 100% for the PR.

## Verify

- `design_qc.py ssim restored.mp4 reference.mp4` passes (0.95 or more).
- `design_qc.py vmaf restored.mp4 reference.mp4` passes (90 or more) when
  ffmpeg has libvmaf; otherwise it reports skipped and the PR says so.
- Temporal flicker is not worse than the source (frame luma standard
  deviation from `signalstats`).
- Crops show no hallucinated text or edges.

## Sources

Real-ESRGAN (BSD-3-Clause) and Real-ESRGAN-ncnn-vulkan (MIT); Netflix VMAF
(BSD-2-Clause-Patent); FFmpeg video filter documentation.
