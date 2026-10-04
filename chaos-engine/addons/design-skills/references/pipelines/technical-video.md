---
name: technical-video-pipeline
description: Use when producing a product, install, or tutorial video end to end and the order of design cards and QC gates matters.
---

# Technical video pipeline

Order of cards for a product or tutorial video with a horizontal master and a
vertical cut. Each step names its gate; do not start a step while the
previous gate fails.

| Step | Card | Output | Gate |
| --- | --- | --- | --- |
| 1 | [D01](../design-brief-storyboard.md) | `brief.md`, `storyboard.json` | `brief`, `holds` |
| 2 | [D02](../brand-system.md) | `theme.json` | `contrast --tokens` |
| 3 | [D11](../screen-capture.md) | captures and logs | tape exit 0, log scan |
| 4 | [D07](../image-graphics.md), [D09](../html-motion-graphics.md), [D10](../technical-animation.md) | graphics, stings, explainer scenes | `easing`, `flash`, `image` |
| 5 | [D17](../voice-over-tts.md) or a human take plus [D16](../noise-removal.md) | narration stem | `ttslint`, `vowords`, `tts` or `noisefloor` |
| 6 | [D12](../edit-assembly.md), then [D13](../cutting-pacing.md) | `edl.json`, rough and fine cut | `vooverlap`, `static`, `blackfreeze`, `silence` |
| 7 | [D19](../video-restoration-upscaling.md) when needed | restored clips | `ssim`, `vmaf` |
| 8 | [D14](../color-grading.md) | graded master | `colortags`, `levels` |
| 9 | [D15](../audio-mix-loudness.md) | mixed master | `loudness` |
| 10 | [D18](../captions-subtitles.md) | SRT, WebVTT, styled vertical captions | `captions` |
| 11 | [D20](../delivery-qc.md) | encodes, thumbnail, manifest | `delivery`, `manifest`, `all` |

## Working folder

```
video/<slug>/
  brief.md  storyboard.json  theme.json  lexicon.json  edl.json
  capture/  gfx/  scenes/  vo/  music/  captions/  out/
  manifest.json  qc-plan.json  qc-results.jsonl
```

Commit sources (brief, storyboard, tapes, compositions, scene code, EDL,
lexicon, plan). Large renders live where the owner decides; record their
hashes in the manifest.

## Review

Run every step with the [runbook](runbook.md): detached builds with
`STATUS.md` checkpoints, fast gates per target, ASR and full QC on an idle
machine, an independent review, then delivery.
Run the [QC script](../../scripts/design_qc.py) `doctor` command first and list missing tools in the
PR. Append every gate's JSON line to `qc-results.jsonl`. A design document
for the video (brief plus storyboard) can go through the core
[design loop](../../../../references/design-loop.md) before production.
