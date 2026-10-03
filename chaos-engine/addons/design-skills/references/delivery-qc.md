---
name: delivery-qc
description: Use when a finished video, thumbnail, and captions must be encoded to platform presets, packaged with a manifest, and gated.
---

# D20 Delivery QC

## Use when

The edit, grade, and mix are approved and files must ship: horizontal
masters, a vertical cut, a thumbnail, captions, chapters, and a manifest.

## Encode presets

| Output | Size | Video | Audio |
| --- | --- | --- | --- |
| 16:9 HD | 1920 x 1080 | H.264 High, yuv420p, CRF 18 or 8 to 12 Mbps at 30 fps | AAC-LC 48 kHz, 320 kbps |
| 16:9 UHD | 3840 x 2160 | H.264 High, yuv420p, 35 to 45 Mbps at 30 fps | AAC-LC 48 kHz, 320 kbps |
| 9:16 short | 1080 x 1920 | H.264 High, yuv420p | AAC-LC 48 kHz |
| Web embed | 1920 x 1080 | H.264, 25 MB or less, `+faststart` | AAC |

Common: constant frame rate, closed GOP of half the frame rate (`-g 15` at
30 fps), BT.709 tags (D14), `-movflags +faststart`.

## Vertical safe zone

Keep text and faces out of the top 14% and bottom 20% of a 1080 x 1920 frame,
and away from the right-hand action column of the platform UI.

## Manifest (`manifest.json`)

```json
{"title": "...", "assets": [{"path": "master-1080p.mp4", "sha256": "...",
  "licence": "owned"}, {"path": "music/bed.wav", "sha256": "...",
  "licence": "CC0", "source": "https://..."}],
 "voice": {"model": "kokoro-82m", "licence": "Apache-2.0", "synthetic": true}}
```

## Workflow

1. Encode each preset from the graded, mixed master.
2. Mux captions as sidecars (SRT, WebVTT) and chapters as metadata.
3. Export the thumbnail (D07) and the description with chapters.
4. Write `manifest.json` with hashes and licences.
5. Run the full gate with a plan file:
   `design_qc.py all qc-plan.json`, where the plan lists delivery, colortags,
   levels, loudness, captions, flash, blackfreeze, and manifest checks.

## Verify

- `design_qc.py delivery out.mp4 --preset 1080p --fps 30` passes (and
  `2160p`, `vertical` for those outputs).
- `design_qc.py manifest manifest.json` passes: every asset has a hash and a
  licence.
- `design_qc.py all qc-plan.json` ends `pass`; any `skipped` step is
  reported in the PR with the missing tool.
- Vertical frames inspected against the safe zone overlay.

## Sources

Platform recommended upload encoding settings and thumbnail guidance;
ITU-R BT.709.
