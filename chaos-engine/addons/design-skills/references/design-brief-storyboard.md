---
name: design-brief-storyboard
description: Use when a video, ad, or visual campaign starts and needs a brief, a timestamped script, and a storyboard before any render.
---

# D01 Brief and storyboard

## Use when

Before the first frame of any video, ad, launch post, or campaign. A
planning-only request stops after this card: no render.

## Inputs

Audience, the one message, call to action, channel (web page, video
platform, social vertical), target duration, aspect ratios, and the evidence
the product claims will cite (doc URLs, command output, files).

## Rules

- One message per piece. A second message is a second video.
- Write for the screen and the ear: short sentences, spoken numbers, no
  acronyms the audience has not seen yet.
- Every product claim carries `evidence` (URL, command plus output, or file
  path). No evidence, no claim. Mockups say "mockup" on screen.
- Storyboard rows are data, not prose. Keep `storyboard.json` beside
  `brief.md` and commit both.
- Durations add up to the target within 5%. Hand-offs name the next card and
  the asset path it consumes.

## Storyboard schema

```json
{"targetDuration": 90, "aspect": ["16:9", "9:16"],
 "scenes": [{"id": "s01", "duration": 6, "visual": "terminal: one-line install",
   "vo": "One line installs the engine.", "onScreenText": "One-line install",
   "textHold": 3, "asset": "capture/install.mp4", "source": "capture",
   "motion": "D09 lower third", "audio": "music bed starts",
   "claims": [{"text": "one line", "evidence": "docs/install#one-liner"}]}]}
```

## Workflow

1. Write `brief.md`: audience, message, CTA, channel, duration, aspect,
   tone, banned claims, owner decisions still open.
2. Draft the narration script scene by scene; read it aloud with a timer.
3. Fill `storyboard.json`; list shots and assets in `manifest.json`.
4. Route each scene: captures to D11, graphics to D07/D09/D10, narration to
   D17, captions to D18, assembly to D12.

## Verify

- `design_qc.py brief storyboard.json` passes: every scene has id, duration,
  visual, vo, asset, and source; claims have evidence; total within 5%.
- `design_qc.py holds storyboard.json` passes for on-screen text.

## Sources

Own synthesis. Brief-contract ideas from HyperFrames (Apache-2.0, ideas only);
editing priorities paraphrased from Murch, *In the Blink of an Eye*.
