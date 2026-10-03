---
name: design-skills
description: Use when a task designs or edits a web surface, image, motion graphic, or video and needs one verified design card plus objective QC.
---

# Design skills (add-on)

Optional ChaosEngine add-on. Installed at `.chaos-engine/addons/design-skills/`
only when a user passed `--with-design-skills`. Without it, use core
[ui-delivery](../../references/ui-delivery.md) for UI work.

## Iron rules

1. Load **one** card for the current step, never the whole set. Return here
   for the next step.
2. Web work loads core [ui-delivery](../../references/ui-delivery.md) first.
   Cards add design judgment; they never replace red-then-green tests or the
   viewport x theme matrix.
3. Every deliverable ends with its card's **Verify** list run through
   [design_qc.py](scripts/design_qc.py). `skipped` is not `pass`:
   install the tool or report the gap.
4. Real evidence only: real captures, real command output, real product
   facts. Mockups are labelled. No generated text inside images.
5. Licences first: record every font, track, voice, model, and footage
   source in the asset manifest before using it.
6. A design document (brief, storyboard, visual direction) runs the core
   [design loop](../../references/design-loop.md) when the owner asks for review.

## Cards

| Step | Card | Use when |
| --- | --- | --- |
| Plan | [D01 brief and storyboard](references/design-brief-storyboard.md) | any video, ad, or visual campaign starts |
| Brand | [D02 brand system](references/brand-system.md) | tokens, logo, voice are needed |
| Web | [D03 visual direction](references/web-visual-direction.md) | a surface needs an aesthetic idea |
| Web | [D04 interface audit](references/web-interface-audit.md) | a UI needs a rule-by-rule audit |
| Type | [D05 typography and layout](references/typography-layout.md) | scale, measure, grid |
| Color | [D06 color and contrast](references/color-contrast.md) | palettes, contrast, overlays |
| Image | [D07 image graphics](references/image-graphics.md) | posters, OG cards, thumbnails, diagrams |
| Motion | [D08 motion principles](references/motion-principles.md) | easing, timing, holds, flashes |
| Motion | [D09 HTML motion graphics](references/html-motion-graphics.md) | lower thirds, stings, kinetic type |
| Motion | [D10 technical animation](references/technical-animation.md) | architecture or data-flow explainers |
| Capture | [D11 screen capture](references/screen-capture.md) | real terminal or browser footage |
| Edit | [D12 edit assembly](references/edit-assembly.md) | EDL-driven ffmpeg edit |
| Cut | [D13 cutting and pacing](references/cutting-pacing.md) | rhythm, dead air, J/L cuts |
| Color | [D14 color grading](references/color-grading.md) | BT.709 conform and grade |
| Audio | [D15 mix and loudness](references/audio-mix-loudness.md) | ducking, two-pass loudnorm |
| Audio | [D16 noise removal](references/noise-removal.md) | recorded speech is noisy |
| Voice | [D17 voice-over (TTS)](references/voice-over-tts.md) | narration without a recording |
| Access | [D18 captions](references/captions-subtitles.md) | SRT/WebVTT, burned-in captions |
| Clean | [D19 restoration and upscaling](references/video-restoration-upscaling.md) | denoise, stabilize, upscale |
| Ship | [D20 delivery QC](references/delivery-qc.md) | final encode, manifest, gate |

End-to-end order for a product or tutorial video:
[technical-video pipeline](references/pipelines/technical-video.md).
Project packs may add brand facts; this add-on stays project-neutral.
Upstream pins and licences: [INVENTORY.md](INVENTORY.md).
