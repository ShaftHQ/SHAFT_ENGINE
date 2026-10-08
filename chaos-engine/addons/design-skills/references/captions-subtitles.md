---
name: captions-subtitles
description: Use when a video needs accurate SRT or WebVTT captions, burned-in vertical captions, or chapter markers.
---

# D18 Captions and subtitles

## Use when

Every published video with speech. Captions are an accessibility
requirement, not an option.

## Line rules

| Rule | Value |
| --- | --- |
| Characters per line | 42 or fewer |
| Lines per cue | 2 or fewer |
| Reading speed | 20 characters per second or fewer |
| Cue duration | 0.833 s to 7 s |
| Gap between cues | 2 frames or more, or none |
| Sync | within 100 ms of the spoken word |

## Rules

- One source string per line: captions, on-screen text, and TTS input are
  generated from the same script line (D17 applies spoken forms only at
  synthesis), so they cannot drift.
- Generate from the script plus word timings from whisper.cpp alignment, so
  spelling follows the script and timing follows the audio.
- Break lines at linguistic boundaries: after punctuation, before
  conjunctions and prepositions; never split a name, a command, or a number
  from its unit.
- Technical terms are spelled exactly as in the documentation; commands keep
  their case.
- Description (WCAG 2.2 1.2.5): every scene's on-screen text is spoken in
  its narration or listed in a `description` that a described version
  speaks. Motion graphics carry meaning; captions alone do not cover it.
- Caption the narration, not the terminal text the viewer can already read.
- Burned-in vertical captions use brand tokens and the D06 scrim rule, stay
  inside the D20 safe zone, and never cover the terminal line being
  discussed.
- Web embeds add `<track kind="captions" srclang="en" label="English">`
  pointing at the WebVTT file.

## Workflow

1. Align the script to the final mix.
2. Build cues with the line rules; export SRT and WebVTT.
3. For the vertical cut, render styled captions (ASS or an HTML overlay).
4. Export chapters from the EDL for the description and the player.

## Verify

- `design_qc.py captions captions.srt --script script.txt --fps 30` passes
  (all line rules, 95% or more script coverage).
- The WebVTT file passes the same command.
- `design_qc.py describe storyboard.json` passes.
- A frame grab shows burned-in captions passing `design_qc.py contrast`.

## Sources

Netflix Timed Text Style Guide (English); BBC Subtitle Guidelines; WCAG 2.2
success criteria 1.2.2 and 1.2.5 (W3C Understanding documents).
