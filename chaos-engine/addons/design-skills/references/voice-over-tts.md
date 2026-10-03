---
name: voice-over-tts
description: Use when a video needs narration without a human recording, produced locally with a commercially licensed synthetic voice.
---

# D17 Voice-over (TTS)

## Use when

The storyboard has narration and no human recording is planned, or a draft
voice is needed to time the edit before a human records.

## Engine and voices

Default: Kokoro-82M (Apache-2.0 weights) through kokoro-onnx (MIT), local
CPU. Piper's engine is MIT but each voice carries its dataset's licence, and
several popular voices are non-commercial: check every voice. Unknown or
non-commercial licences are refused.

## Rules

- Write for the ear: one idea per sentence, spoken numbers ("version three"),
  expand acronyms on first use.
- Keep a pronunciation lexicon (`lexicon.json`, term to phonetic spelling or
  IPA) for product names and commands. Fix pronunciation in the lexicon,
  never by misspelling captions.
- Pace 140 to 160 words per minute; insert pauses with punctuation, not
  silence edits.
- Render one file per storyboard scene so retakes stay local.
- Disclose synthetic narration in the end card and the description.
- If a human records instead, route the takes to D16 and D15.

## Workflow

1. Extract narration lines per scene from `storyboard.json`.
2. Apply the lexicon; render each scene to 24 kHz or 48 kHz WAV.
3. Transcribe each render with whisper.cpp and compare with the script.
4. Normalize the stem per D15; update scene durations from real lengths.

## Verify

- `design_qc.py tts script.txt transcript.txt --duration <seconds>` passes
  (WER 5% or less, 140 to 160 wpm).
- Voice name, model, and licence recorded in the manifest.
- `design_qc.py loudness vo.wav --target -18` passes.

## Sources

Kokoro-82M (Apache-2.0) and kokoro-onnx (MIT); Piper voice model cards;
whisper.cpp (MIT).
