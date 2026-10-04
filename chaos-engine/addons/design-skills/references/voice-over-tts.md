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
- Spoken forms for CLI syntax: "the with design skills flag", never a
  literal `--flag`, path, or variable. A script line keeps `text` (shown)
  and an optional `say` (spoken).
- Scan phonemes for an open syllable meeting the same vowel at a word join
  ("new user" is heard as "new new"); reword or listen and allow.
- Pace 140 to 160 words per minute; Kokoro speed 0.85 lands about 150.
  Measure pace from summed line durations, not the timeline. Insert pauses
  with punctuation, not silence edits.
- Render one file per storyboard scene so retakes stay local.
- Disclose synthetic narration in the end card and the description.
- If a human records instead, route the takes to D16 and D15.

## Workflow

1. Extract narration lines per scene from `storyboard.json`.
2. Apply the lexicon; render each scene to 24 kHz or 48 kHz WAV.
3. Transcribe each line WAV and compare with its script line. Pin ASR
   threads (4; 8 was nondeterministic) and run only on an idle machine:
   ASR under load invents repeats. Confirm any finding on the line WAV;
   one whole-file pass is never ground truth.
4. Normalize the stem per D15; update scene durations from real lengths.

## Verify

- `design_qc.py ttslint script.json --phonemes ipa.json` passes before
  synthesis.
- `design_qc.py vowords script.json transcripts.json` passes (no dropped
  content word, no unscripted repeat).
- `design_qc.py tts script.txt transcript.txt --duration <seconds>
  --fold codecs=codex` passes (folded WER 5% or less, raw WER reported,
  140 to 160 wpm); `design_qc.py idle` passes first.
- Voice name, model, and licence recorded in the manifest.
- `design_qc.py loudness vo.wav --target -18` passes.

## Sources

Kokoro-82M (Apache-2.0) and kokoro-onnx (MIT); Piper voice model cards;
whisper.cpp (MIT).
