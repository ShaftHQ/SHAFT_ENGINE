---
name: voice-over-craft
description: Use when narration is written, directed, recorded, or synthesised.
---

# C06 Voice-over (craft)

## Use when

Narration is written, directed, recorded, or synthesised. Load after the matching D-card; this card raises the ceiling from correct to expert.

## Principles

1. Write for the ear: one idea per sentence, active voice, then cut half.
2. Direct intent, not words: who is speaking to whom, and why now.
3. Silence before the line that matters.
4. Picture leads, voice confirms: on-screen words land 0-2 f before they are heard.

## Do

- Read the script aloud with a stopwatch before recording.
- Hyphenate spelled letters (C-L-I) for TTS (`ttslint`).
- Keep a noise floor below -60 dBFS between lines.

## Don't

- Slogan copy (`copyslop`).
- Wall-to-wall narration with no breath.
- Reading what the screen already says verbatim.

## Numeric targets

| Target | Value | Source | Check |
| --- | --- | --- | --- |
| Pace | 140-160 wpm explainer; 2.5-3 words/s promo | BBC subtitle guidance; motionmaxxing SKILL.md step 5 | `tts --min-wpm --max-wpm` |
| Loudness | -16 LUFS online speech, -1 dBTP | Apple podcast spec; EBU R 128 | `loudness --target -16` |
| Gap noise floor | <= -60 dBFS | home-studio practice | `gapfloor` |
| WER vs script | <= 5% | ASR check (Whisper) | `tts --max-wer` |

## Anti-patterns

- Hype words (revolutionise, seamless, supercharge).
- Monotone TTS with no direction.
- Breaths and clicks left in.

## Rubric

Score each deliverable against these anchors; quote the evidence (frame, check output) for the score. Scores never gate delivery: verified findings do.

- **1 (fail):** Robotic, slogan-heavy, mismatched to picture.
- **3 (competent):** Clear, on-pace, clean; generic voice.
- **5 (expert):** A human, specific voice with deliberate pauses that make the picture land.

## Verify

- `design_qc.py copyslop|ttslint|tts|loudness|gapfloor`

## References

Twenty verified references with techniques: [research/voice-over-craft.md](../research/voice-over-craft.md).
