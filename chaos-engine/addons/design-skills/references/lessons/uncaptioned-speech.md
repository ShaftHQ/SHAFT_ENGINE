---
name: uncaptioned-speech
description: >-
  Use when signing captioned speech video; fail ASR speech spans that lack
  human-cue coverage.
---

# L64 Uncaptioned speech coverage

## Rule

1. Every ASR speech span with duration **≥ 300 ms** must be covered **≥ 50%**
   by human (non-hall) cues.
2. Gate: `craft_qc.py uncaptionedspeech --cues cues.json --asr asr.json` ([helper](../../scripts/uncaptioned_speech.py)).
3. Missing cues or ASR input → **FAIL** (fail-closed).
4. Hall / non-speech cue text (`[music]`, applause, filler) does not count as coverage.

## Verify

- [ ] Speech span 500 ms with no overlapping human cue → FAIL.
- [ ] Speech span 500 ms with ≥50% overlap by human cue → PASS.
- [ ] Missing `--cues` / `--asr` file → FAIL (`missing-input`).
- [ ] Hall-only cues do not cover speech spans.

## Why

Uncaptioned speech shipped past phrase-only karaoke checks when ASR heard
talking but human cues left a hole. Coverage against ASR is the gate.

## Source

Carl field retro 2026-10-31 (designer-workspace coverage helper, ported
project-neutral). Companions: L35, L44, L63, L39.
