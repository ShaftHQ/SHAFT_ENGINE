---
name: uncaptioned-speech
description: >-
  Use when signing captioned speech video; fail ASR speech spans that lack
  human-cue coverage.
---

# L64 Uncaptioned speech coverage

## Rule

1. Every ASR speech span with duration **≥ 300 ms** must be covered **≥ 50%**
   by human cues that set JSON **`human: true`**. Non-hall text **without** the
   flag does **not** count as coverage.
2. Gate: `craft_qc.py uncaptionedspeech --cues cues.json --asr asr.json` ([helper](../../scripts/uncaptioned_speech.py)).
3. Missing cues or ASR input → **FAIL** (fail-closed).
4. Hall / non-speech cue text (`[music]`, applause, filler) does not count as coverage.
5. **Human predicate (vs L63):** L64 coverage uses JSON `human: true` only
   (`human_cue.is_human_json_cue`). ASS Style/Name `Human` is **L63-only** and
   does not satisfy L64 — see [karaoke-human-frag](karaoke-human-frag.md) and
   `human_cue.human_predicate_note()`.

## Verify

- [ ] Speech span 500 ms with no overlapping human cue → FAIL.
- [ ] Speech span 500 ms with ≥50% overlap by cue with **`human: true`** → PASS.
- [ ] Non-hall cue **without** `human: true` overlapping the span → FAIL (does not cover).
- [ ] Missing `--cues` / `--asr` file → FAIL (`missing-input`).
- [ ] Hall-only cues do not cover speech spans.

## Why

Uncaptioned speech shipped past phrase-only karaoke checks when ASR heard
talking but human cues left a hole. Coverage against ASR is the gate. Doc must
name the `human: true` JSON flag so H-DOC-CLAIM holds.

## Source

Carl field retro 2026-10-31 (designer-workspace coverage helper, ported
project-neutral). Companions: L35, L44, L63, L39.
