---
name: motion-graphics-gate
description: >-
  Use when signing kinetic / motion-graphics video; OCR frames, static text,
  caption onset vs word-level ASR, draw-on diagrams; share only approved
  deliverables.
---

# L65 Motion-graphics gate

## Rule

1. **OCR vs transcript:** every rendered-frame OCR row must match the verbatim
   transcript span. Mixed AR/EN: isolate Latin runs with **U+2066…U+2069**
   (FIRST-STRONG ISOLATE / PDI). Unisolated Latin inside RTL → FAIL.
2. **Kinetic static text:** for `kind=kinetic` deliverables, FAIL any static text
   block on screen **> 3 s** or **> 6 words**.
3. **Caption onset (word-level audio):** measure each cue `start` against
   **word-level ASR / force-aligner** timings in observations (`asr_words` /
   `aligner_words` / `asr` / `asr_path`). Compute
   `|cue_start − matched_word_start|`; FAIL when **> 150 ms**. Builder-supplied
   `onset_ms_vs_speech` / `onset_ms` alone are **insufficient** and never pass
   the gate. Missing/empty ASR → FAIL (fail-closed). Same evidence class as
   L64 `--asr` / `capsync --onsets`.
4. **Transcript verified:** `transcript_manually_verified: true` required
   (checked against audio by hand).
5. **Diagrams draw-on:** diagram animation must be `draw-on` (not pop/fade).
6. **Caption lines:** each line ≤ **32 chars**; **1–2 lines** only; two-line cues
   must split at phrase boundaries (`phrase_split: true`).
7. **Share only approved deliverables:** share with the owner only after
   independent review **and** owner approval (named acceptance gate when
   applicable). Never share WIP samples. Minimal reporting = results, decisions,
   blockers — no progress chatter. Observations: `shared_with_owner` /
   `deliverable_shared` requires `reviewed` + `owner_approved` (+
   `coach_accepted` when `coach_acceptance_applicable`); `wip_sample_shared:
   true` → FAIL.
8. **Reviewer briefs** must list these MG checks explicitly
   (`reviewer_brief_lists_mg_checks: true`).
9. Gate: `craft_qc.py mggate <observations.json>` ([helper](../../scripts/motion_graphics_gate.py)).
10. Missing observations / required keys / ASR → **FAIL** (fail-closed).

## L65 follow-ups + L73 (CE 2026.10.31.7)

1. Require `asr_path` + `asr_sha256` + `asr_tool` in the gate report; reject inline
   builder `asr_words` without a path; reject `asr_source` in
   `{cue-aligned, cue_aligned, from-cues, builder}`.
2. Unmatched cue text → FAIL (no nearest-start fallback).
3. Duplicate tokens match sequentially (consume ASR hits in order).
4. Perfect 0 ms max across cues without `asr_independent_proof: true` → FAIL.

## Verify

- [ ] OCR frame text ≠ transcript → FAIL; unisolated Latin in RTL OCR → FAIL.
- [ ] Kinetic static block 4 s or 7 words → FAIL.
- [ ] Obs claims `onset_ms_vs_speech: 50` while ASR `|cue_start−word_start| > 150ms` → FAIL.
- [ ] Missing `asr_path` / `asr_sha256` / `asr_tool` → FAIL.
- [ ] Inline `asr_words` without path → FAIL.
- [ ] `asr_source=cue-aligned` → FAIL.
- [ ] Unmatched cue text (no nearest fallback) → FAIL.
- [ ] Duplicate token first-hit false-FAIL avoided via sequential consume.
- [ ] All-zero measured onset without `asr_independent_proof` → FAIL.
- [ ] Missing/empty ASR words from path → FAIL (`missing-asr-words` / `empty-asr-words`).
- [ ] Cue start aligned to ASR word within 150 ms → PASS path (builder onset field ignored).
- [ ] Caption line 33 chars → FAIL; 2 lines with `phrase_split: false` → FAIL.
- [ ] `transcript_manually_verified` absent/false → FAIL.
- [ ] Diagram `animation=pop` → FAIL; `draw-on` → PASS path.
- [ ] `wip_sample_shared: true` → FAIL.
- [ ] `shared_with_owner: true` without `reviewed` + `owner_approved` → FAIL.
- [ ] Acceptance-applicable share without `coach_accepted` → FAIL.
- [ ] Missing observations file → FAIL (`missing-input`).

## Why

An MG sample passed independent review while field review found five misses
(bidi garble, static kinetic text, late captions, unverified transcript, pop-in
diagrams). Enforced observations + reviewer-brief checklist close that gap.
Caption ≤32 / 1–2 phrase lines keep kinetic type readable. Sharing only
reviewed, owner-approved deliverables — with minimal result/decision/blocker
reporting — stops WIP noise and premature acceptance. Builder-attested onset
fields false-pass the same way cue-file onset gates did (MG r2); measuring
against independent word-level ASR closes that class.

## Source

Carl field retro 2026-10-31 (MG r1 + r2 onset). Companions: L02, L39, L41, L49, L64.
