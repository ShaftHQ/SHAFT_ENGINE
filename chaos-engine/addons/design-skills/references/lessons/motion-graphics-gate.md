---
name: motion-graphics-gate
description: >-
  Use when signing kinetic / motion-graphics video; OCR frames, static text,
  caption onset/lines, draw-on diagrams; share only approved deliverables.
---

# L65 Motion-graphics gate

## Rule

1. **OCR vs transcript:** every rendered-frame OCR row must match the verbatim
   transcript span. Mixed AR/EN: isolate Latin runs with **U+2066…U+2069**
   (FIRST-STRONG ISOLATE / PDI). Unisolated Latin inside RTL → FAIL.
2. **Kinetic static text:** for `kind=kinetic` deliverables, FAIL any static text
   block on screen **> 3 s** or **> 6 words**.
3. **Caption onset:** each cue onset vs speech ≤ **150 ms**.
4. **Transcript verified:** `transcript_manually_verified: true` required
   (checked against audio by hand).
5. **Diagrams draw-on:** diagram animation must be `draw-on` (not pop/fade).
6. **Caption lines:** each line ≤ **32 chars**; **1–2 lines** only; two-line cues
   must split at phrase boundaries (`phrase_split: true`).
7. **Share only approved deliverables:** share with the owner only after
   independent review **and** owner approval (Coach acceptance when applicable).
   Never share WIP samples. Minimal reporting = results, decisions, blockers —
   no progress chatter. Observations: `shared_with_owner` / `deliverable_shared`
   requires `reviewed` + `owner_approved` (+ `coach_accepted` when
   `coach_acceptance_applicable`); `wip_sample_shared: true` → FAIL.
8. **Reviewer briefs** must list these MG checks explicitly
   (`reviewer_brief_lists_mg_checks: true`).
9. Gate: `craft_qc.py mggate <observations.json>` ([helper](../../scripts/motion_graphics_gate.py)).
10. Missing observations / required keys → **FAIL** (fail-closed).

## Verify

- [ ] OCR frame text ≠ transcript → FAIL; unisolated Latin in RTL OCR → FAIL.
- [ ] Kinetic static block 4 s or 7 words → FAIL.
- [ ] Caption onset 200 ms → FAIL; onset 100 ms → PASS path.
- [ ] Caption line 33 chars → FAIL; 2 lines with `phrase_split: false` → FAIL.
- [ ] `transcript_manually_verified` absent/false → FAIL.
- [ ] Diagram `animation=pop` → FAIL; `draw-on` → PASS path.
- [ ] `wip_sample_shared: true` → FAIL.
- [ ] `shared_with_owner: true` without `reviewed` + `owner_approved` → FAIL.
- [ ] Coach-applicable share without `coach_accepted` → FAIL.
- [ ] Missing observations file → FAIL (`missing-input`).

## Why

An MG sample passed independent review while Coach found five misses (bidi
garble, static kinetic text, late captions, unverified transcript, pop-in
diagrams). Enforced observations + reviewer-brief checklist close that gap.
Caption ≤32 / 1–2 phrase lines keep kinetic type readable. Sharing only
reviewed, owner-approved deliverables (Coach when applicable) — with minimal
result/decision/blocker reporting — stops WIP noise and premature acceptance.

## Source

Carl field retro 2026-10-31 (MG r1). Companions: L02, L39, L41, L49, L64.
