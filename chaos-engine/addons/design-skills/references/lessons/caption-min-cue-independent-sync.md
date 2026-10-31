---
name: caption-min-cue-independent-sync
description: Use when timing or QC'ing burned/soft captions; enforce min cue length and sync against independent ASR or forced alignment.
---

# L44 Caption min-cue + independent sync

## Use when

Remapping, snapping, or signing off captions on a rendered talk / podcast /
Short. Extends [L17](caption-micro-gap-snap.md) and [L35](video-caption-handoff.md).

## Rules

1. **Min cue duration 0.8 s.** A cue shorter than 0.8 s fails. Merge it into a
   neighbouring cue (absorb text; keep the neighbour's start) so the short
   block is gone — do not leave a sub-min cue with a merge note.
   Gate: `mincuedur`.
2. **Sync uses independent onsets.** Measure cue starts against word onsets
   from ASR or forced alignment run on the **rendered** audio. Never reuse the
   snapper's own onset logic or clone cue starts into an onsets file — that is
   a tautology and always reports ~0 ms drift. Gate: `capsync` with
   `--onsets`. Echo detection fails closed (`onsets-echo-cue-starts`).
   Pass `--snapper-words` when a snapper word-source file exists: the gate
   fails if `--onsets` resolves to the same path (`onsets-share-snapper-words`).
3. **Code-switch tolerance.** Latin / English tokens inside a mixed-script
   line may use a wider drift budget (`--codeswitch-tol`, default 0.30 s);
   default cue drift stays 0.15 s.

## Verify

- [ ] `design_qc.py mincuedur cues.srt` fails a 0.4 s cue; passes after merge
      into a ≥0.8 s neighbour.
- [ ] `design_qc.py capsync --single-model cues.srt --onsets asr.json` fails when onsets equal
      cue starts exactly; passes with independent ASR/FA within tol.
- [ ] `capsync … --onsets a.json --snapper-words a.json` fails
      `onsets-share-snapper-words`.
- [ ] A code-switched English token at 250 ms drift passes under default
      `--codeswitch-tol`.

## Why

Sub-0.8 s flashes are unreadable. Sync that reuses the snapper's timing
cannot catch a mistimed burn — batch receipts that always show 0.0 ms drift
are that tautology. Independent ASR/FA on the rendered file is the only honest check.

## Sources

Field retro 2026-10-30: sync fix after viewer-eye FAIL; independent onset
requirement (role-neutral). Companions: L17, L35, L38, L39.
