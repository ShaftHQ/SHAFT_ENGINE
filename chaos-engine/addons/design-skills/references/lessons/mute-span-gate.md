---
name: mute-span-gate
description: Use when hard-muting privacy or hard-amount spans; reject long silence that should have been a cut or HOLD.
---

# L15 Mute span gate

## Use when

A publish window uses hard mute (volume zero) for privacy, hard-amount,
or filler, and reviewers must not ship long dead air. Extends
[L06](caption-video-qc.md) silence-gap rules and
[L09](privacy-names-gate.md).

## Rules

1. **Per-span cap.** Each mute interval ≤ **2.0 s**. Longer → recut
   (hard-trim / contiguous alternate) or HOLD; do not mute through a
   whole clause.
2. **Cue longer than 2 s → cut, do not mute the whole cue.** If the
   privacy or hard-amount cue itself is >2 s, contiguous-cut past it
   (or a word-timestamp tight mute of the token only, still ≤2 s) plus
   SRT redact. Full-cue mute of a 3–6 s span is illegal under this gate.
3. **Total mute fraction.** Sum of mute durations ≤ **10%** of content
   duration (excluding end card). Over → recut.
4. **Mute is not a pass for text gates.** Caption/ASR name scans still
   run ([L09](privacy-names-gate.md)); mute notes alone never PASS.
5. **Prefer cut or contiguous alternate** when mute would break
   coherence ([L19](hard-cut-coherence.md)).

## Verify

- [ ] `design_qc.py mutespan --mutes mutes.json --content-dur D` fails a
      single span >2 s and fails when total mute / D > 0.10.
- [ ] A unit with one 1.5 s mute on a 70 s content window passes.
- [ ] BRIEF lists contiguous-cut (not full-cue mute) when the cue itself
      was >2 s.
- [ ] BRIEF / gate JSON lists mute intervals with proof timestamps.

## Why

Long mutes feel like a broken cut. Short surgical mutes hide a word;
multi-second blackouts mean the edit should have dropped the beat.

## Sources

Field lesson from a content QA recut batch: long privacy mutes rejected;
≤2 s/span and ≤10% total became the hard gate.
