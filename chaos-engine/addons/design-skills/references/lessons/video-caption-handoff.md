---
name: video-caption-handoff
description: Use when signing a talk or chapter video for publish; captions must be human-corrected, script-clean, continuous over speech, with measured true peak, matched fps, spelled VFX text, and an unclipped open.
---

# L35 Video caption handoff QC

## Use when

A talk, long-form chapter, or captioned short is ready to sign. Extends
[L06](caption-video-qc.md), [L15](mute-span-gate.md),
[L17](caption-micro-gap-snap.md) and [D15](../audio-mix-loudness.md) /
[D20](../delivery-qc.md).

## Rules

1. **Human-corrected captions, not raw ASR karaoke.** Burn-in must come
   from an ear-checked transcript. Pass `--attestation` affirming
   `human-corrected`. Word-level karaoke (mostly one- or two-token cues
   under ~0.45 s) fails `askaraoke` even with attestation; unattested
   phrase dumps also fail.
2. **Script filter.** Expected scripts only (default Arabic + Latin).
   Fail unexpected-script garbage (CJK, Cyrillic, Thai, Devanagari,
   emoji/symbol-only plates) and filler loops (same token ×3+). Gate:
   `scriptfilter`.
3. **No caption hole over speech.** While speech is present (including
   bursty talk), a caption gap longer than **3.0 s** fails. Measure
   speech with RMS union overlap, not cue math alone. Gate:
   `speechcuegap`.
4. **True peak ≤ −1 dBTP.** Measured `ebur128` true peak must be ≤
   **−1.0 dBTP** on the final mix (`loudness --max-peak -1`). A render
   receipt claiming loudnorm is not enough.
5. **Output fps equals source fps.** Keep source rate (59.94 stays
   59.94; do not silently drop to 30 or 60). Gate: `fpsmatch`.
6. **Spell-check on-screen VFX text.** Motion-graphic labels and
   overlays are lexicon-checked; near-miss typos (≤2 edits) and
   unknown/Arabic tokens fail by default. Gate: `vfxspell`.
7. **First spoken word not clipped.** Audio onset versus the first
   caption must not open on a truncated fragment inside a 0.15 s early
   window. Gate: `onsetclip`.

## Verify

- [ ] `design_qc.py askaraoke cues.srt --attestation note.txt` fails
      karaoke and unattested phrases; passes attested phrase cues.
- [ ] `design_qc.py scriptfilter cues.srt` fails CJK / emoji / 3× filler;
      passes clean Arabic/English.
- [ ] `design_qc.py speechcuegap media.mp4 --cues cues.srt` fails a >3 s
      hole over bursty speech; passes when the hole is silence or covered.
- [ ] `design_qc.py loudness final.mp4 --max-peak -1` passes (true peak
      ≤ −1 dBTP).
- [ ] `design_qc.py fpsmatch out.mp4 --source src.mp4` passes when rates
      match within 0.01.
- [ ] `design_qc.py vfxspell labels.txt --lexicon lex.txt` fails a
      two-edit typo / unknown; passes spelled labels.
- [ ] `design_qc.py onsetclip media.mp4 --cues cues.srt` fails a clipped
      open (incl. ~0.06 s); passes a padded full first phrase.

## Why

Raw ASR karaoke, foreign-script garbage, and caption blackouts over
speech make a chapter unreadable. Measured true peak, matched fps,
spelled VFX, and a clean open are the handoff bar—not self-reported
render flags.

## Sources

Field lesson from an independent talk-chapter review batch: karaoke
garbage, unexpected-script intrusion, true-peak clip above −1 dBTP,
multi-second caption holes over speech, VFX typos, and clipped opens
blocked sign-off.
