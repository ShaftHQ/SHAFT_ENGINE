---
name: caption-phonetic-mute-gate
description: Use when gating Arabic captions for leftover English-tech phonetic transliterations, Egyptian loanword allowlists, silent mute ranges, and burned [muted] placeholders.
---

# L38 Caption phonetic + mute gate

## Use when

Arabic talk captions or Shorts SRT are about to ship and must not keep
ASR phonetic dumps of English tech terms, leave mute windows audible, or
burn a literal `[muted]` placeholder. Extends [L02](mixed-script-captions.md)
(termlexicon), [L09](privacy-names-gate.md), and [L15](mute-span-gate.md).

## Rules

1. **Known Arabic-script tech/loan transliterations fail** until restored
   to Latin (`design_qc.py termlexicon`). Extend with `--term AR=LATIN`.
2. **Egyptian loanword allowlist.** Dialect loanwords that stay Arabic
   (shipped defaults + `--allow AR`) must **not** fail termlexicon or
   listen-verify. Prefer Latin only for tech terms the audience already
   reads in Latin.
3. **Unknown phonetic dumps → listen-verify.** An Arabic-script token
   that looks like a letter-by-letter English phonetic (not in the
   lexicon, not allowlisted, not a common Arabic function word) fails
   `listenverify` so a human ear-pass restores Latin or allowlists it.
   FN seed: `سيكتت ايس بانك`.
4. **Mute ranges must be silent.** Declared mute windows on the mix must
   measure as silence (`mutesilent` RMS). A mute note with residual
   speech fails.
5. **No burned `[muted]` captions.** Mute only the audio (and redact
   amounts/names in text). A cue containing `[muted]` / `[MUTED]` fails
   `mutedburn`. Mute does not replace text gates ([L09](privacy-names-gate.md)).

## Verify

- [ ] `design_qc.py termlexicon cues.srt` fails leftover tech phonetics;
      `--allow موبايل` keeps an Egyptian loanword.
- [ ] `design_qc.py listenverify cues.srt` fails `سيكتت ايس بانك` (FN);
      passes clean Arabic speech; `--allow` / common function words do
      not false-positive (FP).
- [ ] `design_qc.py mutesilent media.wav --mute 1.0,2.0` fails when the
      range still has speech; passes when that range is silent.
- [ ] `design_qc.py mutedburn cues.srt` fails a cue with `[muted]`;
      passes redacted amount text without the placeholder.

## Why

ASR often paints English tech as Arabic letters; an allowlist keeps
spoken Egyptian loanwords. Mute windows that still carry speech (or a
visible `[muted]` burn) leak what the cut was meant to hide.

## Sources

Field lesson from a caption QA batch: phonetic tech dumps, a missed
`سيكتت ايس بانك` listen-verify, audible mute ranges, and burned
`[muted]` placeholders blocked sign-off.
