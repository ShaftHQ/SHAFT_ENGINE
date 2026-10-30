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
3. **Unknown phonetic dumps → listen-verify.** `listenverify` fails a
   token that (a) matches the shipped phonetic seed list or `--seed`,
   including as a substring of the raw token, or (b) matches a
   conservative English-loan shape heuristic (e.g. كوبيلوت). Lexicon
   hits, `--allow`, and common Arabic function words are skipped. A
   human ear-pass restores Latin or allowlists the token. FN seeds:
   `سيكتت ايس بانك`, `كوبيلوت`.
4. **Mute ranges must be silent.** Declared mute windows on the mix must
   measure as silence (`mutesilent` RMS). A mute note with residual
   speech fails.
5. **No `[muted]` in cue text.** Mute only the audio (and redact
   amounts/names in text). A cue containing `[muted]` (case- and
   whitespace-insensitive) fails `mutedburn`. Cue-text / SRT-VTT only —
   no OCR of burned-in frames. Mute does not replace text gates
   ([L09](privacy-names-gate.md)).

## Verify

- [ ] `design_qc.py termlexicon cues.srt` fails leftover tech phonetics;
      `--allow موبايل` keeps an Egyptian loanword.
- [ ] `design_qc.py listenverify cues.srt` fails `سيكتت ايس بانك` and
      `كوبيلوت` (FN); fails seed-substring `بانكانا`; passes clean Arabic
      speech and `البنك`; `--allow` / common function words do not
      false-positive (FP).
- [ ] `design_qc.py mutesilent media.wav --mute 1.0,2.0` fails when the
      range still has speech; passes when that range is silent.
- [ ] `design_qc.py mutedburn cues.srt` fails `[muted]` / `[MuTeD]` /
      `[ muted ]` in cue text; passes redacted amount text without the
      placeholder.

## Why

ASR often paints English tech as Arabic letters; an allowlist keeps
spoken Egyptian loanwords. Mute windows that still carry speech (or a
visible `[muted]` burn) leak what the cut was meant to hide.

## Sources

Field lesson from a caption QA batch: phonetic tech dumps, a missed
`سيكتت ايس بانك` listen-verify, audible mute ranges, and burned
`[muted]` placeholders blocked sign-off.
