---
name: privacy-names-gate
description: Use when gating captions or ASR for personal names and hard currency amounts before a public publish; vocatives and mute notes alone.
---

# L09 Privacy names gate

## Use when

A captioned talk clip, Short, or metadata pack is about to ship and must
not leak personal names or hard currency amounts. Extends
[L06](caption-video-qc.md) (ASR QC) and the content-scan preflight.

## Rules

1. **Scan caption text and ASR tokens**, not mute windows alone. A gate
   that only checks that a span was muted can PASS while the SRT or
   `words.json` still carries the name. Fail closed on any residual hit.
2. **Vocatives count.** Arabic `يا` / `ياا` plus a denylisted name is a
   privacy hit even when the bare name is allowed in other contexts.
   Example (synthetic): `يا فلان` → redact to `يا [اسم]`.
3. **Denylist is project-supplied.** Pass names with `--name` (and
   optional `--vocative-only` for tokens that only count after a
   vocative). Keep real trainee / client / owner names out of ChaosEngine
   source; use placeholders like `فلان` / `Alice` in tests and docs.
4. **Mute notes never PASS alone.** A review note that says "muted 12.0–
   14.0" without a text-scan receipt is a fail. The gate must record that
   caption and ASR were scanned.
5. **Hard currency amounts** fail the same gate when present in caption
   or ASR text. Project amount patterns stay in the gate's amount-regex
   flag or in project scripts; CE ships a small currency-agnostic default.

## Verify

- [ ] `design_qc.py privacynames --srt cues.srt --name فلان` fails on
      residual `يا فلان` and passes after redaction to `يا [اسم]`.
- [ ] `design_qc.py privacynames --words words.json --name فلان` fails
      on the vocative bigram `يا` + `فلان`.
- [ ] A notes file that only lists mute windows (no `text-scan`) fails
      even when the SRT is clean: mute alone ≠ PASS.
- [ ] Lookalikes that embed a name stem inside another word do not trip
      whole-token matching. Synthetic negatives: `احسن` / `يحسنوا` must
      not match denylist `حسن`. Vocative-only positive: `يا حسن` with
      `--vocative-only حسن` fails; bare `حسن` alone does not.

## Why

Mute windows hide audio; captions and ASR still publish the name. A
vocative makes a common word into a person. Fail-closed text scan is the
only check that matches what viewers read and what search indexes.

## Sources

- Field lesson from a content QA recut batch (Coach SIGN r4): scan SRT +
  ASR including vocatives; mute notes alone never PASS.
