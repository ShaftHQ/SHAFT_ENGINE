---
name: asr-names-run-gates
description: Use when a unit render suite runs QC; ASR name/price scan must be wired fail-closed into run_gates.
---

# L18 ASR names in run_gates

## Use when

A per-unit `run_gates` (or equivalent suite) finishes a talk Short and
must not skip privacy/amount scans. Extends
[L09](privacy-names-gate.md).

## Rules

1. **Wire the scan into the suite.** `privacynames` / ASR words scan is a
   required gate in `run_gates`, not an optional follow-up. Write
   `gate-asr-names-*.json` (or CE report) every run.
2. **Fail closed when words are missing.** If the suite expects ASR
   words and `words.json` is absent or empty, status is **fail**, not
   skip/pass. Missing input ≠ clean.
3. **Mute notes never replace the scan.** Even when mutes are listed,
   residual caption/ASR tokens still fail ([L09](privacy-names-gate.md)).
4. **Project denylist stays out of CE.** Pass `--name` / amount patterns
   from the project; CE ships the scanner only.

## Verify

- [ ] `design_qc.py privacynames --words w.json --name فلان --require-words`
      fails when `w.json` is missing; fails on residual vocative hits;
      passes after redaction.
- [ ] Unit `run_gates` receipt lists `asr-names` / `privacynames` with
      status pass|fail (never absent on a Ready unit).

## Why

An optional privacy gate is skipped under time pressure. Fail-closed
wiring makes a missing words file a red suite, not a silent ship.

## Sources

Field lesson from a content QA recut batch: ASR name/price gate had to
live inside `run_gates` and fail when words were missing.
