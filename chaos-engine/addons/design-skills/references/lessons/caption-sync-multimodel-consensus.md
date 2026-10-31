---
name: caption-sync-multimodel-consensus
description: Use when running caption sync QC; require independent multi-model ASR consensus and fail on zero measured cues or drift over 150 ms.
---

# L49 Caption sync multi-model consensus

## Use when

Timing or signing off burned/soft captions. Extends [L44](caption-min-cue-independent-sync.md).
L44 remains the **single-model** path (`capsync --single-model`); L49 is the default
sign-off bar.

## Rules

1. **Independent ASR only.** Onsets come from ASR / forced alignment on the
   rendered audio. Never the builder's `words.json`, never snapper-injected
   anchor rows, never cue starts cloned into an onsets file.
2. **Multi-model consensus (required).** Pass `--onsets` and `--onsets-b` from
   ≥2 independent ASR (or ASR+FA) runs. Consensus is on by default; omit only
   via `--single-model` (L44). A single builder-planted row is rejected
   (`builder-injected-onset`).
3. **Drift >150 ms = FAIL** on ordinary cues (L44 code-switch tolerance may
   widen Latin tokens only).
4. **0 measured cues = FAIL**, never report drift 0.0 as success.
5. **Cue text must match** the spoken words in its window (token-overlap ≥0.5)
   and the cue window must overlap RMS speech (`--media` or `--rms-spans`).
   Gate: `design_qc.py capsync` with `--onsets`, `--onsets-b`, and
   `--media`/`--rms-spans`.

## Verify

- [ ] `capsync` without `--onsets-b` (and without `--single-model`) fails
      `missing-onsets-b`.
- [ ] `capsync` with empty onsets or zero matched cues fails `zero-measured-cues`
      / `empty-onsets`.
- [ ] Builder-tagged onset rows fail `builder-injected-onset`.
- [ ] Drift 151 ms fails; 149 ms passes at default tol 0.15 s.
- [ ] Multi-word cue with missing onset tokens fails `token-overlap`.
- [ ] L49 without `--media`/`--rms-spans` fails `missing-rms-media`.
- [ ] Onsets sharing the snapper words file still fail (L44).

## Why

Circular sync (builder onsets) and single-source ASR both green-washed real
mistimes. Consensus plus fail-closed empty measurement, token-overlap, and RMS
speech is the honest bar.

## Sources

Field retros 2026-10-30/31 (sync consensus). Companions: L44, L35, L38.
