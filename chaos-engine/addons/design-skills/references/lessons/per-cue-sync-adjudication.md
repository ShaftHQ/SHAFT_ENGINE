---
name: per-cue-sync-adjudication
description: Use when FA and ASR disagree on a cue; adjudicate per-cue with local MMS_FA rules and write adjudication.json as the gate source of truth.
---

# L52 Per-cue sync adjudication (L39c)

## Use when

Caption sync review finds disputed cues (FA vs ASR disagree, false lead, or
silence-bridge timing). Extends [L39](video-signoff-viewer-frames.md),
[L44](caption-min-cue-independent-sync.md), and
[L49](caption-sync-multimodel-consensus.md).

## Rules

1. **Local MMS_FA window.** Re-run forced alignment on a ±1.5 s clip around the
   cue. If the local onset snaps near `clip_start` and `|local - clip_center| > 1.0 s`,
   **discard** that hit (`discard: true`). Do not keep a snap-to-clip-start false
   positive.
2. **Reject false leads.** A hit that is the previous plate / filler (`wa`,
   prior cue's last word) is a false lead: set `false_lead: true` and
   `rejected: true` (or `discard: true`).
3. **Bridge after silence.** Timing that resumes after a mute gap must use
   **post-silence RMS rise**, never early ASR sitting in the gap. Record
   `bridge_after_silence: { "used": true, "method": "post_silence_rms_rise" }`.
4. **`adjudication.json` is SoT.** Every disputed cue needs a non-empty
   `rationale`. Gate: `design_qc.py adjudic adjudication.json`.

## Verify

- [ ] Snap-to-clip-start with `|local-center| > 1s` and no discard fails
      `snap-to-clip-start`.
- [ ] Same row with `discard: true` passes that rule.
- [ ] `false_lead: true` without reject/discard fails `false-lead-not-rejected`.
- [ ] Bridge with `method: "early_asr_in_gap"` fails `bad-silence-bridge`.
- [ ] Disputed cue without `rationale` fails `missing-rationale`.

## Why

Local FA on a short clip often locks to the clip edge. Keeping those hits
green-washes real mistimes. Explicit discard + rationale makes the dispute
auditable.

## Sources

Carl field retro 2026-10-31 (L39c). Companions: L39, L44, L49.
