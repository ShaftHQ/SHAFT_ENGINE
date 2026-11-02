---
name: freeze-method-before-review
description: Use before a video or design review loop; freeze METHOD.md + measurement script, cap two fix rounds, then orchestrator adjudicates.
---

# L53 Freeze METHOD before review loop

## Use when

Starting an independent review / fix loop on a video, avatar, or captioned
render. Pairs with [design-loop](../../../../references/design-loop.md) and
[L39](video-signoff-viewer-frames.md).

## Rules

1. **Freeze first.** Commit `METHOD.md` (acceptance standard) and the
   measurement script **before** the first review round. Reviewer and builder
   share that set; findings fail only against the stated standard.
2. **Same method every round.** Do not change thresholds, crops, or ASR
   commands mid-loop without a new METHOD commit and a restart of the round
   count. The METHOD names a **fixed sample set** (cue indices / timestamps /
   frames) reused every round — no cherry-picking new samples after a FAIL.
3. **Independent sync anchors.** Caption/timing reviews use independent ASR/FA
   onsets ([L49](caption-sync-multimodel-consensus.md) / L44 `--single-model`),
   never builder-planted rows. METHOD must name the onset commands.
4. **AMBIG.** A finding is AMBIG only when the cue sits inside the speech
   bracket, on the right plate, **and** the review attaches rationale and a
   clip/frame. Otherwise it is FAIL or NIT.
5. **Cap 2 fix rounds.** After two failed fix rounds, stop the writer/reviewer
   ping-pong: the orchestrator adjudicates (accept AMBIG, re-spec METHOD, or
   escalate). Do not open a third silent fix round.

## Verify

- [ ] Review brief cites a committed METHOD.md path + script SHA/path.
- [ ] METHOD lists the fixed sample set and independent onset commands.
- [ ] Round ledger shows at most 2 fix rounds before orchestrator note.
- [ ] AMBIG rows include speech-bracket + plate + rationale/clip.

## Why

Moving the goalposts mid-loop burns tokens and ships green-washed timing.
A frozen method makes FAIL/AMBIG comparable across rounds.

## Sources

Field retro 2026-10-31. Companions: design-loop, L39, L49, L52.
