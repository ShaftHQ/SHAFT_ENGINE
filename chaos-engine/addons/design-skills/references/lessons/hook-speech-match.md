---
name: hook-speech-match
description: Use when a hook card, title card, or on-screen question sits over speech in the first seconds of a video.
---

# L01 Hook speech match

## Use when

A card in the opening seconds shows words while someone is talking. It
extends [C10 hooks](../craft/hooks.md) and [D13 cutting](../cutting-pacing.md).

## Rules

1. The card shows the words actually spoken in its window, verbatim.
   Paraphrase only after a spoken-match check of at least 60%.
2. The card ends when the spoken hook ends, using a per-item end time, not
   a fixed duration. Anything spoken under the card that is not on the
   card is a defect.
3. Matching is script-aware (Latin speech against a Latin card, native
   script against a transliteration, both ways) and monotonic (in order,
   each card word used once). It also fails when speech under the card
   matches no card word at all.
4. A gate that cannot see a script never fires. Ship every gate with the
   repro that must fail.
5. Never open on meta ("this got the most upvotes"). Meta lasts 1.5 s or
   less and comes after the question.
6. Take the hook start from word timestamps, never from coarse transcript
   line times.
7. Muted test: with sound off, frame 1 must stop a scroll (the card plus a
   moving face).
8. Q&A labels: the label prefix ("Question" / "Speaker") is visible from
   0 s, and the card holds the spoken question verbatim.
9. Hooks-first planning: scan sources for hooks before picking clips.
10. Hook → caption transition: a hard cut from the hook card to a
    different caption chrome reads as a jump. Keep a shared lower-third
    family under the hook; lengthen the hook crossfade (~0.55 s); land
    captions in the same stroke/scrim system. Prove with frames at
    ~0.8 / 1.5 / 1.7 s, not only hookcheck scores.
11. Hook preserves modality: a spoken **question** stays a question on
    the card (end with `؟` / `?`; keep the interrogative). A spoken
    **claim** stays a claim. Modality match is never N/A — including
    viral hooks. Gate: `design_qc.py hookmodality --hook --spoken`.
12. Q&A / disagreement Shorts must include the **answer beat** (or
    reasoning payoff), not only the setup. Extending past a neat
    mid-thought end is required when the answer lands later.
13. OUT SRT hook-bleed: under-hook speech lives in cues that end ≤
    `hook_end+0.3` (dropped from burn); the first surviving cue starts
    at/after `hook_end` with post-hook text only. Gate:
    `design_qc.py hookbleed cues.srt --hook-end <s>`.

## Verify

- [ ] The under-card gate reports 0 unmatched spoken words, and its repro
      fails as expected.
- [ ] The card's end time is within one frame of the end of the last
      hook word.
- [ ] On Q&A clips the label prefix is visible at 0 s.
- [ ] Frame 1 is read muted: the card is legible and a face is visible.
- [ ] Frames at ~0.8 / 1.5 / 1.7 s show shared lower-third chrome across
      the hook→caption crossfade (no chrome jump).
- [ ] `design_qc.py hookmodality --hook hook.txt --spoken spoken.txt`
      passes (question↔؟ / claim↔claim).
- [ ] Q&A / viral disagreement clips carry the answer beat or payoff.
- [ ] `design_qc.py hookbleed cues.srt --hook-end <s>` passes.

## Sources

- Field lessons from one talk-Shorts batch, verified on rendered frames.
- [YouTube Shorts best practices](https://support.google.com/youtube/answer/10059070)
