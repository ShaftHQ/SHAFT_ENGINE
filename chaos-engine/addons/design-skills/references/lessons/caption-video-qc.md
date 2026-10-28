---
name: caption-video-qc
description: Use when cutting a talk clip into a captioned short, checking caption timing at splices, sampling frames for review, or trusting ASR output.
---

# L06 Caption and video QC

## Use when

A captioned talk clip is cut, reframed and reviewed. It extends
[D18 captions](../captions-subtitles.md), [L02](mixed-script-captions.md)
and [L03](talk-edit-and-layers.md).

## Rules

1. Contact sheets sample cue midpoints: full-opacity mid-cue frames, never
   crossfade windows. Extra probe times snap into a cue, at least 0.25 s
   from its edges.
2. Self-review grabs a frame at every cue midpoint plus the hook, and
   reads the bidi order on each frame.
3. Splice joins have no caption gap: the previous cue extends to the join,
   so no frame at a cut is captionless.
4. Cutaway-in-crop: scan the span for the source's own scene cuts and flag
   any B-roll or graphic half-cropped by the face crop.
5. Text or graphic B-roll is reframed to fit, on frame-exact boundaries.
   Verify with frame-number tiles (`select=between(n,a,b)`); seek grabs
   are imprecise.
6. Sentence-boundary end + short audio fade: end the last cue on a
   finished clause from ear-checked word timestamps (last word `e`), not
   a fragment mid-token. Start `--fade_out` only after that `e`
   (150–250 ms, default 200 ms) so the last word is not dimmed and the
   sample does not click into the end card. Pad end-card silence
   **after** loudnorm so span LUFS stay on target. Check every fade
   frame.
7. ASR hallucinates on pauses and noise (outro phrases such as
   «شكرا للمشاهدة» or "thanks for watching"). Drop them from the words
   file, log them in the notes, and cross-check with 2 or more models plus
   a targeted probe of that span.
8. Hook = spoken words: hook text matches the speech under it, and the
   hook card ends when that speech ends.
9. Rendered-mix transcript diffs are noisy over music and end cards. Gate
   on cross-source agreement plus tagged misses; keep the rendered diff as
   evidence and waive only with a recorded reason. For SRT↔`words.json`,
   always report **exact-raw** and fuzzy side-by-side (`design_qc.py
   txdiff`): exact-raw = Latin `.lower()` only, no alias, no
   SequenceMatcher. Gate and headline = exact-raw; never label fuzzy
   “raw”.
10. A span starts on a shot cut and a clean sentence onset, never
    mid-phrase or on a cutaway.
11. Every cue edit re-runs the caption checks: removing a word can
    re-wrap a line and strand a bound prefix at its end. Split the cue on
    word timestamps instead.
12. Run heavy ASR models one at a time, detached. Parallel large models
    can be killed for memory without an error.
13. Silence-gap on the **final** render (post-denoise/loudnorm): measure
    RMS dBFS with a short hop (≤10 ms). Do not trust `silencedetect`
    alone for mid-answer dead air. Hard gate: FAIL if any mid-answer gap
    (after `hook_end`, before end-card) **>0.6 s**. Soft target: aim
    mid-answer gaps **≤0.45 s** (`--target 0.45` warns above it without
    failing). Always pass `--content-end` (last word / last cue, not
    end-card); default `duration−0.3` is **not** safe for long end pads.
14. Antecedent check on every splice: pronouns and epithets in the kept
    span need their referent noun/setup still present (or rewrite the
    card/caption so the referent is self-contained). Gate with
    `design_qc.py antecedent` (`--epithet` / `--noun`).

## Verify

- [ ] `design_qc.py cuemids cues.srt --probe <t>` lists the frames grabbed.
- [ ] `design_qc.py cuegaps cues.srt --join <t>` passes for every splice.
- [ ] `design_qc.py asrhall words.json` passes, or each hit is logged and
      dropped.
      Words files use `word|w`, `start|s`, `end|e`; phrases match across
      consecutive words.
- [ ] `design_qc.py boundprefix cues.srt` passes after every cue edit.
- [ ] `design_qc.py fadetail --words words.json --end <s> --fade <s>`
      passes.
- [ ] `design_qc.py bidi` passes on the captions and the hook/label text.
- [ ] `design_qc.py silencegap final.mp4 --hook-end <s> --content-end <s>`
      passes hard (>0.6 s FAIL); `--target 0.45` has no soft warnings for
      handoff; `--content-end` = last speech, not end-card.
- [ ] `design_qc.py txdiff cues.srt words.json` reports exact-raw + fuzzy;
      gate on exact-raw.
- [ ] `design_qc.py antecedent kept.txt --epithet <e> --noun <n>` passes
      after every splice.
- [ ] Notes record the scene-cut scan, the reframe tile check and the
      shot-cut onset.

## Sources

- Field lessons from one talk-Shorts batch, verified on rendered frames.
- [FFmpeg select filter](https://ffmpeg.org/ffmpeg-filters.html#select_002c-aselect)
