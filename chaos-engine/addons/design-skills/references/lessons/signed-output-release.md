---
name: signed-output-release
description: Use when a render pipeline change could alter outputs an owner already signed off or scheduled, or before asking for sign-off.
---

# L04 Signed-output release

## Use when

A pipeline already has approved (signed) or scheduled outputs and a
visual or gate change is coming. It pairs with
[D20 delivery QC](../delivery-qc.md) and the
[runbook](../pipelines/runbook.md).

## Rules

1. Visual changes ship behind one opt-in flag, and the batch driver sets
   that flag by default for new items. With flags off the output stays
   byte-identical: file md5, audio md5 and `framemd5` match a signed
   output.
2. Gates (checks, not pixels) may tighten globally. Re-run them on every
   signed item as an audit, and never re-render a signed or scheduled item.
3. Every fix needs a repro test that failed before the fix, a throwaway
   proof render (with a temp name), and a before/after crop.
4. Self-review before asking for sign-off: frames from 0-4 s at 10 fps, a
   contact sheet, and the hook-end, video-end and first-cue-after-card
   frames. Mid-answer silence must meet the **≤0.45 s soft target**
   (`silencegap --target 0.45` with no soft warnings; prefer **≤0.40 s**
   mid-gap when splicing) before handoff —
   do not send a near-limit 0.57–0.60 s gap for review. Any **self-flagged
   doubt** (script form vs Latin, lexicon gap, modality risk) must be
   **fixed before handoff** — do not ask the reviewer to decide. Gate:
   `design_qc.py opendoubt SELF-REVIEW.md`.
5. Read pinned inputs with `git show <sha>:<path>`. Scripts are
   all-or-nothing, and rewrites preserve file conventions (for example
   CRLF).
6. On an audio-only relock, re-gate only the cues that changed. A
   transcript-diff waiver needs 0 SRT errors.
7. Never run headless-Chrome renders in parallel on one shared profile.
   One Short active at a time: build → self-review → accept → native
   schedule before starting the next clip. Parallel renders on a shared
   Chrome profile lose hand-fixed SRTs.
8. **G16 Shorts description:** METADATA `## Description` is exactly four
   lines — no freeform paraphrase, no invented spliced-out speech:
   (1) `مقطع من الـ Q&A: ` + **same** Title string (≤70);
   (2) `الإجابة كاملة: ` + canonical
   `https://www.youtube.com/watch?v=<VIDEO_ID>&t=<START_SECS>s`;
   (3) project site URL; (4) project hashtag line. Gate before handoff:
   `design_qc.py metadatadesc METADATA.md --site <url> --tags "<tags>"`
   (write `qc/<rev>/metadata_desc_gate.json`).
9. **G21/G22 title polarity + kept binding:** token presence ≠ meaning.
   Per title segment (split on —), source-cue negation lemmas stay in
   that segment. G22: ordered content-token match ⊆ kept spans
   (complement of `--splice`); claim must not live only in a cut.
   Gate: `design_qc.py titlepolarity --title T --srt cues.srt`
   `[--words W --splice a-b]` (or `--metadata`).

## Verify

- [ ] With flags off, the three md5 checks match a signed output.
- [ ] Every signed item was audited with the new gates, and none was
      re-rendered.
- [ ] A relock re-gated the changed cues; any waiver shows 0 SRT errors.
- [ ] Each fix links its red repro, its proof render and its before/after
      crop.
- [ ] Only one Short is in flight through schedule; no parallel Chrome
      profile renders.
- [ ] `silencegap --target 0.45` shows no soft warnings before handoff (prefer ≤0.40)
- [ ] `txdiff … --words-raw` reports normalized + ASR exact-raw (G20)
- [ ] `orthography` passes on title/short/hook/SRT (G19)
      (hard >0.6 s already FAIL).
- [ ] `design_qc.py opendoubt SELF-REVIEW.md` passes (no open self-flagged
      doubts).
- [ ] `design_qc.py metadatadesc METADATA.md --site … --tags …` passes
      (exact 4-line desc; site/tags are project fill-ins).
- [ ] `design_qc.py titlepolarity --title … --srt … [--words … --splice …]`
      passes (G21 polarity; G22 claim ⊆ kept when words given).

## Sources

- [FFmpeg framemd5 muxer](https://ffmpeg.org/ffmpeg-formats.html#framemd5)
- [Martin Fowler: feature toggles](https://martinfowler.com/articles/feature-toggles.html)
- Field lessons from one talk-Shorts batch.
