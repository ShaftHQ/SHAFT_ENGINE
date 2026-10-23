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
   frames.
5. Read pinned inputs with `git show <sha>:<path>`. Scripts are
   all-or-nothing, and rewrites preserve file conventions (for example
   CRLF).
6. Never run headless-Chrome renders in parallel on one shared profile.

## Verify

- [ ] With flags off, the three md5 checks match a signed output.
- [ ] Every signed item was audited with the new gates, and none was
      re-rendered.
- [ ] Each fix links its red repro, its proof render and its before/after
      crop.

## Sources

- [FFmpeg framemd5 muxer](https://ffmpeg.org/ffmpeg-formats.html#framemd5)
- [Martin Fowler: feature toggles](https://martinfowler.com/articles/feature-toggles.html)
- Field lessons from one talk-Shorts batch.
