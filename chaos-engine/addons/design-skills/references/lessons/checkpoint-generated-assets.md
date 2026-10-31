---
name: checkpoint-generated-assets
description: Use when a render, encode, or generated media finishes; checkpoint or commit the asset immediately so a box restart cannot erase it.
---

# L54 Checkpoint generated assets

## Use when

A pipeline just wrote an mp4, wav, contact sheet, ASS/SRT burn, or other
generated artifact under a volatile path. Extends
[checkpoint-resume](../../../../references/checkpoint-resume.md) and
[L46](render-freshness-sha256.md).

## Rules

1. **Checkpoint or commit immediately.** As soon as a generated asset lands,
   run `tool.py checkpoint` naming the asset path **or** stage only those explicit named paths and commit (never `git add -A` / `commit -a`). Do not wait for the full
   QC pass if a box restart would drop the file.
2. **Keep finals under `/workspace`.** Scratch outside `/workspace` is lost on
   Update Bot / recover.
3. **Name the next step.** The checkpoint `Resume at:` line must say whether
   the next action is QC, upload, or re-render.

## Verify

- [ ] After a render step, checkpoint JSON or commit lists the new asset path.
- [ ] Adversarial: kill the session before QC → replacement worker finds the
      asset from the checkpoint, not from chat.

## Why

Box restarts wiped uncommitted renders and forced full re-encodes. Durable
paths plus an immediate checkpoint close that hole.

## Sources

Field retro 2026-10-31. Companions: checkpoint-resume, L46, bot-entry box-durability.
