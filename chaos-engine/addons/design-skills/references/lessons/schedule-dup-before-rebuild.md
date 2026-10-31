---
name: schedule-dup-before-rebuild
description: >-
  Use before any rebuild or schedule ask; fail closed on title+window matches
  against the live inventory.
---

# L61 Schedule dup before rebuild/schedule

## Use when

Before assigning a rebuild, re-encode, or schedule ask for a unit that may
already exist in the live inventory / Studio library. Complements
[L57](youtube-studio-preupload-check.md) (Studio receipt) and
[L43](lq-vs-short-dup.md) (long vs Short).

## Rules

1. **Inventory first.** Run `design_qc.py scheduledup --inventory <live.json>
   --title TITLE [--start START --end END]` before render or schedule.
2. **Title + window.** Normalized title match with the same source window (or
   missing window on the candidate) is a dup.
3. **Fail closed.** Matches without `--allow-replace` (+ `--yt-id`) → FAIL.
4. **Missing inventory / title** → FAIL (`missing-inventory` / `missing-title`).

## Verify

- [ ] Missing `--inventory` or file → FAIL.
- [ ] Dup title+window → FAIL; clean inventory → PASS.
- [ ] `--allow-replace` without `--yt-id` → FAIL.

## Why

QA sheet "NOT scheduled" rows triggered rebuilds for units already live under
other ids. Inventory dup must run before rebuild/schedule, not only at upload.

## Sources

Field retro 2026-10-31. Companions: L43, L51, L57, L58.
