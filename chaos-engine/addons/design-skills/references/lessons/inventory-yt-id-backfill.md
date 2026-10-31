---
name: inventory-yt-id-backfill
description: >-
  Use after every YouTube schedule or upload; backfill inventory yt_id so no
  scheduled row stays null.
---

# L58 Inventory yt_id backfill

## Use when

A unit is scheduled or uploaded in YouTube Studio, or inventory is about to be
committed after a schedule batch.

## Rules

1. **Backfill immediately.** After every schedule/upload, write the Studio
   `yt_id` (video id) onto the inventory row in the same session.
2. **No stale nulls.** Any row with status `scheduled`, `uploaded`, or
   `public` and a null/empty `yt_id` fails the gate.
3. Gate: `design_qc.py ytidbackfill` on the inventory JSON.

## Verify

- [ ] `ytidbackfill` fails when a scheduled row has `yt_id: null`.
- [ ] Pass when every scheduled/uploaded/public row has a non-empty yt_id.

## Why

Null yt_ids after schedule blocked dedupe, Studio pre-checks, and later
extends — inventory lied about what was already live.

## Sources

Field retro 2026-10-31 (content pipeline). Companions: L57, L43.
