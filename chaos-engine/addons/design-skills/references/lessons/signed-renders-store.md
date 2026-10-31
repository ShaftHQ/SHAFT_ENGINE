---
name: signed-renders-store
description: >-
  Use on SIGN of a unit with a final mp4; copy to the protected signed-renders
  store and treat that path as never-touch.
---

# L62 Protected signed-renders store

## Use when

Orchestrator SIGN of a media unit that has a final mp4 (and companions).
Extends [L54](checkpoint-generated-assets.md) and
[L59](retain-mp4-until-public-url.md).

## Rules

1. **Copy on SIGN.** `cp -a` mp4 (+ srt/thumb/meta) →
   `signed-renders/<UNIT>/` under the workspace root and write `SIGNED.json` with unit,
   sha256s, sign commit, render_rev, signed_at (owner TZ label).
2. **Never-touch.** Path segment `signed-renders` is delete-guard never-touch
   (no owner code unlock for deletes inside it).
3. **Bit-exact reuse.** Rebuild asks that match a signed sha need no re-review.
4. **Personal names out of repo text.** Unit ids only in receipts.

## Verify

- [ ] `delete_guard.never_touch(.../signed-renders/...)` returns never-touch.
- [ ] L39 / cleanup-at-close checklist names the store.
- [ ] Missing signed copy before schedule when policy requires it → FAIL note.

## Why

Signed finals lived only under gitignored `renders/` and were deleted in
cleanup, forcing bit-exact rebuilds.

## Sources

Field retro 2026-10-31. Companions: L54, L59, cleanup-at-close.
