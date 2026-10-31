---
name: youtube-studio-preupload-check
description: >-
  Use before uploading or scheduling a Short or cut; prove the content library
  has no conflicting title, and fail closed on duplicates.
---

# L57 YouTube Studio pre-upload check

## Use when

About to upload or schedule a Short, LQ, or chapter. Extends
[L40](youtube-upload-language-order.md) and [L43](lq-vs-short-dup.md).

## Rules

1. **Content-library receipt first.** Before committing a new upload, record a
   content-library receipt that the normalized title (and source / render id
   when known) was checked against already-listed items.
2. **Existing item = FAIL** unless the brief explicitly records
   `allow_replace` with the existing `yt_id` and owner approval. Silent
   re-uploads that create duplicate Shorts are forbidden.
3. Gate: `design_qc.py ytprecheck` with an observations JSON proving the
   check ran (`queried: true`) and listing `matches`.

## Verify

- [ ] `ytprecheck` without observations / without `queried: true` fails.
- [ ] Non-empty `matches` without `allow_replace` + matching `yt_id` fails.
- [ ] Empty matches + queried passes.

## Why

Uploading without a content-library receipt created duplicate Shorts that
inventory and L43 could not see until too late.

## Sources

Field retro 2026-10-31 (content pipeline). Companions: L40, L43, L58.
