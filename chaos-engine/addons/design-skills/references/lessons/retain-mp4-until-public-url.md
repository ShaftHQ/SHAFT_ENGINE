---
name: retain-mp4-until-public-url
description: >-
  Use after render/sign-off through publish; keep the final mp4 until a public
  watch URL is proven.
---

# L59 Retain final mp4 until public URL proof

## Use when

Cleaning renders, closing a worktree, or deleting media after upload/schedule.

## Rules

1. **Keep the final mp4** (and its upload.meta) until a **public URL proof**
   exists (watch URL or Studio public link recorded in inventory / receipt).
2. **Deleting or losing the mp4 before proof = FAIL.** Scheduled-only without
   a public URL still requires the file retained.
3. Gate: `design_qc.py retainmp4` with observations naming `final_mp4`,
   `mp4_present`, and optional `public_url`.

## Verify

- [ ] `retainmp4` fails when `public_url` is empty and `mp4_present` is false
      or the path is missing.
- [ ] Pass when `public_url` is set, or when mp4 is still present awaiting proof.

## Why

Early cleanup after schedule left no bytes to re-upload when Studio failed or
the wrong cut went public.

## Sources

Field retro 2026-10-31 (content pipeline). Companions: L50, L54, L57.
