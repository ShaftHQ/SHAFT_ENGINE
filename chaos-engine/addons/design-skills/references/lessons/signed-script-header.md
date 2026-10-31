---
name: signed-script-header
description: Use when Coach (or the design owner) signs a video/avatar script; mark the script header as signed in the same edit.
---

# L55 Signed script header

## Use when

A script receives design/Coach sign-off before shot generation.

## Rules

1. On sign-off, set the script header to **signed** (status field or
   `<!-- signed: <role> <ISO-date> -->` near the top). Do not leave an
   unsigned header after the sign-off message.
2. Wardrobe / shot generation gates that require a signed script fail when the
   header is still unsigned.

## Verify

- [ ] After a sign-off edit, the script file contains an explicit signed marker.
- [ ] A render brief that cites an unsigned script is rejected.

## Sources

Field retro 2026-10-31 (Coach sign-off). Companions: L48, L53.
