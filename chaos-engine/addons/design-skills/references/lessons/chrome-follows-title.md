---
name: chrome-follows-title
description: Use when a retitle or title lock must re-render burned-in chrome and keep overlays off the endcard.
---

# L42 Chrome follows title locks

## Use when

A title changes (retitle / lock / dup fix) on a pack that burns chrome, topic pills, or title plates into the video.

## Rules

1. Re-render **every burned-in surface** that echoes the hook — not metadata alone (thumb / desc / schedule title are insufficient).
2. Scope overlays to the **body only**; check the endcard is clean.
3. Prefer frame-index enables (`lt(n,<first_endcard_frame>)`) over float time when PTS sits under a rounded body end.
4. After render: last body frame shows the pill; first endcard + mid + last frames are clean (pixel MAD ≈ codec noise vs prior clean endcard).
5. Do not re-overlay a full karaoke caption band onto a final that already has ASS burned in.

## Verify

- [ ] Pack sha256 / duration refreshed after chrome re-render.
- [ ] Endcard frames clean vs prior clean plate.
- [ ] Audio stream unchanged when only chrome was retitled.

## Sources

- Lesson index row deferred until after the Coach video sign-off PR merges.

- Field FAIL: metadata title A while mp4 chrome still showed the old title; full-file overlay bled onto the endcard.
