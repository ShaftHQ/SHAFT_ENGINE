---
name: render-meta-rev-sha
description: Use when a fix round regenerates upload/render meta; fail if meta.render_rev or sha disagrees with the current render.
---

# L50 Render meta rev/sha

## Use when

A rerender updates upload metadata or a render receipt beside the mp4.
Extends [L46](render-freshness-sha256.md).

## Rules

1. **Regenerate meta on every render.** Stale meta from a prior round fails.
2. **`meta.render_rev` and `meta.sha` (or sha256) must match** the current
   output bytes / revision. Gate: `design_qc.py rendermeta`.
3. L46 still applies: mp4 sha256 must differ from the prior round; `REUSE_MP4`
   fails.

## Verify

- [ ] `design_qc.py rendermeta out.mp4 --meta meta.json` fails on sha mismatch
      or missing rev.
- [ ] Matching rev+sha passes; then `renderfresh` still required vs prior round.

## Why

Gates that trust meta without binding it to the file under review accept
renames and copy-forwards.

## Sources

Field retro 2026-10-31 (render meta). Companions: L46, L04.
