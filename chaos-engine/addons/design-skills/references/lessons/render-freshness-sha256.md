---
name: render-freshness-sha256
description: Use when a video fix round claims a re-render; prove output mp4 sha256 differs from the prior round and fail REUSE_MP4.
---

# L46 Render freshness (mp4 sha256)

## Use when

A fix round after a video review FAIL claims a re-render. Extends [L04](signed-output-release.md)
and [L13](perceptual-acceptance.md).

## Rules

1. **New sha256 required.** The fix-round output mp4 sha256 must differ from the
   prior round's. Gate: `renderfresh` with `--prior-sha256` or `--prior`.
2. **REUSE_MP4 fails.** An env `REUSE_MP4` truthy value or `--reuse-mp4` fails
   the gate even when hashes would otherwise differ — reuse is not a re-render.

## Verify

- [ ] `design_qc.py renderfresh out.mp4 --prior-sha256 <same>` fails
      `mp4-sha256-unchanged`.
- [ ] `REUSE_MP4=1 design_qc.py renderfresh out.mp4 --prior other.mp4` fails
      `REUSE_MP4`.
- [ ] Distinct prior vs current hashes pass.

## Why

A review round that "re-renders" but ships the previous bytes cannot fix
timing, freeze, or caption defects. Hash equality (or an explicit reuse flag)
is the honest proof that no new render happened.

## Sources

Field retro 2026-10-30: fix round claimed re-render that never happened
(role-neutral). Companions: L04, L13, L39.
