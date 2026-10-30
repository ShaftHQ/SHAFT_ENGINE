---
name: avatar-frozen-tail
description: Use when QC'ing avatar or talking-head video; frame-diff the last 2s and any span after speech ends — a static frame fails.
---

# L47 Avatar frozen-tail frame-diff

## Use when

Signing off avatar / talking-head renders where the face can freeze after
speech. Extends [L06](caption-video-qc.md) and [L20](avatar-voice-first.md).

## Rules

1. **Last 2 s must move.** Frame-diff the final 2 seconds; a static stretch
   longer than the gate max fails.
2. **After speech must move or be trimmed.** Any span after speech ends is
   checked the same way. Remediation: trim to speech end plus a short natural
   tail — do not leave a frozen hold.
3. Gate: `frozentail` (`--tail 2`, optional `--speech-end`; default speech end
   from audio RMS).

## Verify

- [ ] A clip whose last 2 s are a still frame fails `frozentail`.
- [ ] A clip with motion through the tail and no post-speech hold passes.
- [ ] Post-speech frozen hold fails even when the pre-speech body moves.

## Why

Frozen avatar tails read as a render glitch. Hash-only or loudness-only SIGN
misses them; frame-diff on the tail and post-speech window catches them.

## Sources

Field retro 2026-10-30: avatar frozen after speech (role-neutral). Companions:
L06, L20, L39.
