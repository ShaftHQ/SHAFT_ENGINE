---
name: avatar-voice-first
description: Use when choosing an avatar or image-to-video path; voice identity is the first dealbreaker, ahead of lip motion.
---

# L20 Avatar voice first

## Use when

Picking or reviewing a digital presenter / avatar path, especially
image-to-video (I2V) from a still, vs a voice-matched talking avatar.

## Rules

1. **Voice is the first dealbreaker.** If the voice is wrong (wrong
   identity, wrong timbre, wrong language rhythm), the package fails
   before lip-sync or face metrics are debated.
2. **I2V from a still lacks voice identity.** Animating a still
   (image-to-video) moves the face; it does **not** supply a
   voice-matched identity. Do not treat I2V motion as a replacement for
   a voice-matched avatar or real presenter audio.
3. **Match voice, then motion.** Lock voice (real take, approved TTS
   clone with licence, or recorded VO) and only then judge mouth/face
   motion against that audio.
4. **Perceptual glance still wins.** Voice-first does not waive
   [L13](perceptual-acceptance.md): human A/B vs real reference remains
   required; metrics stay floors.

## Verify

- [ ] Approach note states the voice source and identity check before
      I2V or lip-sync tooling is chosen.
- [ ] A package with green face/motion metrics and mismatched voice is
      recorded as FAIL on voice, not "residual notes."
- [ ] I2V-only pitches are rejected when the brief requires a
      voice-matched presenter.

## Why

Viewers forgive mild lip error on a familiar voice; they reject a
correct-looking face with the wrong voice immediately. I2V solves motion,
not identity-in-audio.

## Sources

Field lesson from avatar/I2V closeout work: image-to-video without
voice-matched identity is not a substitute for a voice-first avatar path.
