---
name: gsap-web-motion
description: Use when a web page needs scroll-linked, pinned or timeline motion and GSAP is the chosen or a candidate library.
---

# W03 Web motion with GSAP

## Use when

A page needs scroll-driven or sequenced motion. Timing and easing come
from [D08 motion principles](../motion-principles.md); this card covers
the library.

## Rules

1. For ScrollTrigger, follow the official `gsap-scrolltrigger` skill:
   animate the children of a pinned element, not the element itself;
   remove `markers` before shipping; call `ScrollTrigger.refresh()` after
   layout changes.
2. Reduced motion is mandatory. Wrap setup in `gsap.matchMedia()` with a
   `(prefers-reduced-motion: reduce)` branch that skips movement, so
   reverting is automatic.
3. Durations and eases come from D08 and the
   [W02 motion tokens](motion-tokens.md), not from per-call constants.
4. Link the official skills by pin; never copy them in.

## Verify

- [ ] With reduced motion emulated, no ScrollTrigger animation moves
      anything.
- [ ] A production build has no `markers: true`.
- [ ] Every duration and ease traces back to a token or a D08 value.

## Sources

- [greensock/gsap-skills](https://github.com/greensock/gsap-skills)
  @aed9cfd, MIT: `gsap-scrolltrigger` and `gsap-core` (`gsap.matchMedia()`).
