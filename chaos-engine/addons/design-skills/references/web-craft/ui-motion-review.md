---
name: ui-motion-review
description: Use when reviewing UI animation and interaction feel: durations, easing, origins, stagger, reduced motion, pointer handling and interruptions.
---

# W10 UI motion review

## Use when

UI motion or drag interaction is reviewed. Tokens live in
[W02 motion tokens](motion-tokens.md); film motion in [D08](../motion-principles.md).

## Rules

1. **No motion** on keyboard-initiated actions or actions used 100+ times
   a day.
2. **UI motion 300 ms or less** unless justified; never `ease-in` on UI
   enter; no `transition: all`.
3. **Never from `scale(0)`:** start at 0.9-0.97 plus opacity; popovers
   scale from the trigger origin.
4. **Stagger** group entrances 30-80 ms; slow the deliberate phase, snap
   the response.
5. **Reduced motion is gentler, not zero:** keep opacity and colour, drop
   movement. Hover effects sit inside `@media (hover: hover) and
   (pointer: fine)`.
6. **Pointer feel:** highlight on pointer-down, commit on up; about 10 px
   hysteresis before a drag picks a direction; never lock input during
   a transition; an interrupted animation starts from the live
   on-screen value.
7. **Fix order:** remove, then shorten, then re-ease, then re-origin.

## Verify

- [ ] `design_qc.py a11yscan src/` (transition: all, ease-in, over 300 ms)
- [ ] `design_qc.py uimotion src/` and `design_qc.py hovergate src/`
- [ ] Reduced-motion pass recorded; an interrupted drag checked by hand.

## Sources

- [emilkowalski/skills review-animations, apple-design](https://github.com/emilkowalski/skills/tree/2d228630ba1446365f614a108cb8252f1feda202)
  @ `2d228630ba1446365f614a108cb8252f1feda202` (MIT, adapted).
