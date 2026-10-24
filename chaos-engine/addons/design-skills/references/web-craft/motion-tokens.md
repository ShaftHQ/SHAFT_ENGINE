---
name: motion-tokens
description: Use when defining UI transitions, open or close motion for menus, popovers and dialogs, or a reduced-motion fallback on the web.
---

# W02 Motion tokens

## Use when

A web UI needs transitions. It extends
[D08 motion principles](../motion-principles.md).

## Rules

1. Motion tokens are CSS custom properties (durations and easings, for
   example `--motion-fast`, `--ease-out`). Components read the tokens, so
   no component hard-codes a duration.
2. Every transition has a `prefers-reduced-motion: reduce` fallback:
   remove movement and keep at most a short opacity change. State changes
   must not depend on motion.
3. Open and close are origin-aware: a popover or menu grows from the
   control that opened it and closes back towards it. Close faster than
   open. The origin mirrors in `dir="rtl"`: derive it from logical sides
   (inline-start/inline-end), never a hard-coded left/right
   `transform-origin`.
4. Should it animate? Skip motion on high-frequency actions (typing,
   list navigation, repeated toggles, keyboard shortcuts); animate rare,
   spatial changes only.
5. Never animate from `scale(0)`: enter from `scale(0.9)`-`scale(0.97)`
   with opacity, so the element never appears from nothing.
6. Prefer CSS transitions over keyframes for state changes: transitions
   are interruptible and retarget mid-flight.
7. Animate `transform` and `opacity` only (plus `mask`/`clip-path` for
   reveals); never layout properties.
8. A short blur masks a rough swap (icon or text change) better than a
   bare crossfade.
9. Tooltips: delay the first; subsequent peers open instantly.
10. Drag and gesture-driven motion uses springs, not fixed durations.
11. No hover effects on touch: gate them with `@media (hover: hover)`.
12. Image hover-reveal motion: see [W04 micro-polish](micro-polish.md);
    it reads these tokens and the reduced-motion rule above.
13. Rules 1-3 are reference only from transitions.dev. Copy no text,
   snippets or assets from that library. Rules 4-11 are adapted from
   emilkowalski/skills with attribution.

## Verify

- [ ] Grep finds no hard-coded transition durations outside the tokens.
- [ ] With reduced motion emulated in the browser, nothing translates or
      scales.
- [ ] A frame capture of each popover shows it scaling from its trigger,
      in both `dir="ltr"` and `dir="rtl"`.
- [ ] No `scale(0)` and no animated layout property: `design_qc.py uimotion <src>`
      passes; `design_qc.py hovergate <src>` passes.
- [ ] High-frequency actions change state with no transition.

## Sources

- Patterns referenced from [transitions.dev](https://transitions.dev/)
  by Jakub Antalik. Its custom licence forbids redistributing the
  library, so nothing is copied.
- Rules 4-11 adapted with attribution from [emilkowalski/skills](https://github.com/emilkowalski/skills) @2d22863 by Emil Kowalski, MIT; nothing is vendored.
- [MDN: prefers-reduced-motion](https://developer.mozilla.org/en-US/docs/Web/CSS/@media/prefers-reduced-motion)
