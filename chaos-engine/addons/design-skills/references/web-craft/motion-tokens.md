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
   control that opened it (set `transform-origin` to the trigger side)
   and closes back towards it. Close faster than open.
4. Reference only: this card records patterns. Copy no text, snippets or
   assets from the source library.

## Verify

- [ ] Grep finds no hard-coded transition durations outside the tokens.
- [ ] With reduced motion emulated in the browser, nothing translates or
      scales.
- [ ] A frame capture of each popover shows it scaling from its trigger.

## Sources

- Patterns referenced from [transitions.dev](https://transitions.dev/)
  by Jakub Antalik. Its custom licence forbids redistributing the
  library, so nothing is copied.
- [MDN: prefers-reduced-motion](https://developer.mozilla.org/en-US/docs/Web/CSS/@media/prefers-reduced-motion)
