---
name: motion-principles
description: Use when anything moves on screen and needs easing, timing, staging, hold durations, reduced-motion variants, and flash safety.
---

# D08 Motion principles

## Use when

Before building any animation: web transitions, motion graphics, explainer
scenes, or kinetic type. D09 and D10 consume the tokens defined here.

## Timing tokens

| Token | Duration | Curve |
| --- | --- | --- |
| enter | 400 ms | emphasized decelerate `cubic-bezier(0.05, 0.7, 0.1, 1)` |
| exit | 200 ms | emphasized accelerate `cubic-bezier(0.3, 0, 0.8, 0.15)` |
| move | 300 ms | standard `cubic-bezier(0.2, 0, 0, 1)` |
| micro | 100 to 150 ms | standard |

## Rules

- Easing comes from tokens only. Linear easing is for progress bars and
  constant-speed scrolls, nothing else.
- Entrances combine a fade with a small translate (8 to 24 px) or scale
  (0.96 to 1); exits are faster than entrances.
- Stagger related items by 3 to 6 frames at 30 fps; never animate everything
  at once.
- Stage one focal point at a time; move the eye on purpose.
- Hold on-screen text for at least words / 3 seconds plus 0.5 s; kinetic type
  faster than that is allowed only when the same words are spoken or
  captioned.
- No more than three general flashes in any one-second period; avoid
  saturated red flashes entirely.
- Web motion ships a `prefers-reduced-motion: reduce` variant (crossfade or
  none); anything that plays longer than 5 s can be paused.

## Workflow

1. Copy the tokens into CSS variables or scene constants.
2. Block the animation as key poses with holds; add easing last.
3. Review at full speed and at 25% speed; fix pops and drifting motion.

## Verify

- `design_qc.py easing <src>` reports 0 linear or inline curves outside
  token files (mark token files with `design-qc: allow`).
- `design_qc.py holds storyboard.json` passes.
- `design_qc.py flash render.mp4` passes.
- An e2e test or screenshot shows the reduced-motion variant.

## Sources

Thomas and Johnston, *The Illusion of Life* (twelve principles, paraphrased);
Material 3 motion; Apple HIG motion; NN/g on animation duration; WCAG 2.2
2.2.2, 2.3.1, 2.3.3; engine-neutral rules re-expressed from
haidrrrry/claude-remotion-skill (MIT).
