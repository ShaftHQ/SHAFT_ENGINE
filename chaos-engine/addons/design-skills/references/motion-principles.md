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
  none); anything that plays longer than 5 s can be paused. Global overrides
  use `0.01ms`, not `0s` ([D04](web-interface-audit.md)).
- Check stills mid-transition, not only settled frames. A crossfade between
  two busy layouts is a muddy double exposure: stagger it (old out, then new
  in) or dip through the background.
- When the brand's motion language uses scroll-drawn lines or count-ups:
  animate `transform` or `stroke` only; scroll-linked opacity freezes
  part-faded. The end state is fully visible, and reduced motion shows it.
- Count-ups: on load is fine above the fold. Below a tall hero an on-load
  count-up finishes off-screen, so use a scroll-linked one
  (`animation-timeline: view()`, an `@property` integer counter) that
  completes by about 30% of entry, so the number is final wherever it is
  readable. The real number stays in the DOM as text for screen readers,
  browsers without support and reduced motion.
- Card reveals: transform only (translateY about 18 px plus scale 0.97,
  stagger 0/5/10%), done by about 25-35% of the entry range so cards settle
  in the lower viewport. No opacity in the scroll range. Reduced motion is
  static.
- Probe scroll motion at three points (enter, mid, done) per breakpoint and
  record transform and counter values as JSON. Pass means identity
  transform and final numbers at done.
- One motion focus per viewport: no muted video loop beside an animated hero.


## Composition and camera (motion graphics)

- One camera move per beat: push, pan, follow, or parallax — not stacked.
  Hero camera moves last **800–2000 ms** with ease-in-out; never linear
  except continuous loops.
- Elements enter and exit through the **nearest frame edge**, following
  reading direction (LTR: new content from right/below; back/undo reverses).
  Ease-out on enter, ease-in on exit.
- Depth layers map to motion roles: background = ambient (parallax ~0.2×),
  midground = secondary (~0.6×), foreground = primary (~1.0–1.2×). Never
  give two layers competing primary motion.

## Title-safe and aspect margins

Keep critical content (text, logos, faces, CTAs) inside title-safe margins.
**9:16 Shorts** use the pixel map in [V07](video-craft/shorts-safe-area-captions.md)
(top 168 px keep-out; content band y 168–1330 on 1080×1920) — do not invent a
second 9:16 gate. Percentage table below is for planning and for aspects
V07 does not cover; convert to pixels at the delivery size and cross-check
V07 when the cut is vertical.

| Aspect | Title-safe (plan) | Notes |
| --- | --- | --- |
| 16:9 | ~5% all sides | YouTube / web hero |
| 9:16 | see V07 px map | platform UI eats top and bottom |
| 1:1 | ~6–8% all sides | feed |
| 4:5 | ~6% sides, ~8% top/bottom | taller feed |

Restack horizontal layouts for 9:16; do not crop the sides off a 16:9 master.

## Seam / vector continuity (multi-scene MG)

When scenes join, how A exits sets how B enters: same axis, same direction,
matched speed, cut mid-motion on both sides. Prefer a concrete carrier
(cursor, container, mark) across the cut. Do not vendor seam-gate scripts;
record exit/entry vectors in the storyboard and verify on a contact sheet.

## Idle motion and climax holds

- Ban idle wobble used only to fill time (breathe, float, glow-pulse loops
  with no story). Every held phase needs a named route: staged reveals,
  intentional camera, sequenced UI life, or animated action.
- Stillness before climax: schedule a **0.3–0.75 s** pause between the major
  action and its result.
- Measurable: `design_qc.py motionstill` — longest still ≤3.0 s; end-hold
  ≥30 frames at delivery fps; no float/breathe overlays added only to game
  the meter.

## Workflow

1. Copy the tokens into CSS variables or scene constants.
2. Block the animation as key poses with holds; add easing last.
3. Review at full speed and at 25% speed; fix pops and drifting motion.

For UI motion tokens and GSAP, see [W02](web-craft/motion-tokens.md) and
[W03](web-craft/gsap-web-motion.md).

## Verify

- `design_qc.py easing <src>` reports 0 linear or inline curves outside
  token files (mark token files with `design-qc: allow`).
- `design_qc.py holds storyboard.json` passes.
- `design_qc.py flash render.mp4` passes.
- `design_qc.py fadeonly|springs|motiondur|chrome <src>` pass: entries move,
  at most one visible spring, median move <= 0.6 s, no HUD chrome
  ([C07](craft/motion-graphics-craft.md)).
- An e2e test or screenshot shows the reduced-motion variant.

Web UI checks (should-it-animate, no `scale(0)`, interruptible
transitions): [W02](web-craft/motion-tokens.md).

## Sources

Thomas and Johnston, *The Illusion of Life* (twelve principles, paraphrased);
Material 3 motion; Apple HIG motion; NN/g on animation duration; WCAG 2.2
2.2.2, 2.3.1, 2.3.3; engine-neutral rules re-expressed from
haidrrrry/claude-remotion-skill (MIT); composition and camera rules re-expressed from iart-ai/motion-design-skills (MIT, pin in INVENTORY); seam / idle-wobble / stillness-before-climax paraphrased from heygen-com/hyperframes motion-doctrine (Apache-2.0); motionstill thresholds inspired by anything2explainer patterns (PolyForm-NC — own implementation, no upstream code).
