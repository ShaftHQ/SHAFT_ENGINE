---
name: threejs-post-inspire
description: Use when a web page uses three.js post-processing such as bloom, and the canvas breaks on resize or bloom looks wrong.
---

# W11 three.js post-processing (inspiration)

## Use when

A three.js scene uses an EffectComposer. Inspiration only; check the
current three.js docs before relying on an API name.

## Rules

1. On resize, call `renderer.setSize` and also `composer.setSize`, and
   update the camera aspect.
2. Bloom output is glow only: add it to the scene pass, don't replace
   the scene with it.
3. Respect reduced motion: stop camera drift and auto-rotation.

## Verify

- [ ] Resize the window: no stretched or blurry canvas.
- [ ] Bloom off and on: the base scene is unchanged underneath.

## Sources

- [cloudai-x/threejs-skills threejs-postprocessing](https://github.com/cloudai-x/threejs-skills/tree/0e181866f095b5f06d005133876e926515390d95)
  @ `0e181866f095b5f06d005133876e926515390d95` (MIT, inspiration).
