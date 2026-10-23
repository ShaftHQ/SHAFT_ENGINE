---
name: anti-generic-lint
description: Use when reviewing any visual surface (web, slide, still, video card) for template-made tells, naming a review pass, or setting up per-project design context.
---

# W01 Anti-generic lint

## Use when

A web page, slide, still or video card is designed or reviewed. It
extends [D03 web visual direction](../web-visual-direction.md) and
[D04 web interface audit](../web-interface-audit.md).

## Rules

1. Lint for the generic tells. Each one is a finding unless the brief
   explicitly earns it:
   - one default face for everything, every script (for example Inter or
     a system face for Latin and Arabic alike). A deliberate Latin face
     paired with a proper Arabic or other-script face is not a finding;
   - a purple-to-blue gradient, including gradient text;
   - card-in-card (nested cards), or same-size icon cards as the whole
     layout;
   - grey-on-colour: grey text on a coloured surface (tint it from the
     surface hue instead);
   - emoji icons, or Unicode glyphs standing in for an icon set.
2. The lint applies to all visual surfaces, not only web. Slides and
   stills fail the same tells.
3. Use review verbs to name a pass, one per pass: `bolder`, `quieter`,
   `distill`, `typeset`, `layout`, `harden`, `polish`, `critique`,
   `audit`. A verb names the direction of a change, not the change.
4. Each project keeps its design context in two files at its root:
   `PRODUCT.md` (users, purpose, platform, tone) and `DESIGN.md` (tokens
   and rules, following the Google DESIGN.md spec). Read both before
   designing, and change `DESIGN.md` only with owner approval.

## Verify

- [ ] Lint report: each of the 5 tells is listed as absent or as earned
      by the brief, with the brief line quoted.
- [ ] The review names its verb, and every change it lists matches it.
- [ ] `PRODUCT.md` and `DESIGN.md` exist and were read first, and
      `DESIGN.md` passes the spec's lint.

## Sources

- Adapted with attribution from [Impeccable](https://github.com/pbakaus/impeccable)
  @2bb6563 by Paul Bakaus, Apache-2.0. Its NOTICE.md is kept by
  reference; no files are vendored.
- [Google DESIGN.md spec](https://github.com/google-labs-code/design.md) @6e5da53, Apache-2.0.
