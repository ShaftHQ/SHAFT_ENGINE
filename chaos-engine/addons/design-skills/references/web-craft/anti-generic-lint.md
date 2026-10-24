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
   - emoji icons, or Unicode glyphs standing in for an icon set;
   - an eyebrow label over every heading;
   - tracked all-caps labels as the default label style (Arabic and other
     caseless scripts have no caps: judge the Latin labels only).
2. The lint applies to all visual surfaces, not only web. Slides and
   stills fail the same tells.
3. Use review verbs to name a pass, one per pass: `bolder`, `quieter`,
   `distill`, `typeset`, `layout`, `harden`, `polish`, `critique`,
   `audit`. A verb names the direction of a change, not the change.
4. Each project keeps its design context in two files at its root:
   `PRODUCT.md` (users, purpose, platform, tone) and `DESIGN.md` (tokens
   and rules, following the Google DESIGN.md spec). Read both before
   designing, and change `DESIGN.md` only with owner approval.
5. Gate tiers. Every finding carries one tier:
   - **Floor** blocks delivery: contrast, focus, reduced motion, broken
     mobile layout, and invented facts. Invented facts are Floor on all
     surfaces (web, slide, still, video, caption): every number, quote,
     logo or claim traces to a source in the brief.
   - **Reflex** (the tells above and house defaults): fix, or overrule
     with a reason on record in the review.
   - **Finish**: advise, never block.
6. Built from the job: each page category has one primary action, and the
   page ends on it. The layout follows from that action, not from a theme.
7. Credit openly licensed photos next to the image or in the page credits
   (author, licence, link).

## Verify

- [ ] Lint report: each of the 5 tells is listed as absent or as earned
      by the brief, with the brief line quoted.
- [ ] Every finding has a tier; no Floor finding is open; every Reflex
      overrule quotes its reason.
- [ ] Each fact on the surface maps to a brief source line.
- [ ] The page's last section holds its primary action.
- [ ] The review names its verb, and every change it lists matches it.
- [ ] `PRODUCT.md` and `DESIGN.md` exist and were read first, and
      `DESIGN.md` passes the spec's lint.

## Sources

- Adapted with attribution from [Impeccable](https://github.com/pbakaus/impeccable)
  @2bb6563 by Paul Bakaus, Apache-2.0. Its NOTICE.md is kept by
  reference; no files are vendored.
- Gate tiers, built-from-the-job and the two label tells adapted with
  attribution from [hallmark](https://github.com/Nutlope/hallmark) @7ec8682 by Nutlope, MIT. Its theme catalog is not used; nothing is vendored.
- [Google DESIGN.md spec](https://github.com/google-labs-code/design.md) @6e5da53, Apache-2.0.
