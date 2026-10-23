# Design resources evaluated (2026-10-24)

Checked on 2026-10-24 against GitHub metadata (licence, last push) and each
README. Nothing is vendored: when a rule is used, the cards re-express it
in their own words with attribution, as [INVENTORY](../../INVENTORY.md)
requires.

| Resource | Licence | Maintenance (last push) | Overlap | Verdict | Reason |
| --- | --- | --- | --- | --- | --- |
| [Taste Skill](https://github.com/Leonxlnx/taste-skill) @ `6010f91` | MIT | active (2026-10-20) | already re-expressed in D03/D07 at pin `ce26fc2` | **reference** (keep the pin) | Its brief → art direction → 3-width self-screenshot loop matches the D04 audit and L04 self-review. Refresh the pin only after reading the upstream diff. |
| [UI UX Pro Max](https://github.com/nextlevelbuilder/ui-ux-pro-max-skill) @ `d14efff` | MIT | very active (2026-10-23) | D05 type, D06 colour, D04 audit | **reference** | A searchable palette, font-pair and UX-rule database is useful for picking options. It is large and changes fast, so link it by pin and don't copy it. Picks still pass D05/D06 contrast and size checks. |
| [design-motion-principles](https://github.com/kylezantos/design-motion-principles) @ `4a9ca87` | MIT | slow (2026-05-30) | D08 motion, C07 | **reference** (candidate to adapt) | Its audit mode (motion-gap analysis, an anti-slop motion checklist, mandatory reduced motion) is a good lens for card fades and CTA motion. Adapt items into D08 only one rule at a time, with attribution and a red-then-green check. |
| [getdesign.md](https://getdesign.md) | catalog: no clear licence; the spec it follows, [google-labs-code/design.md](https://github.com/google-labs-code/design.md) @ `6e5da53`, is Apache-2.0 | catalog active; spec active (2026-10-16) | D02 brand tokens | **skip the catalog, reference the spec** | Catalog entries describe third-party brands' trade dress and carry no licence to reuse. The DESIGN.md spec and its `lint` give a portable token-and-rationale format and a contrast lint for D02. |
| [TypeUI DESIGN.md Extractor](https://github.com/bergside/design-md-chrome) @ `8e07614` | MIT | slow (2026-05-25) | D02 token capture | **reference** (manual tool) | A Chrome extension that runs in the user's own browser. Use it only on a site you own, to draft tokens, then verify each value against the brand source (D02). The agent never installs it. |

## Web-craft resources (verified 2026-10-24)

| Resource | Licence | Maintenance (last push) | Overlap | Verdict | Reason |
| --- | --- | --- | --- | --- | --- |
| [impeccable](https://github.com/pbakaus/impeccable) @ `2bb6563` | Apache-2.0 (NOTICE.md) | very active (2026-10-23) | D03, D04 | **adapt** with attribution (W01) | The AI-tell list, review verbs and the PRODUCT.md/DESIGN.md convention are re-expressed in our own words. Apache-2.0 attribution and its NOTICE are kept by reference. Its CLI calls a remote API with telemetry, so the agent never installs it. |
| [transitions.dev](https://github.com/Jakubantalik/transitions.dev) | custom: use and modify allowed; no redistribution of the library | active (2026-10-23) | D08 | **reference only** (W02) | Patterns only: motion tokens as CSS vars, a reduced-motion fallback, origin-aware open/close. No text or code is copied. |
| [gsap-skills](https://github.com/greensock/gsap-skills) @ `aed9cfd` | MIT | active (2026-07-29) | D08, D09 | **reference** (W03 pointer) | The official ScrollTrigger and core skills are linked by pin. Reduced motion via `gsap.matchMedia()` is mandatory. |
| [make-interfaces-feel-better](https://github.com/jakubkrehel/make-interfaces-feel-better) @ `35545ea` | MIT | active (2026-08-29) | D04 | **adapt** with attribution (W04) | A micro-polish checklist, applied to web, slides and stills. |

Suggested order when a brand task needs them: DESIGN.md spec (plus the
extractor on your own site), then a motion audit lens, then font and palette
picks, then a taste pass on decks and landing pages.
