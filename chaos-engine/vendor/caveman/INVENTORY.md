# Caveman vendor pin

Owned inventory for the pinned JuliusBrussee/caveman companion. Skill and most
hook bodies track upstream. ChaosEngine applies a project-mode overlay on
`caveman-config.js` and `caveman-activate.js`: when a ChaosEngine router exists
in the walk, mode resolution never falls through to user-level caveman config
(#5698). `caveman-activate.js` also reads the pinned body from
`.chaos-engine/vendor/caveman/` before the plugin copy, because the published
plugin `SKILL.md` is a short [adapter](../../plugin-adapters/INDEX.md) (#6198).
Re-apply that overlay after any upstream pin refresh.

- Pin: [PIN.json](PIN.json)
- License: [LICENSE](LICENSE)
- Skill: [skills/caveman/SKILL.md](skills/caveman/SKILL.md)
- [src/hooks/caveman-activate.js](src/hooks/caveman-activate.js)
- [src/hooks/caveman-config.js](src/hooks/caveman-config.js)
- [src/hooks/caveman-mode-tracker.js](src/hooks/caveman-mode-tracker.js)
- [src/hooks/caveman-parse.js](src/hooks/caveman-parse.js)
- [src/hooks/package.json](src/hooks/package.json)

Refresh by replacing the pin and listed blobs together, then re-apply the
ChaosEngine project-mode overlay and update digests in `PIN.json`.
