# Caveman vendor pin

Owned inventory for the pinned JuliusBrussee/caveman companion. Skill and most
hook bodies track upstream. ChaosEngine applies a project-mode overlay on
`caveman-config.js` and `caveman-activate.js`: when a ChaosEngine router exists
in the walk, mode resolution never falls through to user-level caveman config
(#5698). Upstream v3 stores the pinned ultra intensity as `ultracave`, so that
overlay returns `ultracave`. `caveman-config.js` `skillPathCandidates` reads
the pinned body from `.chaos-engine/vendor/caveman/skills/<mode>/` before the
plugin copy, because the published plugin `skills/caveman/SKILL.md` is a short
[adapter](../../plugin-adapters/INDEX.md) (#6198). `caveman-activate.js` loads
that ruleset through `loadRuleset`. Re-apply that overlay after any upstream
pin refresh.

- Pin: [PIN.json](PIN.json)
- License: [LICENSE](LICENSE)
- Skill: [skills/caveman/SKILL.md](skills/caveman/SKILL.md)
- Ultra intensity: [skills/ultracave/SKILL.md](skills/ultracave/SKILL.md)
- [src/hooks/caveman-activate.js](src/hooks/caveman-activate.js)
- [src/hooks/caveman-config.js](src/hooks/caveman-config.js)
- [src/hooks/caveman-mode-tracker.js](src/hooks/caveman-mode-tracker.js)
- [src/hooks/caveman-parse.js](src/hooks/caveman-parse.js)
- [src/hooks/package.json](src/hooks/package.json)

Refresh by replacing the pin and listed blobs together, then re-apply the
ChaosEngine project-mode overlay and update digests in `PIN.json`.
