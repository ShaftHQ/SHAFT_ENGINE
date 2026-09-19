# ICM Architect vendor pin

Owned inventory for the pinned RinDig/icm-architect companion. Skill, references,
and templates track upstream. ChaosEngine nests the upstream skill root under
`skills/icm-architect/` so host plugins discover it the same way as Caveman and
Ponytail. No ChaosEngine overlay is applied to upstream bodies.

- Pin: [PIN.json](PIN.json)
- License: [LICENSE](LICENSE)
- Skill: [skills/icm-architect/SKILL.md](skills/icm-architect/SKILL.md)
- References: [skills/icm-architect/references/](skills/icm-architect/references/)
- Templates: [skills/icm-architect/assets/templates/](skills/icm-architect/assets/templates/)

Refresh by replacing the pin and listed blobs together, then update digests in
`PIN.json`. ICM Architect is advisory: use on design / workspace-structure work,
not as a required implement-path intensity companion.
