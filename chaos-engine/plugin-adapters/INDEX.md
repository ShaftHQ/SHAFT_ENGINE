# Vendor plugin adapters

Short `SKILL.md` adapters published as `plugins/<vendor>/skills/<vendor>/SKILL.md`
so hosts list a bounded description while the pinned body stays in
`vendor/` (#6198). Descriptions match the harness index.

- [caveman](caveman/SKILL.md): body [vendor/caveman](../vendor/caveman/skills/caveman/SKILL.md);
  the Caveman activate hook reads the vendor body first.
- [icm-architect](icm-architect/SKILL.md): body
  [vendor/icm-architect](../vendor/icm-architect/skills/icm-architect/SKILL.md).
- Ponytail has no adapter: its pinned hooks read the plugin copy verbatim.
