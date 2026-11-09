# mattpocock/skills

- URL: https://github.com/mattpocock/skills
- Lists: One note for both lists. Charlie Hills row 2 (316K stars on the 6 Nov 2026 snapshot) and weekly top 10 row 02 for 2-8 Nov 2026 (+9.6k). Not a second repo. The weekly skills row is this repo, not anthropics/skills.
- GitHub description: Skills for Real Engineers. Straight from my .agents directory.
- README: README headings seen: a short install, a setup command, then two failure modes (the agent did not do what was asked; the agent is too verbose).

## Reusable patterns

- Small skills that name one engineering failure, not a kit that owns the whole process.
- A setup command installs the set and stops. The README argues against process kits that take over the repository.

## Adopt

- ChaosEngine already keeps the always-loaded router thin and puts the rest on demand. Keep that shape.
- The verbosity failure is already the Ponytail companion. Do not add a second terseness skill.

## Skip

- Do not copy skill markdown, setup scripts, or the install recipe.
- Do not add a setup command named after this repository.

## License check

- Checked 2026-11-10. GraphQL licenseInfo.spdxId is MIT. gh api repos/mattpocock/skills/license returned HTTP 200 and path LICENSE.
- No skill edit from this note.

Adopt the idea. Do not copy third-party code. Do not vendor the repository.
