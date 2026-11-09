# cloudflare/security-audit-skill

- URL: https://github.com/cloudflare/security-audit-skill
- Lists: Charlie Hills snapshot, 6 Nov 2026, row 20 (62K stars on that snapshot).
- GitHub description: A coding-agent skill for multi-phase security audits with independently verified, machine-readable findings.
- README: README headings seen: what it does, files, installation, usage, requirements, design principles. The lead says isolated agents do reconnaissance, coverage-led hunting, candidate validation, structured output, independent record verification, and target-neutral reporting.

## Reusable patterns

- A finding is not done until a second pass verifies the record.
- Output is machine-readable and the report is neutral about the target.
- Phases stay separate: look, hunt, validate, record, verify, report.

## Adopt

- ChaosEngine already separates the writer from the reviewer. A verified, machine-readable finding is the idea to keep for our own repositories.
- A security-audit campaign is follow-up https://github.com/ShaftHQ/SHAFT_ENGINE/issues/7882, not a skill body. It must not be an offensive audit of systems we do not own.

## Skip

- Do not copy the skill, its phase prompts, or its finding schema.
- Do not run this study as an audit of anyone else's system.

## License check

- Checked 2026-11-10. GraphQL licenseInfo.spdxId is MIT. gh api repos/cloudflare/security-audit-skill/license returned HTTP 200 and path LICENSE.
- MIT would allow a later, separately reviewed adaptation. This delivery does not edit a skill from this note.

Adopt the idea. Do not copy third-party code. Do not vendor the repository.
