# anomalyco/opencode

- URL: https://github.com/anomalyco/opencode
- Lists: Charlie Hills snapshot, 6 Nov 2026, row 7 (225K stars on that snapshot).
- GitHub description: The open source coding agent.
- README: README headings seen: install, package managers, a desktop beta, agents, and documentation.

## Reusable patterns

- One coding agent with more than one install surface, and a documented agent list.
- Provider choice stays outside the short README lead.

## Adopt

- ChaosEngine already routes local coding delegates through local-agency and local-coding-delegate. OpenCode stays an external program, not a vendored tree.

## Skip

- Do not vendor the OpenCode client, its desktop app, or its provider list.
- Do not add a cloud or extra-machine runner.

## License check

- Checked 2026-11-10. GraphQL licenseInfo.spdxId is MIT. gh api repos/anomalyco/opencode/license returned HTTP 200 and path LICENSE.
- No skill edit from this note.

Adopt the idea. Do not copy third-party code. Do not vendor the repository.
