# heygen-com/hyperframes

- URL: https://github.com/heygen-com/hyperframes
- Lists: Charlie Hills snapshot, 6 Nov 2026, row 17 (80K stars on that snapshot).
- GitHub description: Write HTML. Render video. Built for agents.
- README: README headings seen: quick start, use with an AI coding agent, skills, plugin packages, a core set, an on-demand set, and a manual CLI.

## Reusable patterns

- Video is HTML, CSS, and media rendered to a deterministic MP4, not a one-off editor session.
- Agent skills are a thin layer over a local CLI, with a separate hosted studio called out as a different surface.

## Adopt

- ChaosEngine video work already lives in the design add-on. Deterministic local render is the idea to keep. Do not add a video skill in this delivery.

## Skip

- Do not vendor the renderer, the studio, or the skill pack.
- Do not add a cloud render step.

## License check

- Checked 2026-11-10. GraphQL licenseInfo.spdxId is Apache-2.0. gh api repos/heygen-com/hyperframes/license returned HTTP 200 and path LICENSE.
- No skill edit from this note.

Adopt the idea. Do not copy third-party code. Do not vendor the repository.
