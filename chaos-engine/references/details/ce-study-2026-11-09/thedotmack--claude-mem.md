# thedotmack/claude-mem

- URL: https://github.com/thedotmack/claude-mem
- Lists: One note for both lists. Charlie Hills row 11 (114K stars on the 6 Nov 2026 snapshot) and weekly top 10 row 06 for 2-8 Nov 2026 (+6.4k). Not a second repo.
- GitHub description: Persistent context across sessions. Captures what the agent does, compresses it, and injects relevant context into later sessions.
- README: README headings seen: quick start, other agents, what you get, documentation, best practices.

## Reusable patterns

- Memory is a pipeline: capture, compress, reinject. The reinjected slice is selected, not the whole transcript.
- The same store is described for more than one agent host.

## Adopt

- ChaosEngine already has Memory, MemPalace, and Graphify, selected at entry. Keep one retrieve per task area. Do not add a second capture pipeline.

## Skip

- Do not vendor claude-mem or its compression prompt.
- Do not inject session text into the always-loaded identity or core card.

## License check

- Checked 2026-11-10. GraphQL licenseInfo.spdxId is Apache-2.0. gh api repos/thedotmack/claude-mem/license returned HTTP 200 and path LICENSE.
- No skill edit from this note.

Adopt the idea. Do not copy third-party code. Do not vendor the repository.
