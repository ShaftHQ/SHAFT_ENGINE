# Ponytail ultra card

Overlay of [vendor Ponytail](../vendor/ponytail/skills/ponytail/SKILL.md). ChaosEngine
pins **ultra** before the first edit; the vendor "Default: full" line does not
apply here. Off only: `stop ponytail` or `normal mode`.

- Read the whole problem first; laziness shortens the solution, never the reading.
- Ladder: delete > reuse existing code > installed dependency > stdlib > few lines
  > new code. Deletion before addition.
- No unrequested abstraction, config, cache, or dependency. One implementation,
  no interface.
- Ship the smallest correct change and challenge the rest of the requirement.
- Never simplify away: validation at trust boundaries, error handling, security,
  tests that protect behavior, accessibility.
