`validate_stale_references` in `scripts/ci/validate_agent_guidance.py` is `if stale_reference in content` -- a raw substring test, not a path resolution and not an existence check. It runs over every file matched by `reference_scan_globs`, which includes `.agents/skills/act-as-mohab/references/**/*.md`, `.agents/skills/README.md` and `.agents/skills/*/SKILL.md`.

Consequence, and it is not obvious from the key's name: an entry in `stale_references` that ends in `/` bans a whole namespace forever, including files that do not exist yet. `scripts/ci/agent_guidance_budget.json` currently lists `references/tdd/` and `references/test-driven-development.md`. So when adding a new TDD-related reference:

- A `references/tdd/` SUBDIRECTORY is unusable. Any markdown link to a file inside it contains the banned substring, so every linking file fails `stale-reference` -- including the entrypoint and the skills map. The failure names the linking file, not the new file, which sends you looking in the wrong place.
- The filename `test-driven-development.md` is unusable for the same reason.
- A FLAT sibling whose name merely starts with the same letters is fine: `references/tdd-failure-modes.md` contains `references/tdd-` and never `references/tdd/`. Verified green.

Check the `stale_references` list BEFORE choosing a layout or a filename for anything under `references/`, not after the validator goes red. Discovered while landing #4502 / PR #4503, where it decided one merged flat file over a `tdd/` subdirectory.

Other live entries that constrain naming the same way: `references/caveman.md`, `references/ponytail.md`, `playbooks/README.md`, `.agents/routing-bridges.txt`, `references/shaft-mastery.md`.