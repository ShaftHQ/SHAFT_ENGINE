# Memory v5 schemas

These schemas are the canonical project-store schemas distributed by
`@aictx/memory@0.2.1`, which ChaosEngine pins in `dependencies.json`. They are
installed into adopter repositories because Memory validates canonical data
against project-local, tracked schemas.

The upstream package is MIT-licensed. Keep this directory synchronized with
the pinned package: update the dependency and all five schemas in one change,
then prove `memory status --json` and `memory check --json` in a fresh adopter
project.
