# ICM Architect

Installer-owned advisory companion (#6001). Upstream:
[RinDig/icm-architect](https://github.com/RinDig/icm-architect).

## Role

Design any process, idea, or problem into an **ICM workspace** (folder structure
as agent architecture), or restructure an existing folder into one. Two modes:
**Build** and **Restructure**. Validate with the walk test.

## ChaosEngine integration

- Vendored pin: `chaos-engine/vendor/icm-architect/` (PIN.json + nested skill)
- Published plugin: `plugins/icm-architect/` via `hosts.rematerialize_companions`
- Bundle default-on; disable with `--without-icm-architect`
- Doctor component: `companion-icm-architect`
- SessionStart: **advisory** locator only (not an intensity companion)
- Use with [design-loop](design-loop.md) when the work is structural layout;
  intensity companions remain Caveman + Ponytail on implement paths

## Triggers

"ICM this", "structure this for agents", "build me a workspace", "map this repo",
"audit this folder", folder-as-architecture, process pipeline scaffolding.
