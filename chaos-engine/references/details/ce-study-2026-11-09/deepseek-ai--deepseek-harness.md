# deepseek-ai/deepseek-harness

- URL: https://github.com/deepseek-ai/deepseek-harness
- Lists: Charlie Hills snapshot, 6 Nov 2026, row 5 (274K stars on that snapshot).
- GitHub description: DeepSeek Harness: Everything is a Plugin.
- README: README headings seen: developer preview, run from npm or from source, community, contributing, development. The lead calls the architecture everything-is-a-plugin.

## Reusable patterns

- Capabilities are plugins with a contract, not a growing core script.
- The README separates a preview from a supported run path.

## Adopt

- ChaosEngine already records host mechanisms in harness_index.py. Keep new surfaces on that index. Do not adopt a second plugin runtime.

## Skip

- Do not vendor DeepSeek Harness or its plugin runtime.
- Do not retitle ChaosEngine skills as plugins.

## License check

- Checked 2026-11-10. GraphQL licenseInfo.spdxId is MIT. gh api repos/deepseek-ai/deepseek-harness/license returned HTTP 200 and path LICENSE.
- No skill edit from this note.

Adopt the idea. Do not copy third-party code. Do not vendor the repository.
