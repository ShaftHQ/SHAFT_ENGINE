# Third-party notices

This portable tree is MIT licensed. See [LICENSE](LICENSE). The notices below
cover bundled companions and patterns reimplemented from published sources.
No third-party runtime was copied into this tree.

## Caveman

- License: MIT
- Copyright (c) 2026 Julius Brussee
- Upstream: https://github.com/JuliusBrussee/caveman
- Local pin: [vendor/caveman/PIN.json](vendor/caveman/PIN.json)
- Local license: [vendor/caveman/LICENSE](vendor/caveman/LICENSE)

Only the MIT skill and hook files listed in that pin are vendored. Upstream
Engine-linked directories licensed under Business Source License 1.1 are not
included here.

## Ponytail

- License: MIT
- Copyright (c) 2026 DietrichGebert
- Upstream: https://github.com/DietrichGebert/ponytail
- Local pin: [vendor/ponytail/PIN.json](vendor/ponytail/PIN.json)
- Local license: [vendor/ponytail/LICENSE](vendor/ponytail/LICENSE)

Skill and hook bodies in that pin are verbatim upstream.

## Test-driven development adaptation

See [references/test-driven-development.LICENSE](references/test-driven-development.LICENSE).

## DeepSeek Harness patterns

- License: MIT
- Copyright (c) 2026 DeepSeek
- Upstream license: https://github.com/deepseek-ai/deepseek-harness/blob/master/LICENSE
- Architecture reviewed at commit `47f943859bef60e4160492346772ded9b24f765a`

Capability seams, orthogonal outcomes, tool-result pruning, spill, and code
mode were reimplemented as portable guidance and installer contracts. The
Node runtime, Cordis kernel, session log, agent loop, and UI were not copied.
