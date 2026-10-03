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


## ICM Architect

- License: MIT
- Copyright (c) 2026 Jake Van Clief
- Upstream: https://github.com/RinDig/icm-architect
- Local pin: [vendor/icm-architect/PIN.json](vendor/icm-architect/PIN.json)
- Local license: [vendor/icm-architect/LICENSE](vendor/icm-architect/LICENSE)

Skill, references, and templates in that pin are verbatim upstream, nested under
`skills/icm-architect/` for ChaosEngine plugin discovery.

## Task Observer methodology (self-improve skill)

- License: CC BY 4.0
- Copyright / credit: Eoghan Henn / rebelytics
- Upstream: https://github.com/rebelytics/one-skill-to-rule-them-all
- Local adaptation: [skills/self-improve/](skills/self-improve/) (lean CE-native rewrite; see UPSTREAM.md and references/research-adopt-reject.md)


## Test-driven development adaptation

See [references/test-driven-development.LICENSE](references/test-driven-development.LICENSE).

## deja-vu

- License: MIT
- Copyright (c) vshulcz and contributors
- Upstream: https://github.com/vshulcz/deja-vu
- Pinned release: v0.21.2 CLI archives only (per-platform sha256 in `dependencies.json`). Not vendored.
- ChaosEngine does not register its MCP server, hooks, or user-home skills, and does not run its upstream installer.

## DeepSeek Harness patterns

- License: MIT
- Copyright (c) 2026 DeepSeek
- Upstream license: https://github.com/deepseek-ai/deepseek-harness/blob/master/LICENSE
- Architecture reviewed at commit `47f943859bef60e4160492346772ded9b24f765a`

Capability seams, orthogonal outcomes, tool-result pruning, spill, and code
mode were reimplemented as portable guidance and installer contracts. The
Node runtime, Cordis kernel, session log, agent loop, and UI were not copied.

## Design skills add-on (optional)

The optional `addons/design-skills/` cards re-express, in their own words and
with attribution, rules from these published sources. No upstream file is
copied. Commits are pinned in `addons/design-skills/INVENTORY.md` (present
when the add-on is installed).

- Anthropic skills (frontend-design, canvas-design, algorithmic-art,
  brand-guidelines, theme-factory): Apache-2.0 (per-skill LICENSE.txt).
  https://github.com/anthropics/skills
- impeccable: Apache-2.0; its NOTICE terms apply.
  https://github.com/pbakaus/impeccable
- taste-skill: MIT. https://github.com/leonxlnx/taste-skill
- Web Interface Guidelines: MIT.
  https://github.com/vercel-labs/web-interface-guidelines
- HyperFrames: Apache-2.0. https://github.com/heygen-com/hyperframes
- claude-remotion-skill: MIT. https://github.com/haidrrrry/claude-remotion-skill
- ffmpeg-video-editor: MIT. https://github.com/bryanwhl/ffmpeg-video-editor
- video-editor: MIT. https://github.com/naveedharri/video-editor

Tools the cards call (VHS, Manim, Kokoro, whisper.cpp, DeepFilterNet,
Real-ESRGAN, VMAF, FFmpeg) are installed by the user and are not bundled.
