# Proposals from the 2026-11-09 study

Twenty-nine repositories, one note each. No thirtieth repository. No third-party code was copied. No harness was vendored. No skill body was added or edited from these notes. `job-applications` is the separate #7868 slice, not a study extraction.

## Landed here

| Change | Path |
| --- | --- |
| Continuous-improvement principle | `chaos-engine/identity.md` (Principles only; Truth block unchanged) |
| Published Grok identity | `.grok/rules/identity.md` via `chaos-engine/grok_model_load.py` |
| Monthly cadence | `chaos-engine/references/details/ce-study-2026-11-09/cadence.md` |
| Notes and this list | `chaos-engine/references/details/ce-study-2026-11-09/` |

## Follow-up issues

These four are new products. They are not skill edits in this delivery.

| Product | Study note | Issue |
| --- | --- | --- |
| Voice cloning | [debpalash--VoiceStudio.md](debpalash--VoiceStudio.md) | https://github.com/ShaftHQ/SHAFT_ENGINE/issues/7881 |
| Security-audit campaign | [cloudflare--security-audit-skill.md](cloudflare--security-audit-skill.md) | https://github.com/ShaftHQ/SHAFT_ENGINE/issues/7882 |
| Game studio | [Donchitos--Claude-Code-Game-Studios.md](Donchitos--Claude-Code-Game-Studios.md) | https://github.com/ShaftHQ/SHAFT_ENGINE/issues/7883 |
| Spatial globe | [bilawalsidhu--gods-eye-view.md](bilawalsidhu--gods-eye-view.md) | https://github.com/ShaftHQ/SHAFT_ENGINE/issues/7884 |

#7882 is limited to repositories we own. It is not an offensive audit of anyone else's system. #7881 does not vendor the AGPL-3.0 VoiceStudio tree. #7884 does not treat the `NOASSERTION` license file as a cleared SPDX id.

## Ideas already covered, so no skill edit

| Idea | Existing path | Why it stays |
| --- | --- | --- |
| Terseness and smallest diff | `chaos-engine/companions/ponytail-ultra.md` | Ponytail is already vendored and pinned. No second copy. |
| Local knowledge graph | Graphify retrieve | Already the retrieve path. No vector store and no second engine. |
| Spec-driven sections | `chaos-engine/references/work-item.md` | Sections are already required. spec-kit is not vendored. |
| Plan, test, review, finish | `chaos-engine/skills/chaos-engine/SKILL.md` | superpowers confirms the order. It does not replace the card. |
| Multi-host harness and plugin contracts | `chaos-engine/harness_index.py` | ECC and DeepSeek Harness are not a second harness. |
| Think first, surgical change | `chaos-engine/identity.md` | The Karpathy-guidelines repository has no license file, so a skill edit is blocked anyway. |
| Local coding agent | `chaos-engine/skills/local-agency/SKILL.md` | OpenCode stays an external program. |
| One agent per worktree | `chaos-engine/references/iterative-flow.md` | Orca's desktop, mobile, and remote fleet are not added. |
| Capture, compress, reinject | Memory, MemPalace, Graphify | claude-mem is not a second pipeline. |
| Design reference and anti-slop review | design add-ons and the UI delivery route | taste-skill, impeccable, and hallmark are not copied. #4048 already tracks taste-skill. |
| Ask before an irreversible action | identity and the ethics card | OpenMausBot's virtual machine is an extra machine and is not added. |

## Ideas not filed and not built

The owner named four follow-ups. These are recorded and left unfiled.

| Idea | Why it is not a skill now |
| --- | --- |
| Reverse engineering (`morluto/rea`) | New product. Not one of the four named follow-ups. |
| Semantic ontology (`NVIDIA/auto-ontology`) | New layer on top of Graphify, plus an NVIDIA runtime. Not vendored. |
| Web scrape and crawl (`D4Vinci/Scrapling`) | No scraper and no access-control bypass. |
| Open-web readers (`Panniantong/Agent-Reach`) | Preferred tool plus one fallback only. No platform clients. |
| Interactive or editorial diagrams (`tt-a1i/archify`, `cathrynlavery/diagram-design`) | Later diagram work can use the idea. No template copy. |
| HTML-to-MP4 (`heygen-com/hyperframes`) | Video stays in the design add-on. No renderer vendored. |
| Tutoring loop (`HKUDS/DeepTutor`) | New product. Not one of the four named follow-ups. |
| Mobile control surface (`pingdotgg/t3code`) | New product. Local-agency already delegates locally. |
| Astrology charts (`Renhuai123/ziwei-doushu`) | Out of core. Code is MIT and the dataset file says CC BY 4.0. Neither is copied. |

## License blocks

| Repository | Result on 2026-11-10 | Block |
| --- | --- | --- |
| `multica-ai/andrej-karpathy-skills` | No license file. License API HTTP 404. | Skill edit and vendoring blocked. |
| `debpalash/VoiceStudio` | AGPL-3.0. This repository is MIT. | Skill edit and vendoring blocked. Follow-up is #7881. |
| `bilawalsidhu/gods-eye-view` | SPDX `NOASSERTION`. LICENSE is an MIT grant for code only, plus non-MIT data. | Skill edit and vendoring blocked. Follow-up is #7884. |
