# SHAFT feature video — production brief (phase 2 input)

Source of truth: `scripts/make_boards.py` (one table → `boards/*.json` + `scripts/*.script.json`). Edit there, regenerate, re-run gates.
Spec: `spec/SPEC.md` · Facts + sources: `research/RESEARCH.md` · Issues: epic #6647, exec #6657, tech #6658, publish #6659.

## Cuts
| Cut | Audience | Board | Est. runtime | One message |
|---|---|---|---|---|
| exec | business decision makers | boards/exec.json | ~96 s (16:9) | One open engine replaces the test plumbing every team rebuilds, and adopting it is low risk. |
| exec-vertical | LinkedIn/mobile feed | boards/exec-vertical.json | ~42 s (9:16) | same; hook, data, one engine, report, safe upgrade, CTA |
| tech | technical implementers | boards/tech.json | ~3:36 (16:9, YouTube chapters) | Keep Selenium, Appium and REST Assured; drop the plumbing; upgrade in place with a rollback guarantee. |

Runtimes are estimates (150 wpm + gaps + visual pads); final timing comes from the real line WAVs (precheck/*/lines.json).

## Exec arc (D21)
hook x01 "Red build. Real bug, or flaky test?" → problem x02 (16% Google data-viz) x03 (same plumbing) → solution x04 one engine, x05 built on standards, x06 waits, x07 Allure, x08 one setting → proof x09 safe upgrade, x10 AI optional, x11 MIT/2018/216/62 → CTA x12 generator + "pilot the upgrade on one repository this sprint".

## Tech chapters (YouTube)
0:00 Hook (3-line SHAFT test) · Why (plain Selenium plumbing) · One API · Reliability · Evidence · Scale · Migrate · Agentic, optional · Start. Chapter timestamps are written from the final edit list, not from this estimate.

## Production rules carried from the cards
- Real captures only for product UI (D11): generator project + `mvn test` + Allure report; sample Selenium project + upgrader terminal; trace viewer. No mock UIs.
- Code on screen ≥40 px JetBrains Mono, ≤8 lines per card; every snippet compiles against engine 10.3.20260930.
- Third-party names as text wordmarks only (no logos); no download counts, customer logos, or speed claims.
- Refresh P9 numbers (releases, contributors) from the GitHub API on render day; update text + say together.
- Synthetic-voice disclosure on the end card and in descriptions; burned-in captions on the 9:16 cut; sidecar captions on 16:9.
- Optional P14 insert (tech t02): show plain-vs-SHAFT line counts only if measured on real compiling code.

## Gates (all must pass before upload)
Script: ttslint (+phonemes), vowords/tts with folds on the real VO. Board: brief, holds, describe, arc. Render: flatframes, contenthold, claims --video, edgeclip (9:16), staletext/psparse on captures, captions, loudness (-16 LUFS, TP ≤ -1), colortags, levels, flash, delivery, blackfreeze, silence.
