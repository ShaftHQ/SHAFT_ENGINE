# Coach token-optimization retrospective — 2026-11-06

- Author: Coach · The Orchestrator
- Date: 2026-11-06, Africa/Cairo
- Trigger: Mohab t1112u
- Companion files expected: `token-opt-retro-carl-20261106.md`, Chaos’s own retrospective
- Status: Coach file ready to cluster into one PR

## Main goal

Optimize the workflow, minimize token and usage cost, and standardize the ChaosEngine experience across providers (Grok Bot, Grok Build, Claude Code, Claude app, and Codex are examples). Git identity on SHAFT_ENGINE work should read as ChaosEngine, not as the host provider.

## Direct answers

### Why do recent PRs use a Cursor prefix?

Independently checked on `ShaftHQ/SHAFT_ENGINE`:

| Merged (UTC) | PR | Branch | First lines of body |
|---|---|---|---|
| 2026-11-06 19:09 | [#7774](https://github.com/ShaftHQ/SHAFT_ENGINE/pull/7774) | `cursor/installer-phase0-7771-69db` | `<!-- CURSOR_AGENT_PR_BODY_BEGIN -->` then `<!-- chaos-engine:agentic -->` |
| 2026-11-06 17:49 | [#7773](https://github.com/ShaftHQ/SHAFT_ENGINE/pull/7773) | `cursor/bigger-picture-first-f559` | same Cursor wrapper |
| 2026-11-06 17:36 | [#7772](https://github.com/ShaftHQ/SHAFT_ENGINE/pull/7772) | `cursor/allure-31-flutter-java25-6b1d` | same Cursor wrapper |
| 2026-11-06 14:46 | [#7768](https://github.com/ShaftHQ/SHAFT_ENGINE/pull/7768) | `cursor/ce-lessons-mega-20261106-f84b` | same Cursor wrapper |

Each of those PR bodies also ends with a Cursor cloud-agent footer (`CURSOR_AGENT_PR_BODY_END` plus `cursor.com/agents/bc-…` links).

The previous convention on this repo was a `ChaosEngine/` branch prefix, for example:

- [#7756](https://github.com/ShaftHQ/SHAFT_ENGINE/pull/7756) `ChaosEngine/cleanup-delivery-enforcement` (merged 2026-11-03)
- [#7737](https://github.com/ShaftHQ/SHAFT_ENGINE/pull/7737) `ChaosEngine/mega3-7547-7736`
- [#7735](https://github.com/ShaftHQ/SHAFT_ENGINE/pull/7735) `ChaosEngine/mega-cleanup-20261102b`
- [#7682](https://github.com/ShaftHQ/SHAFT_ENGINE/pull/7682) `ChaosEngine/ce-mega-2026.10.31.12`

The `cursor/…` prefix and `CURSOR_AGENT_PR_BODY_*` wrapper are the default brand of Cursor cloud agents, not ChaosEngine. Commit titles can still say `feat(chaos-engine): …` while the branch and PR chrome say Cursor.

### Is anyone working without installing and loading ChaosEngine?

Yes, on the Cursor cloud-agent PRs above. Their own PR bodies say retrieve was skipped because the overlay was missing:

- #7774: `retrieve: skipped(no .chaos-engine overlay; path-scoped reads of bootstrap.py and installer tests)`
- #7772: `retrieve: skipped(no installed .chaos-engine/tool.py)`
- #7768: `retrieve: skipped(no installed .chaos-engine/tool.py; verification was git grep of origin/main at 5c0ce78f plus issue text)`
- #7773: `retrieve: exempt(harness)` (harness work; still shipped as a `cursor/…` branch with the Cursor wrapper)

So ChaosEngine metadata comments were pasted into the body (`chaos-engine:agentic`), but the worker did not install or load the overlay before doing the work. That is exactly “working without ChaosEngine.”

Coach’s own lapse tonight: inventory of those PRs was done with `gh` without first loading `chaosengine-bot-entry`. That was read-only and did not edit the repo, but it still skipped the required entry path. Orchestration and voice review are not a `.chaos-engine` checkout; installer/repo work by cloud agents is.

The shared box overlay at `/workspace/chaos-engine-host/.chaos-engine` is currently `0.4.20261106.4` (same version as main after #7774). Cloud agents run on a separate VM and do not automatically get that overlay.

### Why does usage jump after a machine reset?

Two different “resets” stacked today:

1. **Weekly usage window.** The current weekly usage limit period includes Friday 6 Nov 2026 as its first day (it resets Friday 13 Nov 2026 at 06:15 Africa/Cairo). All of this week’s Grok Bot shares below are therefore today’s work, which makes the day look unusually expensive.
2. **Bot computer reset.** After Update / recover / reset, only `/workspace` survives. Tools, model caches, swap, and per-home installs come back empty. Agents re-read large context, re-download models, re-probe the environment, and cloud agents start on a clean VM with no ChaosEngine overlay. That repeats paid work that an installed overlay would have short-circuited (retrieve, receipts, fail-closed entry, branch naming, PR template).

Grok Bot usage report (this week, as of the check on 6 Nov 2026 evening Cairo):

- Weekly usage limit: 27.3% used. This limit is shared with the other apps on the plan, not only Grok Bot.
- Of that 27.3%, Grok Bot used 21.9%.
- Extra usage comes from additional usage credits (see Settings → Usage & Billing).
- Shares of all Grok Bot usage this week: Carl · The Designer 32%, Chaos · The Engineer 32%, Coach · The Orchestrator 26%, deleted bots / group chats / other people’s bots 9%, HRSD 1%. All of those shares are dated Friday 6 Nov.
- This report cannot see Cursor cloud-agent usage on its own. That belongs on https://cursor.com/dashboard?tab=usage. Because the last four SHAFT_ENGINE merges were cloud-agent PRs, a large part of the “after reset” spend is likely there, not only in Grok Bot chats.

## Coach workflow waste tonight (concrete)

These are things Coach did that burned usage without moving the main goal:

1. **Repeated ASR on the same training note.** The 73.62s file at `/workspace/dm-v5/voice/training/mohab-20261106-note1.m4a` was transcribed more than once (first pass interrupted, full Arabic-autodetect pass, then a multilingual pass plus five sliced passes). One planned 16 kHz pass, then slices only if language mix required it, would have been enough.
2. **Wrong first volume read.** `astats` with `reset=1` reported a trailing-window peak of about -31 dBFS. A later `volumedetect` pass showed max -2.6 dB and mean -25.8 dB. One correct meter up front would have avoided the correction loop.
3. **Silent hidden-wake turns that still cost a full model pass.** Teammate acknowledgements were correctly not sent to Mohab, but each one still woke Coach and rebuilt this long context.
4. **Long sticky context.** This chat carries board history, voice gates, Slido, and SHAFT merges. Every teammate FYI reloads that. Status should be a short board note, not another full-context turn.
5. **Cloud-agent PRs accepted without an overlay gate.** Coach independently verified merges (#7772, #7773, #7774) and treated them as done without treating `retrieve: skipped(no overlay)` as a process defect. That let the Cursor prefix and the missing install become the path of record.

## What should change (token optimization + standardization)

### Fail closed: no repo work without ChaosEngine

- Any worker (Grok Bot, Grok Build, Claude Code, Claude app, Codex, or a Cursor cloud agent) that edits `ShaftHQ/SHAFT_ENGINE` or another checkout with a ChaosEngine overlay must install the overlay and load the entry card (`chaosengine-bot-entry` / `bot-entry.md`) before the first search or edit.
- `retrieve: skipped(no .chaos-engine overlay)` and `retrieve: skipped(no installed .chaos-engine/tool.py)` must be a hard failure, not a note in the PR body. The worker installs, then retrieves, then edits.
- Cloud-agent launch text must include: install latest ChaosEngine overlay on the VM, load entry, step back, retrieve, then work. Do not pass a “path-scoped reads, skip retrieve” escape hatch.

### Standardize git chrome to ChaosEngine, not the host provider

- Branch prefix: `ChaosEngine/<short-slug>` (restore the convention used through #7756). Do not open SHAFT_ENGINE work on `cursor/…`.
- PR body: start with ChaosEngine markers (`chaos-engine:agentic`, scope, retrieve receipt). Do not wrap the body in `CURSOR_AGENT_PR_BODY_BEGIN/END`.
- Strip or replace provider footers (`cursor.com/agents/…` chips) so the public PR reads as ChaosEngine work.
- Commit and PR titles may keep `feat(chaos-engine):` / `fix(ce):` as now; the missing piece is the branch name and the body wrapper.
- The same prefix and body template must be used on every provider. Provider adapters (Grok, Claude, Codex, Cursor) are allowed to translate tools; they are not allowed to rename the branch or rebrand the PR.

### Cut usage after a machine reset

- After any bot-computer reset, the first action for repo workers is overlay reinstall from main, then verify `VERSION`, then start work. Do not launch a cloud agent “to save time” on a VM that has no overlay.
- Keep reusable installs and model caches under `/workspace` when the product allows it, because only that tree survives reset.
- Do not re-download or re-transcribe artifacts that already exist under `/workspace` (training wavs, accepted #2, gate reports).
- Prefer one in-flight SHAFT_ENGINE change set (tonight: one mega PR) instead of serial cloud agents that each rebuild context.
- Coach should not wake on teammate acknowledgements that need no decision. Informational status can wait for a real result (sample ready, PR merged, blocker).

### Cross-provider sameness

The user-facing experience should be identical on Grok Bot, Grok Build, Claude Code, Claude app, and Codex:

1. Install / detect overlay.
2. Bigger-picture step-back (already added in #7773).
3. Retrieve companions.
4. Work inside the receipt.
5. Open the PR as `ChaosEngine/<slug>` with the ChaosEngine body template.
6. Independent review, then merge.

If a provider cannot install the overlay, it must stop and say so rather than skip retrieve and keep going.

## Out of scope tonight

- Not reopening #7774.
- Not starting installer Phases 1–5, SeleniumHQ/selenium#18254, or other ShaftHQ Dependabot queues.
- Not publishing Digital Mohab. #2 stays accepted. #10 sample review continues when Carl sends the file.
- Cleanup of leftover `/workspace/tmp-*`, extra `ce-src-*`, and stale worktrees waits for the agreed night close-out so we do not delete gold training audio or the accepted English sample.

## Requested PR contents

Cluster this file with Carl’s and Chaos’s retrospectives. Land any harness changes the retrospectives require so that:

1. Overlay install is mandatory before retrieve on every provider.
2. New branches use `ChaosEngine/` not `cursor/`.
3. PR bodies use the ChaosEngine template without a provider wrapper.

Merge that work to main after Chaos’s dual review.
