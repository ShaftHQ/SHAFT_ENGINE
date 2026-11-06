# Token-optimization retrospective — Chaos · The Engineer

Date: 2026-11-06, about 21:25 Africa/Cairo
Author: Chaos · The Engineer
Prompt: Mohab t1112u via Coach (token burn after machine reset; PRs showing a Cursor prefix instead of ChaosEngine; standardize ChaosEngine across providers)

## Verdict

Recent SHAFT_ENGINE pull requests carry Cursor chrome because I handed implementation to Cursor cloud agents, and I did not require those agents to install and load the ChaosEngine overlay before they touched the repo. They still wrote `<!-- chaos-engine:agentic -->` inside the Cursor wrapper, but they skipped the overlay. That is visible in their own `retrieve:` lines. The Cursor branch prefix, `CURSOR_AGENT_PR_BODY` markers, and `cursor.com/agents/...` footer are injected by the Cursor pull-request tool, not by ChaosEngine.

That skip also wastes tokens: a cloud agent that never installs ChaosEngine never gets retrieve-first scoping or the entry card, so it rereads more of the tree and the parent has to restate rules in every prompt. Machine reset makes the same mistake at box scale: only `/workspace` survives, the overlay has to be reinstalled, and everyone rediscovers context.

The fix is not a one-off footer edit on an old PR. Every provider (Grok Bot, Grok Build, Claude Code, the Claude app, Codex, and Cursor cloud agents) must start the same way: install or doctor ChaosEngine, run `tool.py entry`, retrieve, then work. After a Cursor-hosted agent opens a pull request, the parent must rewrite the body so the public prefix is ChaosEngine, not Cursor.

This retrospective should land in a dedicated standardization pull request, not inside the in-flight #28 Dependabot/queue-policy mega PR. Mixing those goals would hide both.

## Why SHAFT_ENGINE pull requests show Cursor markers

I independently read the merged pull-request bodies on 2026-11-06 at about 21:24 Africa/Cairo. All four of today's ChaosEngine/SHAFT implementation pull requests share the same pattern.

| PR | Branch | Cursor wrapper | ChaosEngine marker | `retrieve:` line | Agent footer |
|---|---|---|---|---|---|
| [7768](https://github.com/ShaftHQ/SHAFT_ENGINE/pull/7768) | `cursor/ce-lessons-mega-20261106-f84b` | `CURSOR_AGENT_PR_BODY_BEGIN` / `_END` | `<!-- chaos-engine:agentic -->` | (present later in body; agent [bc-c3df3275](https://cursor.com/agents/bc-c3df3275-0744-5621-82b5-8195e9cff84b)) | `cursor.com/agents/bc-c3df3275-...` |
| [7772](https://github.com/ShaftHQ/SHAFT_ENGINE/pull/7772) | `cursor/allure-31-flutter-java25-6b1d` | same | same | `retrieve: skipped(no installed .chaos-engine/tool.py)` | `cursor.com/agents/bc-9e162c36-...` |
| [7773](https://github.com/ShaftHQ/SHAFT_ENGINE/pull/7773) | `cursor/bigger-picture-first-f559` | same | same | `retrieve: exempt(harness)` | `cursor.com/agents/bc-2e741cd1-...` |
| [7774](https://github.com/ShaftHQ/SHAFT_ENGINE/pull/7774) | `cursor/installer-phase0-7771-69db` | same | same | `retrieve: skipped(no .chaos-engine overlay; path-scoped reads of bootstrap.py and installer tests)` | `cursor.com/agents/bc-e840b818-...` |

The public author is `MohabMohie` because the cloud agent acts as the signed-in user. The branch prefix `cursor/` and the HTML "Open in Web / Open in Cursor" footer with `cursor_ref=pr_footer` are Cursor platform output. ChaosEngine's own `bot-entry` (overlay copy at `/workspace/chaos-engine-host/.chaos-engine/references/bot-entry.md`, inspected the same evening) does not mention `CURSOR`, `cursor.com`, or a vendor prefix at all, so nothing in the harness currently strips or replaces that chrome.

I asked those agents for ChaosEngine pull-request conventions (`<!-- chaos-engine:agentic -->`, a `## Plan` section, and a `retrieve:` line). They complied, and those comments sit *inside* `<!-- CURSOR_AGENT_PR_BODY_BEGIN --> ... <!-- CURSOR_AGENT_PR_BODY_END -->`. What Mohab sees first is still Cursor.

I did not rewrite the bodies with `gh pr edit` after the agents opened the pull requests. That is on me.

## Did cloud agents or workers skip installing and loading ChaosEngine?

Yes, at least the Cursor cloud agents that opened #7772 and #7774 said so themselves.

- #7774: `retrieve: skipped(no .chaos-engine overlay; path-scoped reads of bootstrap.py and installer tests)`
- #7772: `retrieve: skipped(no installed .chaos-engine/tool.py)`
- #7773 used `retrieve: exempt(harness)` because it was editing the harness. That exemption can be valid, but the pull request still used a Cursor branch name and Cursor body wrapper, so it was not a full ChaosEngine-hosted session either.

What I loaded versus what they loaded:

- On the box I do install and load the overlay at `/workspace/chaos-engine-host/.chaos-engine` (most recently `0.4.20261106.4` from merge `f4d7bd83`) and I ran `tool.py entry --full` before later operational writes.
- Grok Bot's `code-changes` skill requires repository implementation to go to a Cursor cloud agent rather than a local checkout. Those agents run on a fresh Cursor VM. They see the `chaos-engine/` *source* tree inside SHAFT_ENGINE, not the box overlay.
- My launch prompts asked for agentic markers, VERSION rules, and preflight, but they did not make this the first required step: `python3 chaos-engine/install.py ...` (or doctor/repair) and then `python3 .chaos-engine/tool.py entry --full`.
- So the workers that actually wrote the commits skipped ChaosEngine install/load. The parent bot loaded it; the implementers did not.

That is a yes to Mohab's question: people (cloud agents) were working without installing and loading ChaosEngine.

Local executor reviewers on the box are a mixed picture: they can use the host overlay when I point them at it, but several review worktrees were throwaway checkouts whose job was "read this diff and write a review file." Those reviewers were not a substitute for the implementer loading the harness.

## Token burn: machine reset and today's workflow

Two separate burns showed up today. They compound.

### After a machine reset

Shared standing fact (support ticket T-I84315): only `/workspace` survives update, recover, or reset. Swap does not survive a restart. The ChaosEngine overlay, caches, and "we already know this" session state on the box go away unless they were kept under `/workspace`.

After a reset the expensive loop is: reinstall ChaosEngine, rerun doctor, reload every skill and companion card, rediscover the board and the day's pull requests, and retell workers rules that the overlay would have printed for free. That is token burn caused by lost durable context, not by the size of a single useful diff.

### During normal work today

#7774 needed five implementation heads and five dual-review pairs because I started adversarial review before the required checks were green (complexity at `2f42e473`, then Agent Guidance Gate at `b6b834e6`). Each new head correctly got a fresh reviewer pair, but several of those pairs reviewed work that CI was about to reject. That is avoidable token cost.

Cursor cloud agents also start cold every launch. With no overlay they skip retrieve-first, so they spend tokens on broader file reads (`#7774` said "path-scoped reads" after admitting there was no overlay). I then restated ChaosEngine rules in long launch prompts because I could not trust the agent to load them. Prompt duplication is a tax on not installing the harness.

This conversation itself compacted more than once. Compaction plus `entry --full` is correct for a hookless bot, but it is another reminder that long parent threads are expensive compared with a worker that loads the overlay and a short outcome-only brief.

## Bigger picture

The local hotfix (strip the footer on one pull request, or add another sentence to a launch prompt) would miss the actual contract:

ChaosEngine is supposed to be the provider-neutral session. Grok Bot, Grok Build, Claude Code, the Claude app, Codex, and Cursor cloud agents should feel the same: same entry, same retrieve-first, same delete-guard, same pull-request shape, same ChaosEngine attribution. The host vendor may run the process, but it should not brand the deliverable.

Today the host that writes SHAFT_ENGINE code is Cursor cloud agents (because that is what Grok Bot's repository-work skill mandates). If we do not bootstrap ChaosEngine *inside that host*, we get Cursor-shaped pull requests and we pay for a second copy of the rules in every prompt.

## What I will change

1. **Bootstrap is mandatory for every implementer, including cloud agents.** The first steps on a SHAFT_ENGINE (or any `.chaos-engine` / `chaos-engine/`) checkout are: install or doctor the overlay from `chaos-engine/` source at the current commit, run `tool.py entry --full`, then one retrieve. `retrieve: skipped(no .chaos-engine overlay)` and `retrieve: skipped(no installed .chaos-engine/tool.py)` are defects, not valid skips. Install first, then retrieve.

2. **Launch prompts will stop being a substitute for the overlay.** I will point workers at the overlay (or tell them to install it) and give a short outcome, constraints, and Definition of Done. I will not paste the harness into the prompt.

3. **Public pull-request prefix is ChaosEngine.** Immediately after a cloud agent opens a pull request I will rewrite the body: keep `<!-- chaos-engine:agentic -->`, `## Plan`, and `retrieve:`; remove `CURSOR_AGENT_PR_BODY_BEGIN` / `_END` and the `cursor.com/agents/...` footer. Branch names should use a `chaosengine/` prefix when the tool lets us choose; when Cursor still creates `cursor/...`, the body and title must still read as ChaosEngine work. I failed to do this rewrite on #7768, #7772, #7773, and #7774.

4. **Encode the above in the harness**, so it does not depend on me remembering a prompt. `bot-entry` and the Grok Bot `chaosengine-bot-entry` skill should say: vendor PR chrome is stripped; overlay install is required before retrieve; a missing overlay is not an exemption. That harness edit should be its own careful change (the always-loaded cards are already near grok-bot and copilot byte ceilings; #7773 documented this). I am not expanding tonight's retro cluster into that card edit.

5. **Spend review tokens only on a mergeable head.** Dual independent adversarial review stays required. It starts after the required checks are green on that head, not before. Follow-up reviews look at the delta from the last reviewed head, not the whole tree again, unless the change is structural.

6. **Survive reset without rediscovering the world.** Keep the overlay, status files, and in-progress artifacts under `/workspace`. After any reset: reinstall from the last known main commit with the sanctioned installer, run doctor, read today's status file, and continue. Do not rebuild the board from memory or from a full repository tour.

7. **One implementer per workstream.** Reply to the existing cloud agent for must-fixes; do not launch a second agent on the same pull request. I already do this; it stays the rule.

8. **Provider-neutral session start.** The same bootstrap (install/doctor, `tool.py entry`, retrieve, ChaosEngine pull-request shape) is what Grok Bot, Grok Build, Claude Code, the Claude app, and Codex should run. The retrospective cluster is the written contract. The later harness edit in item 4 is how it becomes enforceable.

## Tonight versus later

Already in flight, separate goal: the #28 mega PR folds Dependabot #7759–#7763, #7769, and #7770 and lands the fail-closed queue-policy fix. I will not hide this retrospective inside that pull request.

Tonight, once Coach and Carl send their files: cluster the three retrospectives in one dedicated pull request, dual-review it, and merge it. That pull request should not also implement installer Phases 1–5, touch SeleniumHQ/selenium#18254, or sweep other ShaftHQ Dependabot queues.

The harness enforcement in item 4 is planned work described here. It is not silently folded into #28, and it is not a new issue invented beyond t1112u's standardization ask; it is the change I will make so every provider uses the ChaosEngine prefix and workflow.

## Evidence sources

- `gh pr view` of ShaftHQ/SHAFT_ENGINE #7768, #7772, #7773, and #7774 on 2026-11-06 at about 21:24 Africa/Cairo (title, `headRefName`, full body).
- Overlay `chaos-engine/references/bot-entry.md` on `/workspace/chaos-engine-host` (no Cursor/prefix language; VERSION `0.4.20261106.4`).
- First-hand launch practice: Grok Bot `code-changes` routes repository writes to Cursor cloud agents; my prompts required agentic markers but not overlay install.
- Shared durability note: after reset, only `/workspace` survives (T-I84315).

retrieve: skipped(named `gh pr view` of four pull requests and a named bot-entry read; no broad repository search)
