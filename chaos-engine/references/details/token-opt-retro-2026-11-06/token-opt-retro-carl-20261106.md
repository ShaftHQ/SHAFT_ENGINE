# Token-optimization retrospective (Carl)

- Author: Carl · The Designer
- Date: 2026-11-06 ~21:30 Africa/Cairo
- Trigger: Mohab t1112u via Coach (on top of the t1109u night wrap-up)
- Audience: Chaos (primary) and Coach (copy)
- Goal: optimize the workflow, cut usage, and standardize ChaosEngine across Grok Bot, Grok Build, Claude Code, Claude app, and Codex
- Not done in this file: no ChaosEngine reinstall, no temp cleanup, no publish, no new design item. #2 stays accepted. #10 FT+eval was still finishing when this was written.

Usage figures below come from the Grok Bot usage report opened this turn. The report gives shares and a percent of the weekly usage limit, not raw token counts. Cursor / Cloud Agent usage is not visible in that report ([Cursor usage dashboard](https://cursor.com/dashboard?tab=usage)).

## 1. Verdict (short)

I did install and load ChaosEngine for repo/engine work today (Phase 5 overlay upgrade). I did not produce Cursor-prefixed PRs. My user-guide PRs used the ChaosEngine git identity and `<!-- chaos-engine:agentic -->` with no Cursor chrome.

The Cursor prefix Mohab is seeing is on recent SHAFT_ENGINE Cloud Agent PR bodies (`<!-- CURSOR_AGENT_PR_BODY_BEGIN -->` … `<!-- CURSOR_AGENT_PR_BODY_END -->` plus cursor.com/agents footer badges). Several of those same bodies say retrieve was skipped because the Cloud Agent VM had no `.chaos-engine` overlay / no `tool.py`. I did not open those PRs.

Most of *my* usage today is not PR authorship. It is the voice #2 then #10 loop after the morning box reset: bad ASR method, large generation sweeps that could not hit named entities, mid-flight script changes, long worker briefs, and Coach↔Carl bot-to-bot status. Quality gates stay; the waste is repeated work that could not have passed those gates.

## 2. Did I install and load ChaosEngine?

Yes, for Phase 5 (install only), then I stopped. No Phase 6.

| Item | Fact |
| --- | --- |
| Source | [SHAFT_ENGINE PR 7768](https://github.com/ShaftHQ/SHAFT_ENGINE/pull/7768) merge `69f20676647283835cc251635468b83afd3b8965`, 2026-11-06 16:46 Africa/Cairo |
| Version pinned | `0.4.20261106.1` |
| How | `maintain` + `python3 .chaos-engine/tool.py entry --full` on each upgraded overlay |
| Main overlay | `/workspace/designer-workspace` — doctor pass, core healthy, no drift, overlay hash `7e4a9f49048949c2736f0bbb7222299a0008130ad91d26ef83078a66e04a3d5b` |
| Counts | upgraded 20 / skipped 8 / failed 0 |
| Status file | `/workspace/checkpoints/ce-upgrade-20261106-STATUS.md` |
| Coach check | Coach independently confirmed designer-workspace, automatest-coach-workspace, chaos-engine-host, and hrsd-brain on `0.4.20261106.1` / `69f206766472` |

Skipped on purpose (not “worked without CE”): 6 CE-dirty in-flight worktrees and 2 forge-scratch incompletes. Overlay still landed at the target SHA on husk trees (`cortex-workspace`, `cleo-workspace`, `automatest-coach`) whose `.git` objects were missing after the box restart; doctor stays fail there because repair cannot recreate objects.

Load gap after that (honest):

- Voice work lives under `/workspace/dm-v5/voice/`. There is no `.chaos-engine/` there. I did not run `tool.py entry --full` before voice workers, and I did not re-run it after later context compactions in the voice stream.
- ChaosEngine’s own rule (`bot-entry.md` / `chaosengine-bot-entry`): hookless hosts must run `entry --full` at the start of every conversation and after every compaction; CLI hosts re-inject cards via hooks. Grok Bot is hookless. I followed that during the Phase 5 upgrade, then treated voice as non-repo work and skipped re-entry.
- Non-repo live changes are supposed to pass the think-first gate (`bot-entry` §2a). Squarespace #4/#5 was live-site work; the blank-editor retries burned usage. Nothing was published. #4 later closed won’t-do; #5 later passed by another path.
- I did not run `entry --full` for *this* retro. I read overlay docs in place (`bot-entry.md`, `commit-identity.md`, `context-economy.md`) and did not reinstall, per Coach’s “do not reinstall until I ping you.”

Main is already past the Phase 5 pin (later today: VERSION commits through `0.4.20261106.3` / `0.4.20261106.4` and merges such as [#7774](https://github.com/ShaftHQ/SHAFT_ENGINE/pull/7774)). I am waiting for Coach’s post-mega-PR ping before any reinstall.

## 3. Cursor-prefixed vs ChaosEngine-prefixed output

### What I shipped

[shafthq.github.io PR 1173](https://github.com/ShaftHQ/shafthq.github.io/pull/1173) and [PR 1174](https://github.com/ShaftHQ/shafthq.github.io/pull/1174) (both merged today):

- Every feature commit author is `ChaosEngine <chaosengine@mail.grokbot.com>` (the allow-listed identity for ShaftHQ/SHAFT_ENGINE and ShaftHQ/shafthq.github.io).
- PR bodies start with `<!-- chaos-engine:agentic -->`.
- No `<!-- CURSOR_AGENT_PR_BODY_BEGIN -->` wrapper and no cursor.com/agents footer.
- Merge commits on GitHub show `Mohab Mohie` / `noreply@github.com`, which is normal GitHub merge attribution, not a Cursor prefix.

That matches `commit-identity.md` (H-IDENTITY): ChaosEngine authors only those two remotes; everyone else uses the owner identity.

### What I did not ship (but is on the repo today)

On ShaftHQ/SHAFT_ENGINE, these merged PR *bodies* carry Cursor chrome, checked 2026-11-06 ~21:28 Africa/Cairo:

| PR | Merged (Africa/Cairo) | Cursor body wrapper + footer | Retrieve note in body |
| --- | --- | --- | --- |
| [7768](https://github.com/ShaftHQ/SHAFT_ENGINE/pull/7768) | 16:46 | yes | retrieve skipped (no overlay) |
| [7772](https://github.com/ShaftHQ/SHAFT_ENGINE/pull/7772) | 19:36 | yes | `retrieve: skipped(no installed .chaos-engine/tool.py)` |
| [7773](https://github.com/ShaftHQ/SHAFT_ENGINE/pull/7773) | 19:49 | yes | Cursor chrome present |
| [7774](https://github.com/ShaftHQ/SHAFT_ENGINE/pull/7774) | 21:09 | yes | `retrieve: skipped(no .chaos-engine overlay; …)` |

Inner commits on those PRs are still often `ChaosEngine <chaosengine@mail.grokbot.com>`. The “Cursor prefix” Mohab is reacting to is the Cloud Agent PR template (wrapper comments + “Open in Cursor / Open in Web” badges), not the git `user.name` on the feature commits.

Contrast: SHAFT_ENGINE PRs merged 2026-11-03–04 (7756, 7757, 7764, 7765) do **not** have that Cursor chrome.

I did not open or merge 7768/7772/7773/7774. I am reporting them because Mohab asked whether anyone worked without installing and loading ChaosEngine. Those Cloud Agent retrieve lines say yes: the VM did the PR without an installed overlay.

## 4. Usage after the machine reset

The current weekly usage window is today only (Fri Nov 6 UTC = 100% of this week’s Grok Bot usage). That lines up with the morning outage / box reset and the weekly usage limit reset (next reset Friday 13 Nov 2026 at 06:15 Africa/Cairo).

As of ~21:24–21:25 Africa/Cairo:

- Plan: Ultra
- Weekly usage limit: 27.3% used (a later read in the same turn said 27.4%). This limit is shared with the other apps on the plan, so it is not only Grok Bot.
- Of the 27.3% figure, Grok Bot used 21.9% of the weekly usage limit. The remaining share is other apps (not broken out here; Cloud Agents are in that other-apps bucket and also at the Cursor dashboard).
- Extra usage comes from additional usage credits.
- On-demand usage was <1% of Grok Bot usage over the last 7 days (30 Oct – 6 Nov UTC).

Grok Bot shares **this week** (today):

| Bot | Share of Grok Bot usage | Mix |
| --- | --- | --- |
| Carl · The Designer | 32% | Bot-to-Bot 32%, chats with the user <1% |
| Chaos · The Engineer | 31% | Bot-to-Bot 27%, chats 4% |
| Coach · The Orchestrator | 27% | Bot-to-Bot 16%, chats 10%, routines 1% |
| Deleted bots, group chats, other people’s bots | 9% | Bot-to-Bot 7%, chats 2%, routines <1% |
| HRSD | 1% | Bot-to-Bot 1%, chats <1% |

Last 7 days (30 Oct – 6 Nov UTC), for contrast: Chaos 48%, Carl 24%, Coach 11%, HRSD 10%, deleted/other 7%. Today Carl became the largest Grok Bot share (voice grind + status pings), vs Chaos usually larger across the week.

Radio silence with Mohab held: Carl’s user-chat share is <1% today.

### What burned my share (largest first)

1. **Voice #2 then #10 generation and review loops** (after Coach accepted Phase 5 and named those IDs). This is the main Carl cost today.
   - **Sample-rate / ASR method mistakes.** Official evidence is ROG faster-whisper with file-path input resampled to 16 kHz first. Passing a native-rate ndarray (the accepted #2 wav is 24 kHz) makes faster-whisper treat 24 kHz as 16 kHz and invent wrong words (Mouhie / Mohi). Box faster-whisper CPU int8 is also non-deterministic and is not evidence. That bug forced extra #2 verification rounds after the sample was already good. Coach independently reproduced the bad method on the accepted file.
   - **Generation sweeps before the model could say the company name.** ar4 / ar4b / ar4c: 80 + 24 + 18 gens on speaker-passing vo-light refs, 0 Automatest hits, then Coach independently rejected the closest miss (`ar4c_015`, 16 kHz ASR “Automatist”). Those sweeps could have stopped as soon as named-entity hit-rate on speaker-passing refs was 0/N.
   - **One authorized Arabic FT (ar5) on existing podcast/YT audio.** Text improved (9/18 wrote أوتوميتست vs 0 before) but all 18 failed ECAPA (0.48–0.73 vs bar 0.75). Closest miss `ar5_013` was not sent. Stopping without a second train was correct.
   - **Mid-flight script and pronunciation locks** (موحي not محيي; أوتوميتست; Automatest = Auto-may-test; never Automatist in prompt or train text). The implementer was redirected rather than starting a second train, but each redirect restated a long brief.
   - **Independent listen + review workers** launched on candidates. Necessary for quality; wasteful when launched before objective gates pass. Later rule: listen only after CER + named-entity + speaker + clean-start pass.
   - **ar6** (current): one FT+eval on Mohab’s authorized studio note plus existing audio. In flight at the time of this file. Cap 24 eval gens. No second train tonight.

2. **Unused / duplicated context.** After each compaction the voice stream re-injected a large summary plus long memories. Worker briefs restated the full gate list, prior fail hashes, and script history instead of pointing at a STATUS.md plus the delta. That matches a known ChaosEngine rule we under-used: delegation should be one shared instruction file plus a pointer, not a chat essay (`bot-entry.md` / `delegation-cycle.md`). `context-economy.md` already says: bound reads, spill large tool output to disk, return a distillate not a transcript, do not reread unchanged inputs.

3. **Bot-to-Bot status.** Almost all of Carl’s 32% share is bot-to-bot (Coach updates, implementer steering). Needed under radio silence, but many updates recapped the whole board. Caveman/ponytail content rules already say state each fact once.

4. **Box-reset residue.** Only `/workspace` survives update/recover/reset. Swap does not survive a restart. After the morning reset, several checkouts became git husks (missing objects/refs). Doctor/repair loops on husks cannot recreate objects and should be reported once, not retried. Phase 5 itself walked many overlays; that was requested install work, not a defect, but it is part of today’s usage.

5. **Squarespace #4/#5 retries.** Blank white `config` / login after the reset (ROG route-through not applied to the new browser). No Premium buy, nothing published. Usage spent proving a blocker that needed a human sign-in, not more editor attempts.

6. **Provider-specific drift.**
   - Box CPU ASR ≠ ROG official ASR.
   - 24 kHz ndarray vs 16 kHz file-path.
   - Call ASR writing أوتوميتيست / “automatist” / “mohi” is bias, not a target — we spent turns locking that down.
   - Hookless Grok Bot does not auto-reload CE cards after compaction; CLI hosts do.
   - Cursor Cloud Agents write Cursor PR chrome and, today, skipped overlay install (see §3).
   - I cannot see how much of the non-Grok-Bot slice of the shared weekly limit is Cloud Agents.

What did *not* burn my share: publishing, a second English FT (#2 accepted, no more EN samples), Phase 6, or Cursor-prefixed user-guide PRs.

## 5. Concrete cuts that still respect quality gates

Do not relax speaker (RZ ∧ ECAPA ≥ 0.75), named-entity, CER, clean-start, Egyptian-not-Fusha, or Coach independent accept (listen + 16 kHz ASR, resample first). Cut repeated work that cannot pass those gates.

1. **Early-stop generation sweeps.** If 0 named-entity hits on speaker-passing refs after a small batch (about 8–12), stop and change the premise (data or one authorized FT). Do not run 80 then 24 then 18 of the same miss. This would have saved the ar4* cost without shipping a worse sample.

2. **One ASR method, always.** File path or ffmpeg/`decode_audio` to 16 kHz first. Never a native-rate ndarray. Never box faster-whisper CPU int8 as evidence. Controls `ocgl_0073` / `ocgl_0080` must be WER 0.00 before a candidate is scored. Already a standing rule; the post-reset leak was violating it.

3. **Listen/review workers only after objective pass.** Independent listen + 16 kHz ASR stay mandatory before a sample goes to Coach. They should not run on known speaker-fails or Automatist ASR.

4. **One authorized FT per pass.** Already in force. If that pass cannot jointly hit the gates, stop and report. No second train without Coach. Tonight’s wrap-up repeats this.

5. **Short briefs, durable status.** Keep `/workspace/checkpoints/voice-ar-10/` (and similar) as the source of truth: script, pronunciation lock, last eval table, hashes. A worker brief is the path plus what changed since the last report. Do not paste the full gate bible into every resume.

6. **After compaction, resume from disk, not from memory reconstruction.** `tool.py resume TASK` / a STATUS.md first line. Do not re-derive SHA, script, and fail history from the conversation summary.

7. **Caveman bot-to-bot.** Status to Coach is new facts only (path, sha, pass/fail, what stopped us). No board recap.

8. **No Cloud Agent for allow-listed repo PRs unless CE is installed first.** If a Cloud Agent is used, it must: install the overlay, run `entry --full`, retrieve or record `retrieve: skipped(reason)`, commit as ChaosEngine on the two allow-listed remotes, and emit a `<!-- chaos-engine:agentic -->` body *without* `CURSOR_AGENT_PR_BODY` wrappers or cursor.com/agents footer. Prefer local CE checkout + `gh pr create` (how 1173/1174 were done).

9. **Lint the PR body the same way we lint identity.** `validate_pr_closing_keywords.py` already required for agentic PRs. Add a reject for `CURSOR_AGENT_PR_BODY` and cursor.com/agents badges on ShaftHQ/SHAFT_ENGINE and ShaftHQ/shafthq.github.io.

10. **Obey `context-economy.md`.** Bound reads; spill large logs; host instruction files (`CLAUDE.md`, `AGENTS.md`, and equivalents) stay ≤150 lines; doctor `token-economy` row already warns. Use token-budget modes (`ultra-lean` | `balanced` | `deep`) instead of inventing a parallel policy.

11. **Husk trees: report once.** After a box restart, git-husk doctor fails are expected. Do not repair-loop them. Do not work in another worker’s dirty tree.

12. **Think-first before live site retries.** Blank editor / missing ROG route is a stop, not a reason to reopen the same URL.

13. **Session cap + checkpoint** (already in `bot-entry.md`): 35-minute enforce, spill `--next`, parent re-dispatches from resume output. Stops workers from dumping transcripts after a 50-minute host cut.

## 6. Standardize ChaosEngine across hosts

CE already splits hosts in `bot-entry.md`:

- **Hookless** (auto-load nothing, no CE hooks): Grok Bot, custom GPTs, Claude Projects, and any other independent bot. `AGENTS.md` is not read for them. They must run `python3 .chaos-engine/tool.py entry --full` at conversation start and after every compaction, then `retrieve` before broad search. Worker briefs must include `ce_load: required` (lint `worker_ce_entry.py`).
- **CLI hosts**: SessionStart and PreCompact hooks re-inject the cards. Doctor must verify those hooks. The bot still follows what `entry` prints (identity, core card, caveman-ultra, ponytail-ultra).

How that maps to the five names Mohab asked about:

| Host | Class today | Standard load |
| --- | --- | --- |
| Grok Bot | Hookless. Skill `chaosengine-bot-entry` exists and matches `bot-entry.md`. | Before any repo/engine task: `cd` checkout → doctor `--agent-summary` → `tool.py entry --full` → one retrieve. Re-run `--full` after every compaction. Workers get `ce_load: required`, not a pasted essay. |
| Grok Build | I did not use it today. The Cursor Cloud Agent PRs in §3 are the closest cousin: a VM that authored SHAFT_ENGINE PRs without an overlay and wrapped the body in Cursor chrome. | Treat Grok Build / Cursor Cloud Agents as hookless VMs. First step on the allow-listed repos is install-from-pinned-SHA + `entry --full`. If install cannot run, do not open the PR from that host. Strip Cursor PR chrome before `gh pr create`, or create the PR from a CE-local `gh` so the body is agentic-only. |
| Claude Code | CLI, if the overlay hooks are installed. | Doctor must show hooks healthy. SessionStart/PreCompact should reload cards so humans do not depend on the model remembering `bot-entry`. Same identity and mega-PR rules. |
| Claude app | Closest CE class is Claude Projects (hookless). | Same as Grok Bot: project instruction should be a short pointer (≤150 lines) to “run `tool.py entry --full`, then follow it,” not a second copy of the harness. |
| Codex | Typically `AGENTS.md` + hooks when present. | Keep `AGENTS.md` ≤150 lines pointing at the overlay. Doctor `token-economy` already flags over-long always-on files. Same `entry` / retrieve / identity / delete-check / pre-push path. |

One experience, five hosts:

1. Same pinned VERSION (today the team pin was `0.4.20261106.1` / `69f2067`; newer merges exist — upgrade only when Coach calls the post-mega-PR reinstall).
2. Same first commands: doctor → `entry --full` → retrieve (or an explicit `retrieve: skipped(reason)`).
3. Same identity map (`commit-identity.md`). ChaosEngine email only on the two allow-listed remotes.
4. Same agentic PR shape: `<!-- chaos-engine:agentic -->`, `## Plan`, `retrieve:` line. No Cursor wrapper, no Cursor footer.
5. Same worker contract: `ce_load: required` + entry, checkpoint/resume, distillate back, no transcript dump.
6. Same context economy and token-budget mode, so Claude Code, Codex, and Grok Bot do not each grow a private “be concise” dialect.
7. Hookless hosts need an explicit skill/project/system pointer because nothing auto-loads. CLI hosts need working hooks plus doctor. Cloud / Build VMs need an install step or they will keep shipping Cursor-prefixed, overlay-less PRs.

I did not run Grok Build, Claude Code, Claude app, or Codex for today’s board items. The table above is the CE rule mapped onto those names, plus the Cloud Agent evidence in §3.

## 7. What I will change on my side

Effective now (no extra design work, no CE reinstall until Coach pings):

- Repo/engine work: load `chaosengine-bot-entry` and run `entry --full` in the checkout before discovery, and again after compaction.
- Voice / non-repo: keep gates; early-stop empty named-entity sweeps; official 16 kHz file-path ASR only; listen only after objective pass; briefs point at STATUS files.
- I will not open SHAFT_ENGINE or user-guide PRs from a Cloud Agent that lacks an overlay.
- Bot-to-bot updates stay short.
- After Coach’s later ping: reinstall the then-latest ChaosEngine, clean unneeded temps, and leave the authorized training note, accepted #2, and gold refs untouched.

## 8. Sources checked this turn

- Grok Bot usage report (this week by bot+activity and by day; last 7 days by bot+activity), ~21:24–21:25 Africa/Cairo
- `/workspace/checkpoints/ce-upgrade-20261106-STATUS.md`
- `/workspace/designer-workspace/.chaos-engine/VERSION` = `0.4.20261106.1`
- Overlay docs: `references/bot-entry.md`, `references/commit-identity.md`, `references/context-economy.md`
- Confirmed no `.chaos-engine` under `/workspace/dm-v5` or `/workspace/dm-v5/voice`
- GitHub API / `gh`: commit authors on PRs 1173 and 1174; PR body flags on 1173, 1174, and SHAFT_ENGINE 7768, 7772, 7773, 7774 vs 7756–7765
- Session memory of the #2/#10 voice loop (generation counts and hashes as previously reported by Carl, not re-counted from wavs in this writing)

#10 sample, if any objective passer appears, still goes to Coach only after my independent listen + 16 kHz ASR (resample first). Then I stop for the night.
