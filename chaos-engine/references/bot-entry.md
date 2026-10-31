# Bot entry

Entry point for agents that auto-load nothing from the checkout and run no
ChaosEngine hooks: Grok Bot, custom GPTs (OpenAI GPTs, Assistants), Claude
Projects, and any other independent bot. `AGENTS.md` is not read for you.

## No `.chaos-engine/` yet

Install first: [harness maintenance](harness-maintenance.md#install).
From an engine source checkout the installer is `chaos-engine/install.py`
(with `--source <repo>/chaos-engine --commit <40-hex-sha>`). After install,
doctor and `tool.py` use `.chaos-engine/install.py`. Tee install/doctor logs to
`.chaos-engine-state/logs/` via `session_logs.log_path`, never workspace-root `*.log` files.

## Every task, before discovery

Worker briefs: `ce_load: required` + `tool.py entry` (lint `worker_ce_entry.py`).

1. With a shell: run `python3 .chaos-engine/tool.py entry` and follow what it
   prints. It bundles `identity.md`, the core card
   (`skills/chaos-engine/SKILL.md`), and the ethics card, and both companion cards
   (`companions/caveman-ultra.md`, `companions/ponytail-ultra.md`).
   Without a shell: read those five files directly.
   `checkpoint`/`resume` refuse once the cards are 40 min old (rerun
   `entry --full`); write `reload harness first` in summaries and checkpoints.
2. Run one `python3 .chaos-engine/tool.py retrieve --store graphify|mempalace "<q>"`
   before the first broad search or unnamed read
   ([retrieve-first](retrieve-first.md)).
3. Record `retrieve: used|skipped(<reason>)|exempt(harness)` in the
   [research receipt](research-receipt.md).
   Agentic PRs carry `<!-- chaos-engine:agentic -->` (or the `agentic` label);
   CI then requires `## Plan` and `retrieve:` lines. Human PRs are exempt.
4. Before any delete, `tool.py delete-check <paths>`; refusal: owner code
   only ([discipline](delete-check-discipline.md)).
   Lesson: backup dir = push.

Re-runs print one line while the harness is unchanged; `--full` reprints the
cards. Run `--full` at the start of every conversation and after every
context summary or compaction: CLI hosts re-inject the cards on SessionStart
and PreCompact, so a bot must do it itself. One delivery per thread; at each boundary pass
[pre-done](../skills/pre-done-self-review/SKILL.md), then a handoff note
(state, PRs, next step), not a transcript.

## Keep going

Keep-alive: [worker-keep-alive](worker-keep-alive.md) (KEEP-ALIVE charter).
Upgrade: [ce-upgrade-on-release](ce-upgrade-on-release.md) (see [VERSION](../VERSION)) — install latest
ChaosEngine (`chaos-engine/VERSION`), load it for every task; workers inherit
via `ce_load` + entry. Cleanup: [cleanup-at-close](cleanup-at-close.md).
Work order: [work-ordering](work-ordering.md) (open PRs first).
Commit identity: [commit-identity](commit-identity.md, pr-scope-check.md) (ChaosEngine author only on the engine repo + user-guide pages repo).

Work the queue to the end without waiting for "proceed": after each step or
delivery, start the next queued item (KEEP-ALIVE). A turn that ends while CI
runs must leave a wake (a PR-scoped CI listener) whose prompt resumes the
queue; a host whose scheduled runs cannot create listeners hands that to the
parent. While waiting on review, pull the next independent own row.

## Session cap (Grok Bot default)

Grok Bot stops a background session at about 50 minutes of wall clock; pings
do not extend it. The one session-length rule, enforced at 35 minutes:

- Start with `python3 .chaos-engine/tool.py resume TASK` and
  `tool.py checkpoint TASK STEP --start`; follow `Resume at:`, not the transcript.
- After every build, test, commit, push, PR or merge step, run
  `tool.py checkpoint TASK STEP --branch B --pr N --next "..."`. It warns in
  the last 5 minutes and exits 3 past 35: push WIP, record `--next`, end the turn.
- Parent: one issue per background run; split batches. On "interrupted
  before it finished", re-dispatch with `tool.py resume TASK` as the brief.
  Never poll a child to keep it alive. Interrupt → new worker from [checkpoint-resume](checkpoint-resume.md).


## Radio silence, progress, infra, VERSION

1. **Radio silence:** when `CHAOS_RADIO_SILENCE` is armed (or
   `.chaos-engine-state/radio-silence` exists), do not SendToUser for routine
   status — orchestrator only ([radio-silence](radio-silence.md); [`radio_silence.py`](../radio_silence.py)).
2. **Progress log:** after every step append to the task progress log
   ([progress-log](progress-log.md); [`progress_log.py`](../progress_log.py)) and
   `tool.py checkpoint`.
3. **Infra preflight:** if `box_disk_guard.blocked_infra_reasons` is non-empty
   (disk, swap-missing, apt-mirror-unreachable), set **BLOCKED-INFRA** and do
   not start heavy workers.
4. **VERSION bump:** consumer-facing `chaos-engine/` changes require
   `chaos-engine/VERSION` bump (overlay pre-push) unless explicitly waived.
5. **Moving-channel pins:** [moving-channel-pin](moving-channel-pin.md) ([`moving_channel_pin.py`](../moving_channel_pin.py)) —
   minimum-within-major for floating vendor channels.
6. **Ownership + cleanup:** record created paths
   (`scripts/ci/worker_ownership.py`); cleanup-at-close deletes only manifest
   entries ([cleanup-at-close](cleanup-at-close.md)).
7. **Doc-claim review:** [doc-claim-review](doc-claim-review.md) on every gate PR.
8. **Fail-closed gates:** [gate-fail-closed](gate-fail-closed.md).

## Token economy

- One round trip per PR: `overlay_pre_push.py` also checks the open PR body
  and the README inventories, so fix what it reports before pushing, never after.
  Before `gh pr create`, validate the body file:
  `PR_TITLE="$T" PR_BODY="$(cat body.md)" python3 scripts/ci/validate_pr_closing_keywords.py` (repo-only).
  SHA-pin every new `uses:` and run changed contract tests with `python3 -I`
  (stdlib only, as on the runner). Never pipe pre-push output
  (a pipe masks its failure; guard R32). Add the
  release-note label at create time (REST labels API when `gh pr edit` errors).
- Filter test and CI logs to the verdict:
  `2>&1 | grep -E "^(OK|FAILED|FAIL:|AssertionError)" | cut -c1-300`.
  Never print a whole job log or assertion dump. Keep a known-failures list
  instead of re-running the base commit to compare.
- Babysit with ONE background loop per PR, not repeated poll calls: it exits on
  merged, on the first `fail` (latest run per check name; a failed required
  check exits 1 at once), or when the PR is green but not queued (then it
  prints the `enqueuePullRequest` error, which names the blocker). Await it
  once. `gh pr merge --auto --merge` may print a merge-queue message yet still
  set auto-merge; confirm `autoMergeRequest`. Before it merges, read
  non-blocking analysis and coverage reporters and fix them.
- Bound each tool call to 60 s; run longer ones in the background.
- Trust a green gate: no local re-check after CI passes.
- Reports: deltas only (merged, next, blockers), <= 15 lines.
- One worktree per worker on a shared box: `git worktree add "$WT"
  origin/HEAD -b "$BRANCH"`; never branch, stage, or install in another
  worker's clone. Prefer a single `git clone --filter=blob:none` (or bare /
  `--reference`) host checkout and worktrees over N full clones.
  Empty `/workspace` or logged-out `gh` at start means the box is still
  mounting: recheck, never clone over it. Keep `graphify-out/` and
  `.chaos-engine-state/` untracked; do not rebuild a Graphify index per
  worktree on the same host when one shared index already exists.
- Disk gate before a new worktree or clone: warn under 15 GiB free, block
  under 10 GiB or at the linked-worktree cap (`box_disk_guard`); never add a
  full clone when a blobless primary already exists.
  Details: [parallel streams](parallel-streams.md).
- Keep tools and drafts outside `/tmp` (a box restore wipes it) and check
  `git ls-files -d` before each push.
- **box-durability** (default entry/worker checklist):
  1. Manual swap does not survive a restart. At entry after any restart, run
     `free -h`; if swap is `0`, re-add it (`fallocate -l 8G /swapfile`,
     `mkswap /swapfile`, `swapon /swapfile`, `sysctl vm.swappiness=10`).
  2. Only `/workspace` survives Update Bot's Computer / recover / reset. Keep
     all in-progress files, checkpoints, installs, and tool caches under
     `/workspace` (never rely on `$HOME` alone for work product).
  3. Update Bot's Computer runs only when no bot is busy — go idle when asked
     so the update can proceed.
  4. A `.git` directory can become a **husk** after restart (missing `objects/`
     or `refs/` dirs, or a born HEAD whose objects are gone). Doctor reports
     `git-husk` as a **warn** (hard error only when the secret guard cannot run).
     A valid empty repo (`git init`, unborn HEAD, `objects/`+`refs/` present) is
     **not** a husk. Recovery: remote repos → re-clone or fetch; local-only →
     `git init` if needed plus one baseline commit of explicit paths (never
     `git add -A`). ChaosEngine install does not create `.git` in non-git dirs.
  5. **Scripts and commits:** shell scripts that mutate state use `set -e` (or
     `set -euo pipefail`). Stage **explicit paths** only — never
     `git commit -a` / `git add -A`. If a pre-commit / overlay pre-push diff is
     far larger than the planned change, stop and unstage; do not force the
     oversized commit through.
  6. **Freshly generated assets** (renders, encodes, contact sheets): checkpoint
     or commit them in the same turn they appear (L54).


## Delivery ledger

Several tasks, issues, or PRs: first record each with `tool.py checkpoint
TASK --item ID --group G --status todo`, show the table, and update it at
each milestone (`pr #N`, `merged`, `blocked: reason`). `resume` prints it.
Every task: [flow](iterative-flow.md), [PDCA](multi-agent-pdca.md); WIP limit 1
per stream: babysit PR to merged, then next in that stream.
Need parallel on the **same** files? Stack PRs on the previous branch.
File-disjoint streams: [parallel streams](parallel-streams.md) (ownership
manifest, conflict check, disk gate, no force-push under review).

## After each delivery

After every final delivery (owner approval, completed publish, or final
merge), DoD in order: [post-merge own cleanup](parallel-streams.md#post-merge-own-cleanup)
(mandatory; hygiene proofs; own items only), disk/cache inventory via
`box_disk_guard`, Learning Session per the
[router contract](router-contract.md#learning-session), then
`python3 .chaos-engine/tool.py maintain` before the next task.

Lock held, harness edits, or an older overlay:
[harness maintenance](harness-maintenance.md#maintain).

## Host setup

Make the entry automatic, or serve bots without repository access (GPTs):
[harness maintenance](harness-maintenance.md#bot-host-setup).
