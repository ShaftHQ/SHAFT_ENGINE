# Bot entry

Entry point for agents that auto-load nothing from the checkout and run no
ChaosEngine hooks: Grok Bot, custom GPTs (OpenAI GPTs, Assistants), Claude
Projects, and any other independent bot. `AGENTS.md` is not read for you.

## No `.chaos-engine/` yet

Install first: [harness maintenance](harness-maintenance.md#install).

## Every task, before discovery

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

Work the queue to the end without waiting for "proceed": after each step or
delivery, start the next queued item. A turn that ends while CI runs must leave
a wake (a PR-scoped CI listener) whose prompt resumes the queue; a host whose
scheduled runs cannot create listeners hands that to the parent.

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
  Never poll a child to keep it alive.

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
- Disk gate before a new worktree or clone (executable
  `scripts/ci/box_disk_guard.py`): **warn** under 15 GiB free, **block** under
  10 GiB free; also block when linked worktrees hit the cap. Never add a full
  clone when a blobless primary for that repo already exists. Cache caps use
  tool-native prune only. Details: [parallel streams](parallel-streams.md).
- Keep tools and drafts outside `/tmp` (a box restore wipes it) and check
  `git ls-files -d` before each push.

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
merge), complete this **Definition of Done** in order:

1. **Post-merge own cleanup** (mandatory): when a stream PR is MERGED and the
   head is verified on the remote, remove **only** that stream's recorded
   worktree / local branch / remote head via
   `scripts/ci/worktree_hygiene.py` proofs (`--cleanup-task-worktree`,
   `--cleanup-task-local-branch`, `--cleanup-task-remote-head`,
   `--prune-after-task-cleanup`). Own items only; dirty, unpushed, foreign,
   and never-touch paths refuse. See
   [post-merge own cleanup](parallel-streams.md#post-merge-own-cleanup).
2. Inventory disk (`box_disk_guard.disk_level`) and over-cap caches; prune with
   tool-native commands only when safe.
3. Learning Session at once, per the
   [router contract](router-contract.md#learning-session).
4. Then `python3 .chaos-engine/tool.py maintain` before the next task.

Lock held, harness edits, or an older overlay:
[harness maintenance](harness-maintenance.md#maintain).

## Host setup

Make the entry automatic, or serve bots without repository access (GPTs):
[harness maintenance](harness-maintenance.md#bot-host-setup).
