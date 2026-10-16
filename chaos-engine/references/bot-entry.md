# Bot entry

Entry point for agents that auto-load nothing from the checkout and run no
ChaosEngine hooks: Grok Bot, custom GPTs (OpenAI GPTs, Assistants), Claude
Projects, and any other independent bot. `AGENTS.md` is not read for you.

## No `.chaos-engine/` yet

Install first: [harness maintenance](harness-maintenance.md#install).

## Every task, before discovery

1. With a shell: run `python3 .chaos-engine/tool.py entry` and follow what it
   prints. It bundles `identity.md`, the core card
   (`skills/chaos-engine/SKILL.md`), and both companion cards
   (`companions/caveman-ultra.md`, `companions/ponytail-ultra.md`).
   Without a shell: read those four files directly.
2. Run one `python3 .chaos-engine/tool.py retrieve --store graphify|mempalace "<q>"`
   before the first broad search or unnamed read
   ([retrieve-first](retrieve-first.md)).
3. Record `retrieve: used|skipped(<reason>)|exempt(harness)` in the
   [research receipt](research-receipt.md).

Re-runs print one line while the harness is unchanged; `--full` reprints the
cards. Run `--full` at the start of every conversation and after every
context summary or compaction: CLI hosts re-inject the cards on SessionStart
and PreCompact, so a bot must do it itself. Keep one delivery per thread and write a handoff note (state, open PRs,
next step) at each delivery boundary instead of carrying a long transcript.

## Keep going

Work the queue to the end without waiting for "proceed": after each step or
delivery, start the next queued item. A turn that ends while CI runs must leave
a wake (a PR-scoped CI listener) whose prompt resumes the queue; a host whose
scheduled runs cannot create listeners hands that to the parent.

## Session cap (Grok Bot default)

Grok Bot stops a background session at about 50 minutes of wall-clock time.
Pings do not extend a wall-clock cap; resume instead:

- Start each session with `python3 .chaos-engine/tool.py resume TASK` and
  `tool.py checkpoint TASK STEP --start`; follow its `next:` line, not the transcript.
- Keep each work unit under 40 minutes. After each step (commit, push, PR,
  merge) run `tool.py checkpoint TASK STEP --branch B --pr N --next "..."`.
- When `tool.py checkpoint TASK --budget` exits 3, push WIP, record `--next`,
  and end the turn.
- Parent: on "interrupted before it finished", re-dispatch with the
  `tool.py resume TASK` output as the brief. Never poll a child to keep it alive.

## Token economy

Measured on 20 CI PRs: polling, preventable CI round trips and
verbose reports were most of the waste.

- One round trip per PR: `overlay_pre_push.py` also checks the open PR body
  and the README inventories, so fix what it reports before pushing, never after.
  Before `gh pr create`, validate the body file:
  `PR_TITLE="$T" PR_BODY="$(cat body.md)" python3 scripts/ci/validate_pr_closing_keywords.py` (repo-only).
  SHA-pin every new `uses:` and run changed contract tests with `python3 -I`
  (stdlib only, as on the runner). Run pre-push under `set -o pipefail`;
  never pipe it to `tail` before `&&`. Add the release-note label at create
  time (REST labels API when `gh pr edit` errors).
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
- Bound every tool call to 60 s or less; anything longer runs in the background.
- Trust a green gate: no local re-verification after CI passes.
- Reports are deltas only (merged, next, blockers), at most 15 lines.
- Keep tools and drafts outside `/tmp` (a box restore wipes it) and check
  `git ls-files -d` before each push.
- `checkpoint TASK --budget 40` after every merge. When it exits 3, stop.

## Delivery ledger

Several tasks, issues, or PRs: first record each with `tool.py checkpoint
TASK --item ID --group G --status todo`, show the table, and update it at
each milestone (`pr #N`, `merged`, `blocked: reason`). `resume` prints it.
WIP limit: open PR 1, babysit it to merged, then start the next item.
Need parallel? Stack PRs on the previous branch.

## After each delivery

Learning Session after every final delivery (owner approval, completed
publish, or final merge): run it yourself at once, per the
[router contract](router-contract.md#learning-session). Then run
`python3 .chaos-engine/tool.py maintain` before the next task.

Lock held, harness edits, or an older overlay:
[harness maintenance](harness-maintenance.md#maintain).

## Make it automatic

A bot only follows this page when something loads it every task. Put one
always-loaded instruction in the host's persistent slot (Grok Bot shared user
memory or a global skill, GPT instructions, Claude Project instructions):
"In a checkout with `.chaos-engine/`, run `python3 .chaos-engine/tool.py entry`
first and follow `.chaos-engine/references/bot-entry.md`." New agents on that
host then inherit the entry, retrieve-first, and receipts with no per-agent
setup. Keep the rules here; the host slot holds only the pointer.

## Bots without repository access (GPTs)

Save `python3 .chaos-engine/tool.py entry > chaos-engine-entry.md` and load
that file as the bot's instructions or knowledge. Regenerate it after every
ChaosEngine update; a stale copy is a stale harness.

## Chat form

Hosts whose chat demands full sentences keep them; caveman's content rules
still apply: no filler, preamble, or narration, and each fact stated once.
