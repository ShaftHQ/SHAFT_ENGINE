# Bot entry

Entry point for agents that auto-load nothing from the checkout and run no
ChaosEngine hooks: Grok Bot, custom GPTs (OpenAI GPTs, Assistants), Claude
Projects, and any other independent bot. `AGENTS.md` is not read for you.

## No `.chaos-engine/` yet

- Fresh clone: install from a copy of its own `chaos-engine/` kept outside
  the project (source and project must be disjoint), pinned to HEAD:
  `cp -r chaos-engine ../ce-src && python3 chaos-engine/install.py install --project . --source ../ce-src --commit "$(git rev-parse HEAD)"` (repo-only: run in the source checkout)
  (add `--with-<add-on>` flags as needed; `install.py addons --project .`
  lists them). Expect a few minutes for the first store index.
- Linked worktree: install in the primary checkout, then
  `python3 <primary>/.chaos-engine/worktree_overlay.py materialize --primary <primary> --session <worktree>`.

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

- One round trip per PR: `overlay_pre_push.py` also checks the open PR body
  and the README inventories, so fix what it reports before pushing, never after.
- Filter test and CI logs to the verdict:
  `2>&1 | grep -E "^(OK|FAILED|FAIL:|AssertionError)" | cut -c1-300`.
  Never print a whole job log or assertion dump.
- Babysit with one blocking loop that exits on merge or on the first `fail`.
  With a merge queue, `gh pr merge --auto --merge` enqueues the PR. Before
  it merges, read non-blocking analysis and coverage reporters and fix them.
- `checkpoint TASK --budget 40` after every merge. When it exits 3, stop.

## Delivery ledger

Several tasks, issues, or PRs: first record each with `tool.py checkpoint
TASK --item ID --group G --status todo`, show the table, and update it at
each milestone (`pr #N`, `merged`, `blocked: reason`). `resume` prints it.
WIP limit: open PR 1, babysit it to merged, then start the next item.
Need parallel? Stack PRs on the previous branch.

## After each delivery

Learning Session after every final delivery: when the owner approves a
deliverable, a publish completes, or the final PR merges, the bot runs it
itself, at once, without being asked. CLI hosts' Stop hook enforces the same
rule. Follow the [router contract](router-contract.md#learning-session): diff
the session lessons against the skills, file one spec issue, open ONE PR
(`skip-release-notes`, auto-merge MERGE), and babysit it to merged; nothing new
is one report line. Then run
`python3 .chaos-engine/tool.py maintain` to fast-forward, reinstall, and
re-check doctor before the next task.
An interrupted `maintain` keeps running and holds `.chaos-engine.lock`; a
second one fails with "another ChaosEngine operation is already running".
Check the holder PID it names and `python3 .chaos-engine/install.py status --project . --json`
before rerunning; a healthy core at the new commit means it already finished.
The local pre-push hook does not run the Agent Guidance Gate. Before pushing a
reference or skill edit in the ChaosEngine source repository, run (repo-only)
`python3 scripts/ci/harness_pr_gate.py --base "$(git merge-base HEAD origin/HEAD)" --head "$(git rev-parse HEAD)"` (repo-only;
issue tags and bare angle-bracket placeholders in references fail it).
Read every `status=failed` line of its output, not only the tail. Delete
`.pytest_cache/` first; it and materialized overlay files fail two contracts
only locally.
An older overlay can record a git-digest source without a repository; its
`maintain` then stops at "repository must be an explicit GitHub
owner/repository" after the fast-forward. Reinstall once with
`python3 .chaos-engine/bootstrap.py --project . --repository OWNER/REPO --branch BRANCH --distribution portable`;
newer overlays recover on their own.

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
