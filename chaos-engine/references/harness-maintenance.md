# Harness maintenance

Install, `maintain` recovery and harness-edit checks for bots that start from
[bot entry](bot-entry.md). Load only when one of these applies.

## Install

- Fresh clone: install from a copy of its own `chaos-engine/` kept outside
  the project (source and project must be disjoint), pinned to HEAD:
  `cp -r chaos-engine ../ce-src && python3 chaos-engine/install.py install --project . --source ../ce-src --commit "$(git rev-parse HEAD)"` (repo-only: run in the source checkout)
  (add `--with-<add-on>` flags as needed; `install.py addons --project .`
  lists them). Expect a few minutes for the first store index.
- Linked worktree: install in the primary checkout, then
  `python3 <primary>/.chaos-engine/worktree_overlay.py materialize --primary <primary> --session <worktree>`.
- A clean source can also come from `git archive HEAD | tar -x -C ../ce-src`
  (a disjoint tree at the same commit).
- Never reinstall after `entry`: the session's cards are bound to that
  install. Install once, before entry.
- `gh pr create` run from a linked worktree may not see the pushed upstream.
  Pass `--head BRANCH --base main`.

## Maintain

An install older than a repair component rejects it (`argument --component:
invalid choice`): reinstall from the checkout first (Install above), then run
the repair.


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

## Always-loaded budget

Bytes a bot host loads every task (`tool.py entry --full`, bot entry,
`AGENTS.md`) may grow at most 5% over
[always-loaded-budget.json](../always-loaded-budget.json); the harness gate
fails beyond that. Move new detail to an on-demand reference instead. When the
bundle shrinks by more than 10%, lower `bytes` to lock in the gain.

## Moving guidance text

Before moving, merging or deleting a guidance line, search `tests/scripts` for
its phrase. A pinned phrase stays where its test reads it, or the test moves
with it in the same commit. Specialist skills that load one per turn may repeat a
rule on purpose; link instead only within one skill.


## Harness file index

Source repo only: every harness file not on a task route is indexed in
[harness files](harness-files.md), enforced by
`tests/scripts/test_agent_entry_reachability.py`.

## Bot host setup

### Make it automatic

A bot only follows this page when something loads it every task. Put one
always-loaded instruction in the host's persistent slot (Grok Bot shared user
memory or a global skill, GPT instructions, Claude Project instructions):
"In a checkout with `.chaos-engine/`, run `python3 .chaos-engine/tool.py entry`
first and follow `.chaos-engine/references/bot-entry.md`." New agents on that
host then inherit the entry, retrieve-first, and receipts with no per-agent
setup. Keep the rules here; the host slot holds only the pointer.

### Bots without repository access (GPTs)

Save `python3 .chaos-engine/tool.py entry > chaos-engine-entry.md` and load
that file as the bot's instructions or knowledge. Regenerate it after every
ChaosEngine update; a stale copy is a stale harness.

## Token economy

`tool.py entry` ends with one receipt line
(`entry: caveman=… ponytail=… ethics=… persona=loaded|none index=N retrieve=…`), and
`install.py doctor --agent-summary` prints `token-economy: pass|fail`. Both come from
[token_economy.py](../token_economy.py); the lazy map is
[harness-map.md](harness-map.md). An add-on may name one `persona` file (≤ 2 KiB)
in `addon.json`; entry prints it and the receipt shows `persona=loaded`.
