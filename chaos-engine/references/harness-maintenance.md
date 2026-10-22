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

## Deletes

A bot workspace with no pushed copy is one `rm` from total loss (about 85 GB lost
on a shared box, cause unknown). Never delete what is untracked,
dirty or unpushed; the [delete guard](delete-guard.md) refuses it, and doctor's
`backup-remote` row names workspaces to push or back up. Move, never remove,
when unsure.

## Harness edit pitfalls

- New or renamed core files: `python3 chaos-engine/harness_map.py --write` (repo-only)
  in the same commit, or doctor's `token-economy` row fails
  "harness map stale". A new core module must also be linked from a reference,
  or the entry-reachability check fails.
- A new `tests/scripts` module must be selected in `scripts/ci/harness_pr_gate.py`
  (repo-only) or the Agent Guidance Gate fails; prefer extending a gated module.
- Check the always-loaded and host-file budgets before designing always-on
  text; each new line usually needs an equal cut elsewhere.
- A real official bootstrap into an empty directory catches what source-tree
  tests miss: installs ship a subset of files, so doctor checks test
  completeness, not byte equality.
- The one-line bootstrap stages only core; external add-ons are kept from the
  installed copy. After a harness merge, doctor can show `policy-overlay`
  drift until `tool.py maintain` runs.
- In a linked worktree, `repair --component policy-overlay` always fails; to
  reproduce a worktree install, materialize from the primary, then
  `repair --component hosts`.
- Treat checkpoint exit 3 as stop, not error; a gap over 50 minutes is a new
  session (rerun `entry --full`).
- MemPalace: compile privacy terms into one alternation (a thousand separate
  regexes over tens of thousands of drawers exceed two minutes), start mines
  over ten minutes in the background, and list every installer artifact in
  ChaosEngine-owned excludes, because MemPalace reads only `.gitignore`.
  Re-mining is idempotent.

- A repo can keep its git dir elsewhere (`.git` file, `git init --separate-git-dir`).
  The parent of `--git-common-dir` is the worktree only when that dir is named
  `.git`; `git worktree list` then names the git dir itself. Resolve with
  `--show-toplevel`, and test path code with a separate-git-dir repo.

- Core `chaos-engine/` files ship portable: no repository or owner names
  (installer test) and no ecosystem terms (`ECOSYSTEM_TERMS` in
  `scripts/ci/ce_lean_lint.py`; it scans references too). Put ecosystem data in `packs/<lang>/` and project data in an
  add-on's JSON, and have core code read it.

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
Register rule and token tactics: [token tactics card](../companions/token-tactics.md).

MemPalace privacy: [mempalace_privacy.py](../mempalace_privacy.py) scans mined drawers for
emails, money amounts and the untracked `.chaos-engine-state/privacy-terms.txt` terms;
it prints counts only and exits 1 on any hit.
