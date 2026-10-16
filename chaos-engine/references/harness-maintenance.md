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

## Maintain

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

