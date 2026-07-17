# User harness (canonical)

Canonical copies of user-level Claude harness config live here, in
SHAFT_ENGINE, instead of only in `~/.claude`. This directory is the source
of truth; `~/.claude` is a deploy target.

- `scripts/agents/sync_user_harness.py` (no args or `--check`) compares
  each file here against its counterpart in `~/.claude` and reports
  IN-SYNC / DRIFTED / MISSING per file.
- `sync_user_harness.py --apply` deploys these files to `~/.claude`,
  backing up any differing existing target to `<name>.bak` first.

Secrets and machine/runtime state are never synced or committed:
`.credentials.json`, caches, sessions, plugins, and `projects/` all stay
local to `~/.claude` only.

`~/.claude` may separately keep its own private local git repo (not this
one) purely for local rollback history of runtime state.
