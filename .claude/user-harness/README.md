# User harness

Canonical user-level host config lives here. Run
`py -3 scripts/agents/sync_user_harness.py --check` for drift or add `--apply`
to deploy with backups.

Sync scope is explicit: only Claude `CLAUDE.md` and tracked non-secret settings
keys. It does not deploy skills or role adapters; each repository supplies its
source-controlled entrypoint and roles. Settings are merged recursively, so
unrelated environment variables, plugins, credentials, and personal keys remain
untouched. Exact historical sync-owned user copies are backed up and retired;
an unowned guidance collision stops the migration before any profile change.
