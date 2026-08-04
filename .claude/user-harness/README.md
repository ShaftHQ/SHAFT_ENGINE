# User harness

Canonical user-level host config lives here. Run
`py -3 scripts/agents/sync_user_harness.py --check` for drift or add `--apply`
to deploy with backups.

Sync scope is explicit: Claude `CLAUDE.md`, tracked non-secret settings keys and
role/skill adapters; Codex `AGENTS.md` and role adapters; and the canonical
`.agents` skills. Settings are merged recursively, so unrelated environment
variables, plugins, credentials, and personal keys remain untouched. Managed
files are exact copies, existing backups are preserved, and an unowned Codex
guidance collision fails closed instead of being overwritten.
