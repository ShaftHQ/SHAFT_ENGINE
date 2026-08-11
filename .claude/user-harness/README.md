# Claude user-harness template

This directory is the source-controlled template for the maintainer's generic
Claude user-level host configuration. Repository policy still starts at
`.agents/skills/act-as-mohab/SKILL.md`; this template does not replace it.

Run from the repository root:

`py -3 scripts/agents/sync_user_harness.py --check` for drift or add `--apply`
to deploy with backups. Review the reported target paths before applying.

Sync scope is explicit: only Claude `CLAUDE.md` and tracked non-secret settings
keys. It does not deploy skills or role adapters; each repository supplies its
source-controlled entrypoint and roles. Settings are merged recursively, so
unrelated environment variables, plugins, credentials, and personal keys remain
untouched. Exact historical sync-owned user copies are backed up and retired;
an unowned guidance collision stops the migration before any profile change.

Never place credentials, provider tokens, machine-specific paths, repository
skills, or role adapters here. The sync check is safe and read-only; `--apply`
is the explicit mutation boundary.
