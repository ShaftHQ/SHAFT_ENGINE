# User harness

Canonical user-level host config lives here. Run
`py -3 scripts/agents/sync_user_harness.py --check` for drift or add `--apply`
to deploy with backups.

Sync scope is explicit: `CLAUDE.md`, tracked non-secret settings keys, thin role
adapters, the Claude act-as-mohab redirect, and canonical `.agents` entrypoint.
Settings are merged recursively, so unrelated environment variables, plugins,
credentials, and personal keys remain untouched; other managed files are exact
copies.
