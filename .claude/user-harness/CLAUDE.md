# Global agent harness

Before every task, load the deployed relative entrypoint
`../.agents/skills/act-as-mohab/SKILL.md`. It selects working
method, capability tier, skills, MCPs, and knowledge sources. Project
`AGENTS.md` files add repository-specific constraints.

This file, thin role adapters, and canonical entrypoint are deployed from a
source-controlled harness. Secrets, credentials, sessions, caches, indexes,
and other runtime state stay local and are never synced.
