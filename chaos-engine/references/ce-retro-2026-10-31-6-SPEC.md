# CE 2026.10.31.6 consolidated retro SPEC (pointer)

Full origin-only SPEC: [guides/ce-retro-2026-10-31-6-SPEC.md](../guides/ce-retro-2026-10-31-6-SPEC.md).

This PR implements H-* + L60–L64 + H-IDENTITY + H-SCOPE. Enforcement surfaces:

- H-IDENTITY — `commit_identity.py` / `references/commit-identity.md`
- L64 — `addons/design-skills/scripts/uncaptioned_speech.py` + `craft_qc.py uncaptionedspeech`
- H-SCOPE — `scripts/ci/pr_scope_check.py` / `references/pr-scope-check.md`

Personal names and client data stay out of portable tree text.
