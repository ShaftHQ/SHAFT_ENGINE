# CE 2026.10.31.6 consolidated retro SPEC (pointer)

Full origin-only SPEC: [guides/ce-retro-2026-10-31-6-SPEC.md](../guides/ce-retro-2026-10-31-6-SPEC.md).

This PR implements H-* + L60–L65 + H-IDENTITY + H-SCOPE + H-PR-SCOPE. Enforcement surfaces:

- H-IDENTITY — `commit_identity.py` / `references/commit-identity.md` (`allowed_remotes()`; optional `CE_OWNER_IDENTITY`; origin-only SPEC names both remotes)
- L64 — `addons/design-skills/scripts/uncaptioned_speech.py` + `craft_qc.py uncaptionedspeech` (`human: true`)
- H-SCOPE — `references/h-scope.md` (L41 / sequential-wip / parallel_stream_conflict)
- H-PR-SCOPE — `scripts/ci/pr_scope_check.py` / `references/pr-scope-check.md` (wired in overlay_pre_push + harness_pr_gate)
- L65 — `addons/design-skills/scripts/motion_graphics_gate.py` + `craft_qc.py mggate`

Personal names and client data stay out of portable tree text.
