# SHAFT_ENGINE Copilot Instructions

Follow `AGENTS.md` as the canonical repository policy.

- Apply `.github/instructions/framework-source.instructions.md` only to
  production Java and `.github/instructions/java-tests.instructions.md` only
  to test Java.
- Load one matching playbook from `.github/skills/` for CI, flaky tests,
  release/deps, UI, ads, docs, MCP, modules, reports, guidance, or PDCA work.
- Prefer targeted reads and deterministic local checks; reuse valid evidence.
- Preserve unrelated work and never expose secrets or claim unverified remote
  results.
- Guidance/memory changes: run `python3 scripts/ci/validate_agent_setup.py`.
