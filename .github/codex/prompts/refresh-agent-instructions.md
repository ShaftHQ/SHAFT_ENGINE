# Refresh Agent Guidance

Read `.agent-guidance-refresh-request.md` and
`.agent-guidance-audit.json` first. Address only the reported violations or
the explicit maintainer reason.

## Allowed Work

- Edit compact agent guidance: `AGENTS.md`, `CLAUDE.md`,
  `.github/copilot-instructions.md`, `.github/instructions/**`,
  `.github/skills/**`, `docs/AGENT_GUIDANCE_MAINTENANCE.md`,
  `CONTRIBUTING.md`, and `.github/pull_request_template.md`.
- Inspect executable configuration only when needed to verify a disputed fact.
- Preserve unique safety and engineering rules in the narrowest canonical
  surface.

## Constraints

- Do not edit product code, tests, manifests, workflows, validators, generated
  files, dependencies, or secrets.
- Do not perform a broad repository scan when the audit identifies the
  affected file and rule.
- Do not add dates, incident logs, volatile versions, long examples, repeated
  commands, session memory, mandatory repeated work, or automatic remote
  operations to global guidance.
- Prefer replacement and deletion over appending another version of a rule.
- Keep host adapters thin and task workflows in one skill.

## Validation

Run:

```bash
python3 scripts/ci/validate_agent_guidance.py
git diff --check
```

If both already pass and the maintainer reason requires no durable change,
leave the repository unchanged. In the final response, list changed guidance
files, validation results, and any fact that remains uncertain. State that no
product code was intentionally changed.
