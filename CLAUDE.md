# CLAUDE.md

@AGENTS.md

## Claude-Specific Guidance

- Treat imported `AGENTS.md` as the canonical repository policy; do not restate
  it in responses or append session logs to this file.
- Use progressive disclosure: inspect the smallest relevant file set, then load
  one task playbook from `.github/skills/` only when its trigger matches.
- Prefer local deterministic commands over extra model, web, MCP, GitHub, or
  cloud calls. Use Claude's reported usage/cost data only when available and do
  not estimate billing.
- Keep plans and final responses proportional to the task. Avoid duplicate
  summaries and stop after the requested behavior is verified.
- For Java edits, apply the matching path-scoped file in
  `.github/instructions/`.
