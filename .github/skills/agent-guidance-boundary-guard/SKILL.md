---
name: agent-guidance-boundary-guard
description: Use when auditing SHAFT agent guidance, host adapters, skill bridges, guidance budgets, Codex config, or Memory setup.
---

# Agent Guidance Boundary Guard

Use for agent guidance and memory/config surfaces. Keep `AGENTS.md` canonical and host adapters thin.

## Workflow

1. For new or renamed skills, update the bridge in `.agents/skills/**`, the canonical `.github/skills/**` playbook, `.github/skills/README.md`, and `scripts/ci/agent_guidance_budget.json`.
2. Keep `.agents` skill bodies under their configured budget and include `agents/openai.yaml` with `allow_implicit_invocation: true`.
3. For `.codex` or `.memory` changes, preserve the restricted Memory MCP setup and avoid routine diary/end-session saves.
4. Run `python3 scripts/ci/validate_agent_setup.py --skip-external`; run the full command when network/npm access is stable.
5. If refresh automation changes guidance, check `.github/workflows/refresh-agent-instructions.yml` allowlist before widening scope.

## Output

List guidance surfaces changed, budget/metadata changes, validation command results, and any intentional budget headroom.
