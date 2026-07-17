# Global harness shim — canonical config lives in SHAFT_ENGINE

The agent harness (instructions, skills, this file) is source-controlled in
`C:\Users\Mohab\IdeaProjects\SHAFT_ENGINE` (`AGENTS.md`, `.claude/`). Only
secrets and machine/runtime state live in `~/.claude`; they are never
committed to any repository.

Outside SHAFT_ENGINE:
- Nontrivial engineering task -> read and follow
  `C:\Users\Mohab\IdeaProjects\SHAFT_ENGINE\.claude\skills\act-as-fable\SKILL.md`.
- `/graphify` -> read and follow
  `C:\Users\Mohab\IdeaProjects\SHAFT_ENGINE\.claude\skills\graphify\references\full-pipeline.md`.

Drift check / deploy:
`py -3 C:\Users\Mohab\IdeaProjects\SHAFT_ENGINE\scripts\agents\sync_user_harness.py [--apply]`
