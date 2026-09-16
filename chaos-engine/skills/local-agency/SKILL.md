---
name: local-agency
description: >-
  Use when the adopter asks to delegate to local agents (OpenCode / OSS agency)
  against a READY local runtime instead of orchestrator-session subagents.
license: MIT
---

# Local agency (OpenCode)

Optional **local coding agency** path. Not a workflow owner; select the
canonical workflow in [execution workflows](../../references/execution-workflows.md)
first.

**Local agency vs session agents**

| Path | When | Owner |
| --- | --- | --- |
| Session subagents / Task | Default orchestrator labor on the host | Host adapter + [delegation](../../references/delegation.md) |
| Local agency (this skill) | Adopter asked for OpenCode / local OSS agency against local weights | This skill + READY runtime |

## Hard rails (never regress)

- Prefer a **READY** local runtime: FreeToken (`:1919`), then Ollama / LM Studio /
  llamacpp via [local-openai-compat](../local-openai-compat/SKILL.md).
- Configure OpenCode with **ephemeral** `OPENCODE_CONFIG` /
  `OPENCODE_CONFIG_CONTENT` that sets `enabled_providers` to the READY local
  provider. OpenCode merges global config; the allowlist keeps other providers
  out of that process. `--pure` only disables external plugins.
- **Never** write into durable `~/.config/opencode` (helper refuses those paths),
  **never** `ft launch` / `ft serve`, **never** start local servers from CE.
- Do **not** silently fall back to cloud OmniRoute. Missing local runtime:
  tell the operator; continue with session agents / `SOLO` / OmniRoute only when
  the adopter explicitly asked for that path.
- Do **not** install OpenCode from this skill. Operator install stays on vendor
  docs (README third-party table).
- Installer / doctor / status must **not** fail because OpenCode or a local
  runtime is missing.
- Never bind or probe a non-loopback URL from ChaosEngine.

## Probe → resolve → ephemeral OpenCode

Helper:
[`chaos-engine/skills/local-agency/scripts/dispatch.py`](scripts/dispatch.py).

```text
python3 chaos-engine/skills/local-agency/scripts/dispatch.py resolve
python3 chaos-engine/skills/local-agency/scripts/dispatch.py --prefer freetoken config
python3 chaos-engine/skills/local-agency/scripts/dispatch.py argv --prompt '…' --workdir '<worktree>'
```

`resolve` ranks FreeToken then OpenAI-compat peers. On `READY`, `config` /
`argv` emit ephemeral OpenCode material (`OPENCODE_CONFIG` path). Run OpenCode
yourself with that env; do not persist the config into the durable user file.

Example (session only):

```text
python3 chaos-engine/skills/local-agency/scripts/dispatch.py --prefer freetoken config
# export OPENCODE_CONFIG from the JSON env map for one process only
OPENCODE_CONFIG=<ephemeral-path> opencode run --pure --model freetoken/gpt-oss-20b --dir '<worktree>' '<prompt>'
```

Hardware size-class hint (optional):
[`local-coding-delegate/scripts/probe_hardware.py`](../local-coding-delegate/scripts/probe_hardware.py).

## Folded: local-coding-delegate

[local-coding-delegate](../local-coding-delegate/SKILL.md) is a **compat shim**:
hardware probe stays there; local OpenCode / agency routing lives here. Prefer
this skill when the adopter names OpenCode or “local agents”.

## Related

- Guide: [local-agency.md](../../guides/local-agency.md)
- FreeToken: [freetoken](../freetoken/SKILL.md)
- Local OpenAI-compat: [local-openai-compat](../local-openai-compat/SKILL.md)
- OmniRoute (explicit cloud only): [omniroute](../omniroute/SKILL.md)
- Identity push-back: [identity-push-back.md](../../references/identity-push-back.md)
