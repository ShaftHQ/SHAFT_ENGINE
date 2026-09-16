# Local agency (OpenCode) guide

Operator-facing companion for [local-agency](../skills/local-agency/SKILL.md).

ChaosEngine does **not** install OpenCode. When you want OpenCode against
already-running local weights, CE routes to a READY loopback runtime and emits
ephemeral OpenCode config with `enabled_providers` limited to that runtime —
never `ft launch`, never a silent OmniRoute hop. OpenCode merges global config;
the allowlist (not `--pure`) is what keeps other providers out of that process.
`--pure` only disables external plugins.

## Official install (vendor)

- OpenCode: [https://opencode.ai/docs](https://opencode.ai/docs) (CLI install
  from upstream docs)
- FreeToken: [install.md](https://github.com/FlashML-org/FreeToken/blob/main/docs/install.md)
- Ollama / LM Studio / llamacpp: see [local-openai-compat guide](local-openai-compat.md)

## Skill helpers

```text
python3 chaos-engine/skills/local-agency/scripts/dispatch.py resolve
python3 chaos-engine/skills/local-agency/scripts/dispatch.py --prefer freetoken config
python3 chaos-engine/skills/local-agency/scripts/dispatch.py argv --prompt 'smoke' --workdir .
```

`config` prints JSON with `env.OPENCODE_CONFIG` pointing at a temp
`opencode.json`. Export that for one process only. Do not merge it into
`~/.config/opencode/opencode.json`.

## Proven path (ROG + FreeToken)

With FreeToken serving on `http://127.0.0.1:1919` (`gpt-oss-20b`):

1. `dispatch.py resolve` → `state=READY`, `chosen.runtime=freetoken`
2. `OPENCODE_CONFIG=<ephemeral>` `opencode run --pure --model freetoken/gpt-oss-20b …`
3. Durable `~/.config/opencode/opencode.json` hash unchanged

## Session agents vs local agency

Default orchestrator labor stays on host session subagents / Task. Use this
guide only when the adopter asked for OpenCode (or peer) against local
runtimes. If no local runtime is READY, say so; do not auto-route to OmniRoute
unless asked.
