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
`~/.config/opencode/opencode.json`. `argv` includes `--pure` and `--variant`
(default `medium`).

## Mechanical dispatch

Small-context local models often exit 0 without tools, overflow on the next
turn after verbose logs, or miss gitignored trees via glob.

- Orchestrator writes one bounded runner; OpenCode runs exactly one command.
- EXIT 0 with zero tool calls after a multi-step spec is a writer failure.
  Run the same one command in the worktree yourself; do not retry the
  oversized prompt.
- Use exact paths; OpenCode glob may skip gitignored trees (including Memory).
- Do not feed verbose unit-test logs into the next model turn.
- Advertised `context_length` is not usable KV. On `context_length_exceeded`,
  shrink prompt and variant; do not start `ft serve` from ChaosEngine.
- Size-class soak (`probe_hardware.py`: small / medium / large / refuse):
  knobs first. If still insufficient and class is medium or large, the
  operator may serve the next known-good coding MoE from vendor docs.
  `refuse`: do not recommend a larger checkpoint.

## Proven path (loopback + ephemeral config)

With a READY local runtime on loopback:

1. `dispatch.py resolve` → `state=READY` and a chosen local runtime
2. `OPENCODE_CONFIG=<ephemeral>` `opencode run --pure --variant medium …`
3. Durable `~/.config/opencode/opencode.json` hash unchanged

## Session agents vs local agency

Default orchestrator labor stays on host session subagents / Task. Use this
guide only when the adopter asked for OpenCode (or peer) against local
runtimes. If no local runtime is READY, say so; do not auto-route to OmniRoute
unless asked.
