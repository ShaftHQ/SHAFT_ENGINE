# FreeToken epic #5867 closeout proof receipt

Recorded: 2026-09-16 23:22 UTC+03:00 (Africa/Cairo).

## Children delivered

| Issue | Title | Status | Evidence |
| --- | --- | --- | --- |
| #5868 | Delete legacy FreeToken surface | CLOSED | PR #5874 |
| #5869 | FreeToken skill v2 | CLOSED | PR #5874 |
| #5870 | ROG G14 hardware proof | CLOSED | Live `ft serve` + `/v1/models` + chat smoke on ROG |
| #5871 | Local OpenAI-compat (Ollama/LM Studio/llamacpp) | CLOSED | PR #5878 |
| #5872 | Local agency OpenCode dispatch | CLOSED | local-agency skill + `enabled_providers` |
| #5882 | Learning: `enabled_providers` allowlist | This PR | Dispatch + unit tests |
| #5883 | Learning: spell `llamacpp` in scanned skills | This PR | Skill text + contract test |

## Coding-loop proof (OpenCode ↔ FreeToken)

Target loop (operator ROG, machineId `e4fdb845-7ac4-425a-8c5e-13fdca42bba5`):

1. Serve with enlarged KV (never `ft launch`):

```bash
FT=/media/mohab/OS/Users/Mohab/.local/share/freetoken/venv/bin/ft
"$FT" serve --model openai/gpt-oss-20b --host 127.0.0.1 --port 1919 \
  --moe-strategy offload --num-tokens 24576 --max-output-tokens 2048
```

2. Ephemeral OpenCode config from local-agency:

```bash
python3 .chaos-engine/skills/local-agency/scripts/dispatch.py --prefer freetoken config
# export OPENCODE_CONFIG from JSON env; confirm enabled_providers == ["freetoken"]
```

3. Tiny edit proof (e.g. remaining `llamacpp` → `llamacpp` in a scanned skill) via OpenCode, or fallback:

```bash
curl -sS http://127.0.0.1:1919/v1/chat/completions \
  -H 'Content-Type: application/json' \
  -d '{"model":"gpt-oss-20b","messages":[{"role":"user","content":"ping"}]}'
```

### This closeout session

- **Harness side:** dispatch already emits `enabled_providers`; unit tests assert on-disk config + `OPENCODE_CONFIG_CONTENT` (#5882). Scanned skills use `llamacpp` (#5883).
- **Live ROG OpenCode loop:** blocked for the executor that authored this receipt — Shell was bound to the Cursor box, not ROG Local Execution (`/media/mohab/...` and `:1919` unreachable). Prior #5870 closeout already recorded FreeToken READY + chat smoke on ROG.
- **Dual CE reinstall:** only after this CE PR merges (note for operator).

## Related docs

- [FreeToken guide](../guides/freetoken.md)
- [Local agency guide](../guides/local-agency.md)
- Gotchas: `local-agency-opencode-needs-enabled-providers-allowlist`, `guidance-forbid-regex-llama-catches-llamacpp-vendor-names`
