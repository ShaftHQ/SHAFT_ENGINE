# Loopback runtime contract

Shared rules for an optional local MoE runtime that serves an OpenAI-compatible
API on a fixed loopback port (FreeToken, Colibri). The runtime page names its
port, CLI, probe helper, and when to prefer it.

## Hard rails (never regress)

- Do **not** install the runtime, download or convert weights, or start its
  server from ChaosEngine.
- Do **not** rewrite Claude / Codex / OpenCode config files from these skills.
- Do **not** clear cloud API keys. Do not persist route, model, or provider IDs
  in receipts or repository files.
- Installer / doctor / status must **not** fail because the runtime is missing.
- Never bind or probe a non-loopback URL from ChaosEngine; ambient base-URL
  variables are ignored for the probe host.

## Agent machine vs user machine

The runtime almost always lives on the **user's GPU/disk machine**. An agent box
loopback is a different host. If the adopter asked for the runtime and Shell is
not on the user machine, say so, require Local Execution / the user host, and
stop. Do not invent remote `--base-url` workarounds from ChaosEngine.

## Probe → attest → models → dispatch

1. **Probe** with the runtime's `probe.py`:

   | State | Meaning |
   | --- | --- |
   | `ABSENT` | CLI missing and nothing answers on the fixed loopback port. Normal. |
   | `UNHEALTHY` | CLI exists but the port is closed, or the response is not a models payload. Do not start the server. |
   | `READY` | JSON list or object with `models`/`data` array. |

2. **Attest** on `READY` with `probe.py attest` (this session only).
3. **Models** with `probe.py models --json`: an array of `{id}` plus
   `context_length` only when the payload advertised a positive integer.
   Session stdout only; never persist ids or measured KV in git. Advertised
   `context_length` is not usable KV; on `context_length_exceeded` shrink the
   prompt and variant first.
4. **Dispatch** when `READY`: set **ephemeral** env for one bounded implementer
   process only (for example `OPENAI_BASE_URL=http://127.0.0.1:<port>/v1`, plus
   the Anthropic base URL when the client needs `/v1/messages`). Point OpenCode
   / Claude Code / Codex at that loopback without starting the server and
   without rewriting durable host config.

`ABSENT` / `UNHEALTHY`: tell the operator the runtime is optional; point at the
guide; continue with other qualified paths. Never auto-serve.

Prefer smaller coding MoEs when
[`probe_hardware.py`](../../local-coding-delegate/scripts/probe_hardware.py)
returns `small` or `medium`; do not stretch to a larger checkpoint until
mechanical dispatch knobs are proven.

## Related

- Identity push-back: [identity-push-back.md](../../../references/identity-push-back.md)
- OmniRoute (cloud-quota peer, not a dependency): [omniroute](omniroute.md)
- Loopback peers (Ollama / LM Studio / llamacpp): [local-openai-compat](local-openai-compat.md)
- Local OpenCode agency: [local-agency](../../local-agency/SKILL.md)
