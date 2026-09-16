---
name: local-openai-compat
description: >-
  Use when an orchestrated workflow may dispatch bounded implementation through
  an optional local OpenAI-compatible runtime (Ollama, LM Studio, llama.cpp).
license: MIT
---

# Local OpenAI-compat runtimes

Optional **bring-your-own** local OpenAI-compatible servers. Peer class to
[FreeToken](../freetoken/SKILL.md): probe and dispatch against an already-running
loopback server. Not a workflow owner; select the canonical workflow in
[execution workflows](../../references/execution-workflows.md) first.

**Standalone from OmniRoute and FreeToken.** Neither is required. Missing these
runtimes is normal: use FreeToken (if READY), OmniRoute (if READY), a qualified
native implementer, or `SOLO`.

| Backend | Default loopback | Typical CLI on PATH |
| --- | --- | --- |
| `ollama` | `http://127.0.0.1:11434/v1` | `ollama` |
| `lmstudio` | `http://127.0.0.1:1234/v1` | `lms` |
| `llamacpp` | `http://127.0.0.1:8080/v1` | `llama-server` |

Other OpenAI-compat servers (vLLM, etc.) may reuse `--url` on a single backend
slot when the models path is loopback `/v1/models`.

## Hard rails (never regress)

- Do **not** install Ollama, LM Studio, llama.cpp, or download weights from this
  skill. Do **not** start their servers from ChaosEngine.
- Do **not** rewrite Claude / Codex / OpenCode durable host config from here.
- Do **not** clear cloud API keys. Do not persist route, model, or provider IDs
  in receipts or repository files.
- Installer / doctor / status must **not** fail because these runtimes are missing.
- Never bind or probe a non-loopback URL from ChaosEngine.
- Not an OmniRoute plugin and not a FreeToken plugin.

Operator install stays on vendor docs. See the
[local OpenAI-compat guide](../../guides/local-openai-compat.md).

## Agent machine vs user machine

These runtimes almost always live on the **user's GPU machine**. An agent box
loopback is a different host: `127.0.0.1:<port>` there is not the user's server.
If the adopter asked for a local runtime and Shell is not on the user machine,
say so, require Local Execution / the user host, and stop. Do not invent remote
`--base-url` workarounds from ChaosEngine.

## Probe → attest → models → dispatch

### 1. Probe

Probe helper:
[`chaos-engine/skills/local-openai-compat/scripts/probe.py`](scripts/probe.py).

```text
python3 chaos-engine/skills/local-openai-compat/scripts/probe.py
python3 chaos-engine/skills/local-openai-compat/scripts/probe.py --backend ollama
python3 chaos-engine/skills/local-openai-compat/scripts/probe.py --backend lmstudio
python3 chaos-engine/skills/local-openai-compat/scripts/probe.py --backend llamacpp
```

Ambient `OPENAI_BASE_URL` / `OLLAMA_HOST` are ignored for the probe host. States:

| State | Meaning |
| --- | --- |
| `ABSENT` | Known CLI missing and nothing answers on the default loopback port. Normal. |
| `UNHEALTHY` | CLI exists but the port is closed, or the response is not a models payload. Do not start the server. |
| `READY` | JSON list or object with `models`/`data` array. |

Default (`--backend all`) prints `backend STATE` lines. A single backend prints
only the state token.

### 2. Attest

```text
python3 chaos-engine/skills/local-openai-compat/scripts/probe.py --backend ollama attest
```

### 3. Models (session stdout only)

```text
python3 chaos-engine/skills/local-openai-compat/scripts/probe.py --backend ollama models --json
```

Prefer smaller coding models on constrained GPUs (e.g. RTX 3060 Laptop 6 GB).
Optional size-class hint:

```text
python3 chaos-engine/skills/local-coding-delegate/scripts/probe_hardware.py
```

### 4. Dispatch when READY

For a bounded implementer, set **ephemeral** env for that process only, for
example `OPENAI_BASE_URL=http://127.0.0.1:11434/v1` for Ollama. Point the client
at that loopback without rewriting durable host config and without starting the
server from ChaosEngine.

`ABSENT` / `UNHEALTHY`: tell the operator the runtime is optional; point at the
guide; continue with other qualified paths. Never auto-serve.

## Related

- Guide: [local-openai-compat.md](../../guides/local-openai-compat.md)
- FreeToken peer: [freetoken skill](../freetoken/SKILL.md)
- Local OpenCode agency: [local-agency](../local-agency/SKILL.md)
- OmniRoute peer (cloud-quota, not a dependency): [omniroute skill](../omniroute/SKILL.md)
- Identity push-back: [identity-push-back.md](../../references/identity-push-back.md)
