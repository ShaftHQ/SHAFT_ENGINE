---
name: colibri
description: >-
  Use when an orchestrated workflow may dispatch bounded implementation through
  an optional local Colibri process (frontier MoE via disk/RAM/VRAM hierarchy;
  standalone; not OmniRoute or FreeToken).
license: MIT
---

# Colibri

Optional **frontier MoE / multitier local-weights** companion from
[JustVugg/colibri](https://github.com/JustVugg/colibri). It is not a workflow
owner; select the canonical workflow in
[execution workflows](../../references/execution-workflows.md) first.

**Standalone from OmniRoute and FreeToken.** Colibri loopback defaults to
`http://127.0.0.1:8000`. FreeToken is `http://127.0.0.1:1919`. OmniRoute is
`http://127.0.0.1:20128`. None requires another. Missing Colibri is normal: use
FreeToken (if READY), OmniRoute (if READY), a qualified native implementer,
`SOLO`, or another local OpenAI-compat runtime when those skills exist.

## When Colibri vs FreeToken

Support **both**. There is no single winner — pick by workload:

| Prefer | When |
| --- | --- |
| **FreeToken** | Coding / agent loops on GPU-friendly coding MoEs; lower disk footprint; CE local-agency default after probe. |
| **Colibri** | Frontier-scale MoE (hundreds of B to multi-T) via disk→RAM→VRAM staging when the operator already converted/served weights; research or single surgical asks — **not** multi-turn agent swarms on a cold disk stream. |

Colibri's own API docs warn that large agent system prompts on a CPU/disk path
can mean long silent prefills. Treat READY Colibri as a **mechanical** runner
only after a tiny `curl` smoke test, the same way FreeToken is treated.

## Hard rails (never regress)

- Do **not** install Colibri, download/convert weights, start `coli serve` /
  `coli web`, or run cluster helpers from ChaosEngine.
- Do **not** rewrite Claude / Codex / OpenCode durable host config from this skill.
- Do **not** clear cloud API keys. Do not persist route, model, or provider IDs
  in receipts or repository files.
- Installer / doctor / status must **not** fail because Colibri is missing.
- Never bind or probe a non-loopback Colibri URL from ChaosEngine.

Operator install stays on vendor docs. See the
[Colibri guide](../../guides/colibri.md).

## Agent machine vs user machine

Colibri almost always lives on the **user's GPU/disk machine**. An agent box
loopback is a different host: `127.0.0.1:8000` there is not the user's Colibri.
If the adopter asked for Colibri and Shell is not on the user machine, say so,
require Local Execution / the user host, and stop. Do not invent remote
`--base-url` workarounds from ChaosEngine.

## Probe → attest → models → dispatch

### 1. Probe

Probe helper: [`scripts/probe.py`](scripts/probe.py).

```text
command -v coli
curl -sf --max-time 2 http://127.0.0.1:8000/v1/models
python3 chaos-engine/skills/colibri/scripts/probe.py
```

Ambient `COLIBRI_BASE_URL` / `OPENAI_BASE_URL` are ignored for the probe host.
States:

| State | Meaning |
| --- | --- |
| `ABSENT` | `coli` missing and nothing answers on the fixed loopback port. Normal. |
| `UNHEALTHY` | `coli` exists but the port is closed, or the response is not a models payload. Do not start the server. |
| `READY` | JSON list or object with `models`/`data` array. |

### 2. Attest

```text
python3 chaos-engine/skills/colibri/scripts/probe.py attest
```

### 3. Models (session stdout only)

```text
python3 chaos-engine/skills/colibri/scripts/probe.py models --json
```

Never persist model ids in git. Prefer FreeToken for coding agency unless the
adopter explicitly asked for Colibri / frontier MoE.

### 4. Dispatch when READY

For a bounded implementer, set **ephemeral** env for that process only, e.g.
`OPENAI_BASE_URL=http://127.0.0.1:8000/v1` (and Anthropic base URL when the
client needs `/v1/messages`). Point OpenCode / Claude Code / Codex at that
loopback **without** starting `coli` and without rewriting durable host config.

`ABSENT` / `UNHEALTHY`: tell the operator Colibri is optional; point at the
guide; continue with other qualified paths. Never auto-serve.

Local-agency rank places Colibri **after** FreeToken and OpenAI-compat peers so
a frontier disk-stream server never silently wins over a coding MoE. Use
`dispatch.py resolve --prefer colibri` when the adopter asked for Colibri.

## Related

- Guide: [colibri.md](../../guides/colibri.md)
- FreeToken peer: [freetoken skill](../freetoken/SKILL.md) · [guide](../../guides/freetoken.md)
- Local agency: [local-agency](../local-agency/SKILL.md)
- Local OpenAI-compat peers: [local-openai-compat](../local-openai-compat/SKILL.md)
- OmniRoute (cloud-quota peer, not a dependency): [omniroute skill](../omniroute/SKILL.md)
