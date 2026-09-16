---
name: freetoken
description: >-
  Use when an orchestrated workflow may dispatch bounded implementation through
  an optional local FreeToken process (standalone; not OmniRoute).
license: MIT
---

# FreeToken

Optional **local MoE / local-weights** companion from
[FlashML-org/FreeToken](https://github.com/FlashML-org/FreeToken). It is not a
workflow owner; select the canonical workflow in
[execution workflows](../../references/execution-workflows.md) first.

**Standalone from OmniRoute.** FreeToken loopback is
`http://127.0.0.1:1919`. OmniRoute is `http://127.0.0.1:20128`. Neither
requires the other. Missing FreeToken is normal: use OmniRoute (if READY), a
qualified native implementer, `SOLO`, or another local OpenAI-compat runtime
when those skills exist.

## Hard rails (never regress)

- Do **not** install FreeToken, download weights, start `ft serve`, or run
  `ft launch` (launch rewrites host agent configs and can clear cloud API keys).
- Do **not** rewrite Claude / Codex / OpenCode config files from this skill.
- Do **not** clear cloud API keys. Do not persist route, model, or provider IDs
  in receipts or repository files.
- Installer / doctor / status must **not** fail because FreeToken is missing.
- Never bind or probe a non-loopback FreeToken URL from ChaosEngine.

Operator install stays on vendor docs. See the
[FreeToken guide](../../guides/freetoken.md).

## Agent machine vs user machine

FreeToken is almost always on the **user's GPU machine** (for GPU-backed operator hosts such as a
laptop ROG). An agent box loopback is a different host: `127.0.0.1:1919` there is not
the user's FreeToken. If the adopter asked for FreeToken and Shell is not on
the user machine, say so, require Local Execution / the user host, and stop.
Do not invent remote `--base-url` workarounds from ChaosEngine.

## Probe → attest → models → dispatch

### 1. Probe

Probe helper: [`chaos-engine/skills/freetoken/scripts/probe.py`](scripts/probe.py).

```text
command -v ft
curl -sf --max-time 2 http://127.0.0.1:1919/v1/models
python3 chaos-engine/skills/freetoken/scripts/probe.py
```

Ambient `FREETOKEN_BASE_URL` is ignored for the probe host. States:

| State | Meaning |
| --- | --- |
| `ABSENT` | `ft` missing and nothing answers on the fixed loopback port. Normal. |
| `UNHEALTHY` | `ft` exists but the port is closed, or the response is not a models payload. Do not start the server. |
| `READY` | JSON list or object with `models`/`data` array. |

### 2. Attest

On `READY`, confirm OpenAI (`http://127.0.0.1:1919/v1`) and optionally Anthropic
(`http://127.0.0.1:1919/v1/messages`) answer for this session only:

```text
python3 chaos-engine/skills/freetoken/scripts/probe.py attest
```

### 3. Models (session stdout only)

```text
python3 chaos-engine/skills/freetoken/scripts/probe.py models --json
```

Prefer smaller coding MoEs on constrained GPUs (e.g. RTX 3060 Laptop 6 GB +
~22 GB RAM). Do not assume 35B is reliable on that class until soak-proven.
Optional size-class hint (stdlib, no downloads):

```text
python3 chaos-engine/skills/local-coding-delegate/scripts/probe_hardware.py
```

### 4. Dispatch when READY

For a bounded implementer, set **ephemeral** env for that process only, for
example `OPENAI_BASE_URL=http://127.0.0.1:1919/v1` (and Anthropic base URL when
the client needs it). Point OpenCode / Claude Code / Codex at that loopback
**without** `ft launch` and without rewriting durable host config.

`ABSENT` / `UNHEALTHY`: tell the operator FreeToken is optional; point at the
guide; continue with other qualified paths. Never auto-serve.

## Related

- Guide: [freetoken.md](../../guides/freetoken.md)
- Identity push-back: [identity-push-back.md](../../references/identity-push-back.md)
- OmniRoute (cloud-quota peer, not a dependency): [omniroute skill](../omniroute/SKILL.md)
- Local OpenCode agency (later): GitHub #5872
