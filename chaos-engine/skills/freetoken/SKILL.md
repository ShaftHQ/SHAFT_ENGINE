---
name: freetoken
description: >-
  Standalone FreeToken process: install, READY probe and bounded dispatch. Use
  when the local-runtimes table points a job at FreeToken rather than a loopback
  server.
license: MIT
---

# FreeToken

Optional **local MoE / local-weights** companion from
[FlashML-org/FreeToken](https://github.com/FlashML-org/FreeToken). It is not a
workflow owner; select the canonical workflow in
[execution workflows](../../references/execution-workflows.md) first.

Peer ports for FreeToken, Colibri, OmniRoute, and local OpenAI-compat are the [transport-order table](../../references/execution-workflows.md#transport-is-orthogonal).

**Standalone from OmniRoute.** FreeToken loopback is
`http://127.0.0.1:1919`. OmniRoute is `http://127.0.0.1:20128`. Neither
requires the other. Missing FreeToken is normal: use OmniRoute (if READY), a
qualified native implementer, `SOLO`, or another local OpenAI-compat runtime
when those skills exist.

**Peer to Colibri.** FreeToken is the default local coding MoE path. Colibri (`:8000`) hosts frontier multitier MoE — see [colibri skill](../colibri/SKILL.md) and guide (repo-only `chaos-engine/guides/colibri.md`). Keep both; choose FreeToken for agency loops, Colibri for frontier MoE when the operator already serves it.

## Hard rails (never regress)

- Do **not** install FreeToken, download weights, start `ft serve`, or run
  `ft launch` (launch rewrites host agent configs and can clear cloud API keys).
- Do **not** rewrite Claude / Codex / OpenCode config files from this skill.
- Do **not** clear cloud API keys. Do not persist route, model, or provider IDs
  in receipts or repository files.
- Installer / doctor / status must **not** fail because FreeToken is missing.
- Never bind or probe a non-loopback FreeToken URL from ChaosEngine.

Operator install stays on vendor docs. See the
FreeToken guide (repo-only `chaos-engine/guides/freetoken.md`).

## Agent machine vs user machine

FreeToken is almost always on the **user's GPU machine**. An agent box
loopback is a different host: `127.0.0.1:1919` there is not the user's
FreeToken. If the adopter asked for FreeToken and Shell is not on the user
machine, say so, require Local Execution / the user host, and stop. Do not
invent remote `--base-url` workarounds from ChaosEngine.

## Probe → attest → models → dispatch

### 1. Probe

Probe helper: [`chaos-engine/skills/freetoken/scripts/probe.py`](scripts/probe.py).

```text
command -v ft
curl -sf --max-time 2 http://127.0.0.1:1919/v1/models
python3 .chaos-engine/skills/freetoken/scripts/probe.py
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
python3 .chaos-engine/skills/freetoken/scripts/probe.py attest
```

### 3. Models (session stdout only)

```text
python3 .chaos-engine/skills/freetoken/scripts/probe.py models --json
```

JSON `models` is an array of objects `{id}` plus `context_length` only when the
loopback payload advertised a positive integer. Session stdout only; never
persist ids or measured KV in git.

Prefer smaller coding MoEs when
[`probe_hardware.py`](../local-coding-delegate/scripts/probe_hardware.py)
returns `small` or `medium`. Do not stretch to a larger checkpoint until
mechanical dispatch knobs are proven. Advertised `context_length` is not
usable KV; on `context_length_exceeded` shrink prompt and variant first.
Optional size-class hint (stdlib, no downloads):

```text
python3 .chaos-engine/skills/local-coding-delegate/scripts/probe_hardware.py
```

### 4. Dispatch when READY

For a bounded implementer, set **ephemeral** env for that process only, for
example `OPENAI_BASE_URL=http://127.0.0.1:1919/v1` (and Anthropic base URL when
the client needs it). Point OpenCode / Claude Code / Codex at that loopback
**without** `ft launch` and without rewriting durable host config.

`ABSENT` / `UNHEALTHY`: tell the operator FreeToken is optional; point at the
guide; continue with other qualified paths. Never auto-serve.

## Related

- Guide: freetoken.md (repo-only `chaos-engine/guides/freetoken.md`)
- Identity push-back: [identity-push-back.md](../../references/identity-push-back.md)
- OmniRoute (cloud-quota peer, not a dependency): [omniroute skill](../omniroute/SKILL.md)
- Local OpenAI-compat peers (Ollama / LM Studio / llamacpp): [local-openai-compat](../local-openai-compat/SKILL.md)
- Local OpenCode agency: [local-agency](../local-agency/SKILL.md)
