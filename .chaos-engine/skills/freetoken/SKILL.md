---
name: freetoken
description: Use when an orchestrated workflow may dispatch bounded implementation through an optional local FreeToken process.
license: MIT
---

# FreeToken

Optional local-weights server. It is not a router and not a workflow owner.
Select the canonical workflow in [execution workflows](../../references/execution-workflows.md)
first. OmniRoute remains the optional cloud-quota gateway. Missing FreeToken
is normal: use OmniRoute, a qualified native implementer, or `SOLO`.

Do not install FreeToken, download weights, start a server, or rewrite
Claude, Codex, or OpenCode config. Do not clear cloud API keys. Receipts and
repository files never persist route, model, or provider IDs. Live stdout of
this probe may name models for the current session only; do not write them.

## Probe only

Fixed probe. Ignore non-loopback URLs. No redirect. No ambient
`FREETOKEN_BASE_URL` override of the fixed probe host.

Use the standard-library [probe](scripts/probe.py):

```text
command -v ft
curl -sf --max-time 2 http://127.0.0.1:1919/v1/models
python3 chaos-engine/skills/freetoken/scripts/probe.py
```

Ready when the API answers. Do not install. If the binary exists but health
fails, do not auto-serve (weights and GPU are unknown). Tell the operator the
vendor command is `ft serve` (not `ft launch`) and ChaosEngine will not start
it. Never invoke `ft launch`. That rewrites host configs and clears
`ANTHROPIC_API_KEY` / `OPENAI_API_KEY`.

The probe emits only `ABSENT`, `UNHEALTHY`, or `READY`:

| State | Meaning |
| --- | --- |
| `ABSENT` | `ft` is missing and nothing answers on the fixed loopback port. Normal. |
| `UNHEALTHY` | `ft` exists but the port is closed, or something answers without a models payload. Do not start the server. |
| `READY` | A JSON list or an object with a `models`/`data` array came back. |

No catalog ranking. No `omniroute run` equivalent.

## Dispatch when READY

If healthy, a bounded implementer may set the loopback OpenAI-compatible base
URL `http://127.0.0.1:1919/v1` for that dispatch only. Do not rewrite host
config files. Do not clear cloud API keys. Do not persist the model id.

`ABSENT` and `UNHEALTHY` leave OmniRoute, native implementers, and `SOLO`
valid. They never fail install, doctor, or the selected workflow.

Installer doctor/status must not fail because FreeToken is missing. This
probe is the capability check. FreeToken is not an installer bundle component
and is not installed by default.

Operator install stays on vendor docs. See the
[FreeToken guide](../../guides/freetoken.md). Hardware note only: NVIDIA RTX
30 series or newer, as documented by FlashML-org/FreeToken. Apache-2.0.
ChaosEngine does not install the package or start `ft serve`.
