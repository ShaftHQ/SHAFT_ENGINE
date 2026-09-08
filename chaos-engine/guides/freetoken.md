# FreeToken: optional local-weights companion

FreeToken and OmniRoute are different optional transports. ChaosEngine never
installs either. Execution workflows remain the sole owner. Neither replaces
`SOLO` or a native implementer.

| | OmniRoute | FreeToken |
| --- | --- | --- |
| Role | Optional cloud-quota router | Optional local-weights MoE server |
| Loopback | `http://127.0.0.1:20128` | `http://127.0.0.1:1919` |
| ChaosEngine | Never installs, configures, starts, or authenticates | Never installs, downloads weights, or starts |

Guide: [OmniRoute](omniroute.md). Skill:
[FreeToken](../skills/freetoken/SKILL.md). Probe:

```bash
python3 chaos-engine/skills/freetoken/scripts/probe.py
```

The probe GETs only `http://127.0.0.1:1919/v1/models`, rejects redirects, and
ignores proxies and any ambient `FREETOKEN_BASE_URL`. States are `ABSENT`,
`UNHEALTHY`, and `READY`. Missing is normal.

## Operator install is vendor docs

Not the ChaosEngine installer. Do not add FreeToken to the default installer
bundle (Memory, MemPalace, Graphify, Ponytail, Headroom). Do not run
`ft launch`: it rewrites host configs and clears `ANTHROPIC_API_KEY` /
`OPENAI_API_KEY`.

Vendor sequence, from the operator's own machine, as documented by
FlashML-org/FreeToken (Apache-2.0):

```bash
uv pip install "freetoken[accel]"
ft serve
```

Hardware note only: NVIDIA RTX 30 series or newer. ChaosEngine will not start
`ft serve` and will not download weights. If `ft` is on PATH and the probe is
not `READY`, tell the operator the vendor command is `ft serve` and stop.

If `READY`, a bounded implementer may set `http://127.0.0.1:1919/v1` for that
dispatch only. Do not rewrite Claude, Codex, or OpenCode config. Do not clear
cloud API keys. Receipts and repository files never persist route, model, or
provider IDs.

## Local proof

Local serve proof on 2026-09-08 did not run: this computer has no NVIDIA GPU
(`nvidia-smi` absent) and the user's ROG was offline. That does not block the
harness probe. Do not treat a missing server as harness failure.
