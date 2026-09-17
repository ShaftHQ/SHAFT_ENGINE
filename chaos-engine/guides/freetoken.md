# FreeToken: standalone local-weights companion

FreeToken ([FlashML-org/FreeToken](https://github.com/FlashML-org/FreeToken)) is
an optional edge-native MoE serving engine. ChaosEngine treats it as a
**third-party integration** in the same class as OmniRoute: probe and dispatch
against an already-running loopback server. ChaosEngine never installs it.

| | OmniRoute | FreeToken |
| --- | --- | --- |
| Role | Optional cloud-quota transport | Optional local MoE / local-weights server |
| Loopback | `http://127.0.0.1:20128` | `http://127.0.0.1:1919` |
| ChaosEngine | Never installs or authenticates providers | Never installs, downloads weights, or starts `ft` |
| Coupling | None to FreeToken | None to OmniRoute |

Skill: [FreeToken](../skills/freetoken/SKILL.md).

```bash
python3 chaos-engine/skills/freetoken/scripts/probe.py
python3 chaos-engine/skills/freetoken/scripts/probe.py attest
python3 chaos-engine/skills/freetoken/scripts/probe.py models --json
```

## Operator install (vendor docs only)

Not the ChaosEngine installer. Do not add FreeToken to the default installer
bundle. Do not run `ft launch` from ChaosEngine.

Typical vendor sequence on a Linux host with a vendor-supported GPU driver
(see upstream `docs/install.md`):

```bash
uv venv && source .venv/bin/activate
uv pip install "freetoken[accel]"
ft bench bw   # once per machine; enables hybrid MoE when useful
ft serve --model /path/or/HF-id
```

API surfaces: OpenAI `/v1/chat/completions`, `/v1/models`; Anthropic
`/v1/messages`. Server ready log mentions `127.0.0.1:1919`.

## Size-class soak (probe_hardware.py)

Use [`probe_hardware.py`](../skills/local-coding-delegate/scripts/probe_hardware.py)
(`small` / `medium` / `large` / `refuse`). Start with a small/medium known-good
coding MoE. Prove the READY checkpoint with mechanical dispatch knobs
(one command, `--variant` low/medium, tiny tool output) before stretching
checkpoints. Advertised `context_length` from `/v1/models` is not usable KV;
on `context_length_exceeded` shrink prompt and variant first.

If quality is still insufficient and the probe is `medium` or `large`, the
operator may serve the next known-good coding MoE from **vendor docs**.
ChaosEngine never selects, downloads, or starts that serve. `refuse`: do not
recommend a larger checkpoint. Put large weights on a volume with enough
free space.

## Agent vs user machine

If the agent runs on a different computer than `ft serve`, loopback probes
fail closed. Use the user machine (Local Execution) or guide the operator to
install/serve FreeToken there. Do not invent remote FreeToken URLs from the
harness.

## Local proof

Operator soak notes stay on the operator host. Closeout evidence for the
FreeToken coding loop lives in
[freetoken-5867-closeout-proof.md](./freetoken-5867-closeout-proof.md).
