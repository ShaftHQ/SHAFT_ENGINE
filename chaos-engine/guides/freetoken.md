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

Typical vendor sequence on an NVIDIA RTX 30+ Linux host with driver r580+ /
CUDA 13 (see upstream `docs/install.md`):

```bash
uv venv && source .venv/bin/activate
uv pip install "freetoken[accel]"
ft bench bw   # once per machine; enables hybrid MoE when useful
ft serve --model /path/or/HF-id
```

API surfaces: OpenAI `/v1/chat/completions`, `/v1/models`; Anthropic
`/v1/messages`. Server ready log mentions `127.0.0.1:1919`.

## Hardware honesty (ROG G14 class)

On an RTX 3060 Laptop (**6 GB** VRAM) with ~**22 GB** RAM, start with a
**small/medium** known-good coding MoE and measure tokens/s and tool-loop
stability before stretching toward 35B-class checkpoints. Upstream marketing
for “35B on 8 GB laptops” is not a promise for 6 GB chassis. Put large weights
on a volume with enough free space (often `/media/...`, not a full `/home`).

## Agent vs user machine

If the agent runs on a different computer than `ft serve`, loopback probes
fail closed. Use the user machine (Local Execution) or guide the operator to
install/serve FreeToken there. Do not invent remote FreeToken URLs from the
harness.

## Local proof

ROG adoption and soak notes are tracked under GitHub #5867 / #5870 on the
operator host. Closeout evidence for the FreeToken coding loop and children
#5868–#5872 / #5882–#5883 lives in
[freetoken-5867-closeout-proof.md](./freetoken-5867-closeout-proof.md).
