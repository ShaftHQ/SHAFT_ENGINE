# Colibri: frontier MoE companion (multitier inference)

Colibri ([JustVugg/colibri](https://github.com/JustVugg/colibri)) is an optional
pure-C inference engine that treats **storage, RAM, and VRAM as one hierarchy**
so frontier MoE models (documented families span ~7B OLMoE up through
hundreds-of-B / multi-T checkpoints) can run without fitting every expert in
fast memory. ChaosEngine treats it as a **third-party integration** in the same
class as FreeToken: probe and dispatch against an already-running loopback
server. ChaosEngine never installs it.

| | OmniRoute | FreeToken | Colibri |
| --- | --- | --- | --- |
| Role | Optional cloud-quota transport | Optional local MoE / coding-weights server | Optional frontier MoE multitier server |
| Loopback | `http://127.0.0.1:20128` | `http://127.0.0.1:1919` | `http://127.0.0.1:8000` |
| ChaosEngine | Never installs or authenticates providers | Never installs, downloads weights, or starts `ft` | Never installs, converts weights, or starts `coli` |
| Coupling | None to FreeToken/Colibri | None to OmniRoute/Colibri | None to OmniRoute/FreeToken |

Skill: [Colibri](../skills/colibri/SKILL.md).

```bash
python3 chaos-engine/skills/colibri/scripts/probe.py
python3 chaos-engine/skills/colibri/scripts/probe.py attest
python3 chaos-engine/skills/colibri/scripts/probe.py models --json
```

## Capability comparison (research summary)

| Dimension | FreeToken (FlashML-org) | Colibri |
| --- | --- | --- |
| Primary bet | Edge-native MoE serving for practical local coding / chat MoEs | Inference systems research: place experts across NVMe/RAM/VRAM |
| Typical scale | Coding-sized MoEs that fit a hybrid GPU/CPU path | Frontier MoEs (documented 744B–2.8T families) streamed from disk |
| Engine shape | Vendor `ft serve` stack | Tiny C engines (`coli chat` / `coli serve` / `coli web`) |
| API | OpenAI + Anthropic on `:1919` | OpenAI + Anthropic on `:8000` (default) |
| Agent loops | Preferred CE local-agency peer when READY | Use only for short prompts after smoke-test; long agent prefills can stall on disk paths |
| Disk | Modest vs frontier containers | Hundreds of GB–TB containers common |
| Install from CE | Forbidden | Forbidden |

**Verdict:** keep **both**. FreeToken wins for day-to-day local coding agency.
Colibri wins when the operator deliberately hosts a frontier MoE on multitier
hardware. Dropping either would erase a distinct capability.

## Operator install (vendor docs only)

Not the ChaosEngine installer. Do not add Colibri to the default installer
bundle. Do not run `coli serve` / `coli web` / `coli convert` from ChaosEngine.

Typical vendor sequence (see upstream README / `docs/quickstart.md` /
`docs/api.md`):

```bash
# obtain coli binary (release archive or build from source)
COLI_MODEL=/path/to/converted-model ./coli serve --host 127.0.0.1 --port 8000
# OpenAI-compat: http://127.0.0.1:8000/v1
# Anthropic-compat: http://127.0.0.1:8000/v1/messages
```

Smoke-test before any agency client:

```bash
curl -sf http://127.0.0.1:8000/v1/models
curl -sf http://127.0.0.1:8000/v1/chat/completions \
  -H 'Content-Type: application/json' \
  -d '{"model":"<served-id>","messages":[{"role":"user","content":"hi"}],"max_tokens":8}'
```

## Agent vs user machine

If the agent runs on a different computer than `coli serve`, loopback probes
fail closed. Use the user machine (Local Execution) or guide the operator to
install/serve Colibri there. Do not invent remote Colibri URLs from the harness.

## Local-agency ranking

`dispatch.py` ranks FreeToken, then Ollama / LM Studio / llamacpp, then Colibri.
`--prefer colibri` selects Colibri first when READY. Missing Colibri never fails
installer / doctor / status.
