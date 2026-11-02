# Local OpenAI-compat runtimes: standalone companions

Optional local OpenAI-compatible servers treated as **third-party integrations**
in the same class as FreeToken and OmniRoute: probe and dispatch against an
already-running loopback server. ChaosEngine never installs them.

| | OmniRoute | FreeToken | Local OpenAI-compat |
| --- | --- | --- | --- |
| Role | Optional cloud-quota transport | Optional local MoE / weights | Optional Ollama / LM Studio / llama.cpp |
| Loopback defaults | `http://127.0.0.1:20128` | `http://127.0.0.1:1919` | `:11434` / `:1234` / `:8080` |
| ChaosEngine | Never installs providers | Never installs or starts `ft` | Never installs or starts these servers |
| Coupling | None to FreeToken | None to OmniRoute | None to OmniRoute or FreeToken |

Skill: [local-openai-compat](../skills/local-runtimes/references/local-openai-compat.md).

```bash
python3 chaos-engine/skills/local-openai-compat/scripts/probe.py
python3 chaos-engine/skills/local-openai-compat/scripts/probe.py --backend ollama
python3 chaos-engine/skills/local-openai-compat/scripts/probe.py --backend lmstudio attest
python3 chaos-engine/skills/local-openai-compat/scripts/probe.py --backend llamacpp models --json
```

## Discovery defaults

| Backend | Models URL | OpenAI base | CLI hint |
| --- | --- | --- | --- |
| Ollama | `http://127.0.0.1:11434/v1/models` | `http://127.0.0.1:11434/v1` | `ollama` |
| LM Studio | `http://127.0.0.1:1234/v1/models` | `http://127.0.0.1:1234/v1` | `lms` |
| llama.cpp server | `http://127.0.0.1:8080/v1/models` | `http://127.0.0.1:8080/v1` | `llama-server` |

Other OpenAI-compat hosts (vLLM, SGLang, and similar) can use `--url` with a
single `--backend` when the path is loopback `/v1/models`.

## Operator install (vendor docs only)

Not the ChaosEngine installer. Do not add these tools to the default installer
bundle. Do not start their servers from ChaosEngine.

- Ollama: [ollama.com](https://ollama.com/) · [OpenAI compatibility](https://ollama.com/blog/openai-compatibility)
- LM Studio: [OpenAI compatibility docs](https://lmstudio.ai/docs/developer/openai-compat)
- llama.cpp: [serve docs](https://github.com/ggml-org/llama.cpp) (default `127.0.0.1:8080`)

## Size-class soak (probe_hardware.py)

Use [`probe_hardware.py`](../skills/local-coding-delegate/scripts/probe_hardware.py)
(`small` / `medium` / `large` / `refuse`). Start with a small/medium coding
model. Prove the READY checkpoint with mechanical dispatch knobs before
stretching. If quality is still insufficient and the probe is `medium` or
`large`, the operator may serve the next known-good coding model from vendor
docs. ChaosEngine never starts those servers. `refuse`: do not recommend a
larger checkpoint. Put large weights on a volume with enough free space.

## Agent vs user machine

If the agent runs on a different computer than the local server, loopback probes
fail closed. Use the user machine (Local Execution) or guide the operator to
install/serve there. Do not invent remote OpenAI-compat URLs from the harness.

## Local proof

Operator soak notes stay on the operator host.
