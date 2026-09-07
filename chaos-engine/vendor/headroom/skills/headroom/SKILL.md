---
name: headroom
description: >
  ChaosEngine Headroom companion for wrap/proxy/MCP and token-budget mapping.
  Enforces agent-90 max savings, beacon off, Ponytail XOR OUTPUT_SHAPER.
---

# Headroom (ChaosEngine companion)

Apache-2.0 upstream `headroom-ai` — managed pin in [PIN.json](../../PIN.json); inventory [INVENTORY.md](../../INVENTORY.md).
MemPalace + Graphify remain the RAG/memory spine. Headroom memory injection
stays **off** by default.

## CE max-savings defaults

| Knob | CE default | Notes |
| --- | --- | --- |
| `HEADROOM_SAVINGS_PROFILE` | **`agent-90`** | ~90% target; `token` mode; `force_kompress`; keep-ratio `0.10` |
| `HEADROOM_BEACON` | `off` | CE privacy default (also honor `DO_NOT_TRACK`) |
| `HEADROOM_MEMORY_INJECTION_MODE` | `disabled` | MemPalace/Graphify SoT |
| `HEADROOM_OUTPUT_SHAPER` | `0` (off) | **XOR Ponytail** — never both for output lean |
| Optional add-ons | `THINKING_COMPACT`, `COLD_RECOMPACT`, `DEDUPE` | Documented; not a separate profile |

Token budget map (do not invent aliases):

| `CHAOS_ENGINE_TOKEN_BUDGET` | Headroom profile |
| --- | --- |
| `ultra-lean` | `agent-90` |
| `balanced` (default) | `balanced` (or upstream `coding` if operator opts cache-first) |
| `deep` | `coding` / passthrough |

`agent-90` trades prefix-cache stability for max crush (`proxy_mode=token`).
Protect tool results / CCR under crush; never strip MemPalace/Graphify payloads
operators need for retrieval.

## Install / wrap / proxy

```bash
# Managed pin (exact version from PIN.json)
uv tool install --python 3.13 "headroom-ai==0.37.0"

# Apply CE env, then wrap or proxy
eval "$(python3 .chaos-engine/headroom_policy.py export-env)"
HEADROOM_SAVINGS_PROFILE=agent-90 headroom wrap claude   # or codex|grok|copilot|cursor
HEADROOM_SAVINGS_PROFILE=agent-90 headroom proxy --port 8787
headroom mcp serve   # headroom_compress | headroom_retrieve | headroom_stats
headroom doctor
```

Library path: prefer `default_mode=optimize` (not `audit`/`simulate`).

## Coexistence

- **Caveman** = communication style; **Headroom** = payload compression (complementary).
- **Ponytail XOR `HEADROOM_OUTPUT_SHAPER`** for output lean — CE keeps Ponytail ultra on by default, so OUTPUT_SHAPER stays off unless the operator stops Ponytail.
- Soft-degrade when the CLI is absent: SessionStart still exposes this skill; doctor reports advisory fix-next.

## Local smoke

```bash
python3 -c "from pathlib import Path; import json; print(json.load(open('chaos-engine/vendor/headroom/PIN.json'))['version'])"
python3 chaos-engine/headroom_policy.py self-check
# After uv tool install:
HEADROOM_SAVINGS_PROFILE=agent-90 HEADROOM_BEACON=off headroom doctor
```

See [references/headroom.md](../../../../references/headroom.md).
