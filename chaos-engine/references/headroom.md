# Headroom (native companion)

ChaosEngine provisions Headroom as a **managed pin** of Apache-2.0
[`headroom-ai`](https://pypi.org/project/headroom-ai/) (upstream
[chopratejas/headroom](https://github.com/chopratejas/headroom)), matching the
Caveman/Ponytail companion pattern for skill + SessionStart locators while
keeping the large runtime out of the portable tree.

Normative defaults live in [`../headroom_policy.py`](../headroom_policy.py) and
[`../vendor/headroom/PIN.json`](../vendor/headroom/PIN.json).

## Max savings

CE enforces **`HEADROOM_SAVINGS_PROFILE=agent-90`** for `ultra-lean` / max
profile work (upstream default `coding` is wrong for highest savings).

Optional recommended under ultra-lean **only when Ponytail is off**:

```bash
export HEADROOM_OUTPUT_SHAPER=1
export HEADROOM_THINKING_COMPACT=1
export HEADROOM_COLD_RECOMPACT=1
export HEADROOM_DEDUPE=1
```

CE privacy: `HEADROOM_BEACON=off`. Memory injection: `disabled` (MemPalace /
Graphify remain SoT). Protect tool results via CCR under `agent-90` crush.

## Ponytail XOR OUTPUT_SHAPER

Ponytail owns lean *solution/output bias* in-process. Headroom
`OUTPUT_SHAPER` owns proxy-level output trimming. Running both double-steers
terseness — CE policy keeps OUTPUT_SHAPER off while Ponytail is active.

## Host wrap gaps

`headroom wrap` applies where upstream supports it (Claude Code, Codex, Grok,
Copilot, Cursor manual). Gemini uses proxy/MCP adapter path. See
[host-parity-matrix](host-parity-matrix.md) Headroom row and measured gaps.

## Local smoke

```bash
python3 chaos-engine/headroom_policy.py self-check
python3 chaos-engine/install.py doctor --project .   # headroom component
# Optional runtime:
uv tool install --python 3.13 "headroom-ai==0.37.0"
eval "$(python3 chaos-engine/headroom_policy.py export-env --token-budget ultra-lean)"
headroom doctor
```
