<!-- Folded from skills/local-coding-delegate/SKILL.md (#6200). -->
# Local coding delegate (folded)

This skill is a **compatibility shim**.

- Prefer [local-agency](../../local-agency/SKILL.md) when the adopter asks for
  **OpenCode / local OSS agency** against a READY local runtime (not session
  subagents).
- Prefer [FreeToken](freetoken.md) for local-weights probe/attest when
  the ask is FreeToken / local MoE inference without an agency CLI.
- Prefer [local-openai-compat](local-openai-compat.md) for Ollama /
  LM Studio / llamacpp peers.

## What remains here

Stdlib hardware size-class probe (no downloads, no vendor model names):

See [`chaos-engine/skills/local-coding-delegate/scripts/probe_hardware.py`](../../local-coding-delegate/scripts/probe_hardware.py).
Shim imports use [`hardware_probe.py`](../../local-coding-delegate/hardware_probe.py), which re-exports that home:

```text
python3 .chaos-engine/skills/local-coding-delegate/scripts/probe_hardware.py
```

Use it as an optional hint before choosing a checkpoint size class
(`small` / `medium` / `large` / `refuse`). Honest `refuse` is a valid
completion.

## Delegation rules (unchanged intent)

The most-intelligent or default capability model stays the decider. Local
loops are mechanical or default-capability labor only. Close that writer after
its PR exists. Do not use a local loop for review, the GitHub playbook, or
public-API changes unless both the probe and the task say it is enough.

Never `ft launch`. Never silently fall back to cloud OmniRoute; use OmniRoute
only when the adopter explicitly asked for that path.
