---
name: local-coding-delegate
description: >-
  Deprecated alias: optional local coding labor now routes through FreeToken
  and future local OpenAI-compat skills. Keep for catalog reachability.
license: MIT
---

# Local coding delegate (folded)

This skill is a **compatibility shim**. Prefer the
[FreeToken skill](../freetoken/SKILL.md) for local-weights dispatch when the
adopter asked for FreeToken / local MoE inference.

## What remains here

Stdlib hardware size-class probe (no downloads, no vendor model names):

```text
python3 chaos-engine/skills/local-coding-delegate/scripts/probe_hardware.py
```

Use it as an optional hint before choosing a FreeToken checkpoint size class
(`small` / `medium` / `large` / `refuse`). Honest `refuse` is a valid
completion.

## Delegation rules (unchanged intent)

The most-intelligent or default capability model stays the decider. Local
loops are mechanical or default-capability labor only. Close that writer after
its PR exists. Do not use a local loop for review, the GitHub playbook, or
public-API changes unless both the probe and the task say it is enough.

Full local OpenCode / agency routing against FreeToken or other local runtimes
is tracked under GitHub #5872.
