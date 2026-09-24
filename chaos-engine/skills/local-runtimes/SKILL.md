---
name: local-runtimes
description: >-
  Use when choosing an optional local inference runtime for a narrow mechanical or
  offline job. One table routes to OmniRoute, FreeToken, Colibri, a loopback
  server, or the hardware probe.
license: MIT
---

# Local runtimes

Cloud implementers write code directly (#6171). A local runtime is optional
and only pays for narrow mechanical or offline jobs; never let it review its
own diff. Pick one row, then open only that route.

| Need | Route | Open |
| --- | --- | --- |
| Which model size fits this machine | hardware probe | [local-coding-delegate](../local-coding-delegate/SKILL.md) |
| Already READY loopback server (Ollama, LM Studio, llamacpp) | loopback | [local-openai-compat](../local-openai-compat/SKILL.md) |
| Standalone FreeToken process | FreeToken | [freetoken](../freetoken/SKILL.md) |
| Frontier MoE through disk/RAM/VRAM tiers | Colibri | [colibri](../colibri/SKILL.md) |
| OmniRoute runner with delegate continuity | OmniRoute | [omniroute](../omniroute/SKILL.md) |
| Local agents (OpenCode / OSS agency) | agency | [local-agency](../local-agency/SKILL.md) |

Rules:

- A runtime is used only when its READY probe passes; otherwise stay on the
  cloud implementer and record `local: skipped(<reason>)` once.
- One-shot jobs only: one short spec in, one digest out. No parent relay.
- Avoided-spend metering and USD/EGP lines are required only when a local
  inference channel was actually used
  ([process owner](../../references/process-owner-scrum-master.md)).
- Descriptions, paths and host exposure live in the
  [catalog](../../references/catalog.md).
