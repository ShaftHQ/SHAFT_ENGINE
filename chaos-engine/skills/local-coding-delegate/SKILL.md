---
name: local-coding-delegate
description: >-
  Optional local coding loop as a mechanical or default delegate. Use when the
  decider model wants a cheap local coder and a hardware probe says the host
  can run one.
license: MIT
---

# Local coding delegate

Optional. Not always-loaded. A user-configurable local coding loop that runs
as a **mechanical** or **default** delegate only.

The most-intelligent or default capability model stays the decider. Load this
skill when that decider judges a local loop would add value, then probe the
host before configuring anything.

## When it may run

- The adopter asked for a local loop, or the decider chose one for cheap,
  bounded, already-specified work.
- The [hardware probe](scripts/probe_hardware.py) recommends a size class
  other than `refuse`.
- The task is mechanical or default-capability labor, not review, not the
  GitHub playbook, and not a public-API change unless both the probe and the
  task say the local coder is enough.

Honest `refuse` is a valid completion. Do not download or configure a local
coder unless the adopter asked and the probe says the host can run a useful
one. Never invent a default vendor model name. Speak in capability levels and
size classes (`small`, `medium`, `large`, or `refuse`).

## Probe

Run the stdlib probe. It reports OS, RAM, and GPU memory when present, then
recommends a size class or refuses.

```text
python3 chaos-engine/skills/local-coding-delegate/scripts/probe_hardware.py
```

- `refuse` — do not install or invoke a local coder.
- `small` / `medium` / `large` — the host can run a useful coder in that
  class. The adopter chooses the vendor and the model. This skill does not.

## Bounds

The local loop never replaces independent review, never owns merge or GitHub
delivery, and never decides architecture. The host session may hand one
mechanical batch to a writer. Close that writer after its PR exists. If the
probe refuses, or the task outgrows the recommended class, keep the work on
the decider model.
