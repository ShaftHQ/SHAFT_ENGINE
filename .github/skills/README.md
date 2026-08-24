# Copilot task-playbook adapters

This directory exposes SHAFT's repository playbooks to GitHub Copilot without
creating a second policy owner. Every child `SKILL.md` is a thin adapter to a
canonical playbook under `chaos-engine/profiles/shaft/references/`.

- Start repository work from
  [ChaosEngine](../../.agents/skills/chaos-engine/SKILL.md).
- Use the [canonical routing table](../../chaos-engine/profiles/shaft/references/routing.md)
  to select one playbook.
- Edit policy only at the canonical target; keep adapters as pointers.
- After changing an adapter, playbook, or route, run
  `py -3 scripts/ci/validate_agent_setup.py --skip-external` from the repository
  root.

This README is an operational map for the adapter directory, not public SHAFT
product documentation. User-facing agent skills live in the
[official guide](https://shafthq.github.io/docs/agentic/skills).
