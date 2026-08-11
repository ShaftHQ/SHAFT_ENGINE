# Copilot task-playbook adapters

This directory exposes SHAFT's repository playbooks to GitHub Copilot without
creating a second policy owner. Every child `SKILL.md` is a thin adapter to a
canonical playbook under `.agents/skills/act-as-mohab/references/`.

- Start repository work from
  [act-as-mohab](../../.agents/skills/act-as-mohab/SKILL.md).
- Use the [canonical routing table](../../.agents/skills/act-as-mohab/references/routing.md)
  to select one playbook.
- Edit policy only at the canonical target; keep adapters as pointers.
- After changing an adapter, playbook, or route, run
  `py -3 scripts/ci/validate_agent_setup.py --skip-external` from the repository
  root.

This README is an operational map for the adapter directory, not public SHAFT
product documentation. User-facing agent skills live in the
[official guide](https://shafthq.github.io/docs/agentic/skills).
