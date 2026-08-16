# ChaosEngine repository map

This page inventories the harness. It does not define behavior.

## Sources of truth

- Workflow: `chaos-engine/skills/chaos-engine/SKILL.md`
- SHAFT facts: `chaos-engine/profiles/shaft/entrypoint.md`
- Host entrypoint: `.agents/skills/chaos-engine/SKILL.md`
- Compatibility alias: `.agents/skills/act-as-mohab/SKILL.md`
- Topic details: `chaos-engine/references/`
- SHAFT playbooks: `chaos-engine/profiles/shaft/references/`

Host adapters point to these files and add no policy. Topic references cannot override the canonical
workflow or introduce mandatory gates.

## Runtime surfaces

| Surface | Owner | Purpose |
| --- | --- | --- |
| Installer and host rendering | `chaos-engine/install.py`, `chaos-engine/hosts.py` | Portable project and user harness deployment |
| Component contracts | `chaos-engine/component-contracts.json` | Machine-readable ownership, dependencies, probes, and fallbacks |
| Diagnostic guard | `scripts/agents/guard.py`, `chaos-engine/hooks/guard.py` | Explicit compatibility diagnostics only; never lifecycle enforcement |
| Host hook config | `.codex/hooks.json`, `.claude/settings.json`, `plugins/chaos-engine/hooks/hooks.json` | Empty by design |
| Repository operations | `scripts/agents/` | Explicit issue, PR, delivery, planning, and maintenance commands |
| Memory | `.memory/` | Repository-owned durable records |
| MemPalace | `mempalace.yaml` | Local semantic retrieval configuration |
| Graphify | `tools/repository-map/` | Shared repository structure cache and resolver |
| Host sync | `scripts/agents/sync_user_harness.py` | Explicit user-harness drift and deployment |

## Invariants

- Local hooks do not intercept lifecycle events.
- Host permissions and repository rulesets enforce safety; prose and diagnostics do not.
- Generated indexes, runtime state, secrets, caches, reports, and binaries stay out of git.
- Current files outrank retrieval output.
- Shared retrieval resolves through repository identity and the primary checkout.
- Store failure degrades ordinary work rather than blocking it.
- Operational paths in tracked portable guidance remain relative.
- Capability levels are named only most intelligent, default, and mechanical.

Validation lives under `tests/scripts/` and `scripts/ci/validate_agent_setup.py`. Run only the
scope selected by the user or canonical workflow.
