# Router Heal surface

Always reachable by **file path** even when the marketplace plugin is absent.
The core [`chaos-engine` skill](../skills/chaos-engine/SKILL.md) Route table
points here; do not require plugin activation to heal.

## Prefer (zero-LLM)

1. Doctor: `python3 .chaos-engine/install.py doctor --project .`
2. Fix-next only: `… doctor --project . --fix-next-only`
3. Component repair (no full wipe):  
   `python3 .chaos-engine/install.py repair --project . --component <id>`  
   Components: `plugins`, `hosts`, `core`, `headroom`, `mempalace`, `graphify`,
   `memory`, `hooks`, `mcps`, `skills`, `roles`, `tools`
4. Official install one-liner from [`INSTALL.md`](../INSTALL.md) for missing
   core, wiped runtime, or multi-component drift
5. Installer rewrite contract:
   [`installer-program.md`](installer-program.md) (deterministic merge,
   success-with-agent-prompt, agentic one-liner) and
   [`installer-program-executable-spec.md`](installer-program-executable-spec.md)

## When marketplace plugin is absent

- Read this file and [`INSTALL.md`](../INSTALL.md) from the portable tree or
  `.chaos-engine/` copy — path locators stay valid without Codex/Claude plugin
  registration. For first install, upgrade, and conflict merge duties, also
  follow [`installer-program.md`](installer-program.md).
- SessionStart still injects the Heal locator under `SESSION_START_MAX_BYTES`.

## Out of scope

- Auto-merge skill mutations
- Pretending exit-2 hard-blocks on GAP-EXIT2 hosts (see host-parity checklist)
