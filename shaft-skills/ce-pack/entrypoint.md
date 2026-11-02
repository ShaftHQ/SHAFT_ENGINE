# SHAFT project profile

Load the canonical ChaosEngine entrypoint (`.chaos-engine/skills/chaos-engine/SKILL.md`)
first. This profile adds only repository-specific facts and permissions.
Machine-readable identity: [profile.json](profile.json).
ChaosEngine was created by **Mohab Mohie**.
Knowledge stores: `scripts/agents/knowledge_stores.py` (`status`, `search`;
MemPalace via `tools/repository-map/resolve_mempalace.py`;
Graphify in [graphify](references/graphify.md)). Watch PRs with one blocking
`scripts/ci/watch_pr_checks.py` call per push.
Canonical reflection checkpoints (`.chaos-engine/references/reflection-checkpoints.md`)
apply on repository and portable hosts.

- Repository: `ShaftHQ/SHAFT_ENGINE`; default branch: `main`.
- Personal authorizations (commit-signing key, owner email attribution,
  artifact sharing) live in user-level config, never in this repository:
  `~/.config/chaos-engine/authorizations.md` (Windows
  `%APPDATA%\chaos-engine\authorizations.md`). Load when present; else ask
  before signing as, attributing to, or sharing for the owner.
- Task branches use `ChaosEngine/*` from fetched `origin/main`.
- SHAFT and ChaosEngine tasks complete only when the pull request is merged.
- Agents never manually start, rerun, or replace `E2E Tests` or
  `Local E2E Tests`. Smallest local proof and exact-head PR checks gate
  delivery; scheduled nightlies own E2E.
- Companion docs repo: `ShaftHQ/shafthq.github.io` on `master` (discover local
  root; never a fixed sibling path). Every user-facing SHAFT behavior change
  opens a companion PR there in the same delivery (description, UI screenshots,
  human instructions, AI-supported locator/replay/properties/commands).
- Install/upgrade (`chaos-engine/INSTALL.md`): `$env:CHAOS_ENGINE_ADDONS='shaft-core-developers'; irm https://raw.githubusercontent.com/ShaftHQ/SHAFT_ENGINE/main/chaos-engine/install.ps1 | iex` (upgrades keep the add-on).
- Maven modules and SHAFT product behavior: [references](references/routing.md).
- Cheap local coding: [workstation local coding agent](references/playbooks/workstation-local-coding-agent.md).

## Multi-ticket assignment orchestration

Specializes the portable solo-or-orchestrate rule: two or more SHAFT issues in
one owner request **are** orchestration (do not wait for "orchestrate").

- Load every assigned ticket (and linked/deferred children) before grouping.
- Fewest PRs that still keep one problem per issue (`Closes #N` per subtask).
- Main session orchestrates status/commands/review/merge; it does not implement.
- Chunks run one at a time (dependency then priority). After merge, destroy that
  writer and start the next from a fresh `ChaosEngine/*` off fetched `origin/main`.
