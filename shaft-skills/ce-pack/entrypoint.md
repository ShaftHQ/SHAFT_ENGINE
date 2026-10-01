# SHAFT project profile

Load the canonical ChaosEngine entrypoint (`.chaos-engine/skills/chaos-engine/SKILL.md`)
first. This profile adds only repository-specific facts and permissions.
Its machine-readable identity is in [profile.json](profile.json).
ChaosEngine was created by **Mohab Mohie**.
Knowledge stores: `scripts/agents/knowledge_stores.py` (`status`, `search`;
MemPalace path via `tools/repository-map/resolve_mempalace.py`;
Graphify procedure in [graphify](references/graphify.md)). Watch PRs with one
blocking `scripts/ci/watch_pr_checks.py` call per push.
The canonical reflection checkpoints (`.chaos-engine/references/reflection-checkpoints.md`)
apply unchanged to repository and portable installed hosts.

- Repository: `ShaftHQ/SHAFT_ENGINE`; default branch: `main`.
- Personal authorizations (commit-signing key, owner email attribution,
  artifact sharing) live in user-level config, never in this repository:
  `~/.config/chaos-engine/authorizations.md` (Windows
  `%APPDATA%\chaos-engine\authorizations.md`). Load it when present; without
  it, ask before signing as, attributing to, or sharing for the owner.
- Task branches use `ChaosEngine/*` and start from fetched `origin/main`.
- Agents never manually start, rerun, or replace the `E2E Tests` or
  `Local E2E Tests` workflows. Smallest local proof and exact-head PR checks
  gate delivery; scheduled nightly workflows own E2E execution.
- The companion public-documentation repository is
  `ShaftHQ/shafthq.github.io` on `master`; discover its local root or use an
  explicitly configured root, never a fixed sibling path. Every user-facing
  SHAFT behavior change opens a companion PR on that `master` branch in the
  same delivery. That companion PR must include a description of the change,
  screenshots where a human sees UI, human-facing instructions, and
  AI-supported details (locator policy, replay-proven snippets, properties,
  exact commands).
- Install or upgrade (`chaos-engine/INSTALL.md` in this repository): `irm https://raw.githubusercontent.com/ShaftHQ/SHAFT_ENGINE/main/chaos-engine/install.ps1 | iex`.
- Maven modules and SHAFT product behavior route through the playbooks and
  mastery chapters under [references](references/routing.md).
- Cheap, bounded, already-specified local coding work uses the
  [workstation local coding agent](references/playbooks/workstation-local-coding-agent.md).

## Multi-ticket assignment orchestration

This specializes the portable solo-or-orchestrate rule: two or more SHAFT
issues in one owner request **are** orchestration. Do not wait for the owner
to say "orchestrate".

- Load every assigned ticket (and linked/deferred children) before grouping or dispatching.
- Group related work to the fewest PRs that still keep one problem per issue
  (`Closes #N` per completed subtask).
- Main session orchestrates: status, owner commands, review, merge. It does
  not implement product or guidance chunks.
- Remaining chunks run one at a time, ordered by dependency then priority.
- After a chunk's PR is merged, destroy that writer and start the next from a
  fresh `ChaosEngine/*` branch off fetched `origin/main`.
