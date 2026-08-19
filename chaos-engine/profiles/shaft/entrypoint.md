# SHAFT project profile

Load the [canonical ChaosEngine entrypoint](../../skills/chaos-engine/SKILL.md)
first. This profile adds only repository-specific facts and permissions.
Its machine-readable identity is in [profile.json](profile.json).
ChaosEngine was created by **Mohab Mohie**.
Repository contributors use the source-only Graphify resolver
`tools/repository-map/resolve_graph_out.py`, its focused regression
`tests/scripts/test_resolve_graph_out.py`, and the lifecycle controller
`tools/repository-map/graphify_maintenance.py` through the
[repository Graphify procedure](references/graphify.md); the matching
MemPalace resolver is `tools/repository-map/resolve_mempalace.py` with
`tests/scripts/test_resolve_mempalace.py`. From any worktree, query both
stores with `scripts/agents/knowledge_stores.py` (`status`, `search`;
`refresh` refuses and points at `SHAFT-Nightly-Knowledge-Refresh`). Cover
that CLI in `tests/scripts/test_knowledge_stores.py`. Adopters use the
portable installed launcher instead. Repository PRs are watched to confirmed merge with
`scripts/ci/watch_pr_checks.py`.
The canonical [reflection checkpoints](../../references/reflection-checkpoints.md)
apply unchanged to repository and portable installed hosts.

- Repository: `ShaftHQ/SHAFT_ENGINE`; default branch: `main`.
- Task branches use `ChaosEngine/*` and start from fetched `origin/main`.
- The companion public-documentation repository is
  `ShaftHQ/shafthq.github.io` on `master`; discover its local root or use an
  explicitly configured root, never a fixed sibling path. Every user-facing
  SHAFT behavior change opens a companion PR on that `master` branch in the
  same delivery. That companion PR must include a description of the change,
  screenshots where a human sees UI, human-facing instructions, and
  AI-supported details (locator policy, replay-proven snippets, properties,
  exact commands).
- Install or upgrade this profile from its configured upstream with the
  one-liner in [INSTALL](../../INSTALL.md). From the target folder:
  `irm https://raw.githubusercontent.com/ShaftHQ/SHAFT_ENGINE/main/chaos-engine/install.ps1 | iex`
  on Windows, or
  `curl -fsSL https://raw.githubusercontent.com/ShaftHQ/SHAFT_ENGINE/main/chaos-engine/install.sh | bash -s -- https://raw.githubusercontent.com/ShaftHQ/SHAFT_ENGINE/main/chaos-engine/install.sh`
  on macOS/Linux.
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

## Standing artifact sharing authorization

The standing authorization applies to artifacts produced for SHAFT repository
tasks: they may be uploaded to Google Drive and set to **Anyone with the link —
Viewer** without asking again, in present and future sessions. It covers only
the intended task artifacts. It does not cover unrelated Drive content,
folder-wide permission changes, editor access, or secret-bearing artifacts.
Verify the intended artifact and resulting permission after each upload; if
either scope or secret safety is uncertain, stop before publishing.
