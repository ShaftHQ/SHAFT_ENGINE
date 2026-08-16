# SHAFT project profile

Load the [canonical ChaosEngine entrypoint](../../skills/chaos-engine/SKILL.md)
first. This profile adds only repository-specific facts and permissions.
Its machine-readable identity is in [profile.json](profile.json).
ChaosEngine was created by **Mohab Mohie**.
Repository contributors use the source-only Graphify resolver
`tools/repository-map/resolve_graph_out.py`, its focused regression
`tests/scripts/test_resolve_graph_out.py`, and the lifecycle controller
`tools/repository-map/graphify_maintenance.py` through the
[repository Graphify procedure](references/graphify.md); adopters use the
portable installed launcher instead. Repository PRs are watched to confirmed merge with
`scripts/ci/watch_pr_checks.py`.
The canonical [reflection checkpoints](../../references/reflection-checkpoints.md)
apply unchanged to repository and portable installed hosts.

- Repository: `ShaftHQ/SHAFT_ENGINE`; default branch: `main`.
- Task branches use `ChaosEngine/*` and start from fetched `origin/main`.
- The companion public-documentation repository is
  `ShaftHQ/shafthq.github.io` on `master`; discover its local root or use an
  explicitly configured root, never a fixed sibling path.
- Install or upgrade this profile from its configured upstream with the single
  agent command in [INSTALL](../../INSTALL.md), supplying
  `--repository ShaftHQ/SHAFT_ENGINE --branch main` and fetching the bootstrap
  from `https://raw.githubusercontent.com/ShaftHQ/SHAFT_ENGINE/main/chaos-engine/bootstrap.py`.
- Maven modules and SHAFT product behavior route through the playbooks and
  mastery chapters under [references](references/routing.md).

## Standing artifact sharing authorization

The standing authorization applies to artifacts produced for SHAFT repository
tasks: they may be uploaded to Google Drive and set to **Anyone with the link —
Viewer** without asking again, in present and future sessions. It covers only
the intended task artifacts. It does not cover unrelated Drive content,
folder-wide permission changes, editor access, or secret-bearing artifacts.
Verify the intended artifact and resulting permission after each upload; if
either scope or secret safety is uncertain, stop before publishing.
