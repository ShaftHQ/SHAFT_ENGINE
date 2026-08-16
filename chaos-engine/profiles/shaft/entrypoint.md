# SHAFT project profile

Load the [canonical ChaosEngine entrypoint](../../skills/chaos-engine/SKILL.md) first. This file is
the single source of SHAFT-specific facts; it does not redefine the portable workflow.

- Repository: `ShaftHQ/SHAFT_ENGINE`; default branch: `main`.
- SHAFT is a Maven Java automation framework. Core code is in `shaft-engine/`, optional modules
  use `shaft-*`, and CI tooling is in `scripts/ci/`.
- The companion documentation repository is `ShaftHQ/shafthq.github.io` on `master`. Discover
  its checkout or use an explicit root; never assume a fixed sibling path.
- Product and module work routes through [references/routing.md](references/routing.md).
- Repository Graphify maintenance uses `tools/repository-map/graphify_maintenance.py`; consumers
  resolve the primary cache with `tools/repository-map/resolve_graph_out.py`. Generated
  `graphify-out/` and MemPalace indexes are never committed.
- Keep Maven work scoped and headless. Include `-Dallure.automaticallyOpen=false` and disable GUI
  Lighthouse behavior.
- Do not launch GUI handlers or applications. Ask before servers, watchers, browser capture,
  emulators, or external/cloud suites.
- Do not commit generated reports, binaries, caches, `target/`, secrets, or user-host state.
- User-visible engine behavior updates its companion documentation when that documentation changes.
- The requested delivery endpoint and explicit owner authority decide branch, PR, merge, cleanup,
  validation, and review shape.

## Standing artifact sharing authorization

Task artifacts may be uploaded to Google Drive as **Anyone with the link - Viewer** without asking
again. This excludes unrelated content, broad folder permissions, editor access, and secret-bearing
artifacts. Verify the intended artifact and resulting permission after upload.
