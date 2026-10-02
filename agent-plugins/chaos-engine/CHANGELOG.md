# Changelog

## Unreleased

- Fix: a fresh-clone install no longer fails with CE-INSTALL-FAILED when the
  first MemPalace mine outlasts the 900 s setup budget. Install creates the
  exact palace synchronously, then runs the incremental mine in a detached
  background runner (`mempalace-index.json` beside the palace). Doctor shows
  `[info] mempalace/index` with a fix-next line while it is running,
  interrupted, or failed, and the next install resumes an unfinished mine. Set
  `CHAOS_ENGINE_MEMPALACE_MINE=foreground` to keep the old synchronous mine.
- Breaking (hard cut, CE-10): the repository's project profile moves out of
  the core to `shaft-skills/ce-pack/` and installs under
  `.chaos-engine/packs/shaft/`. The old `.chaos-engine/profiles/shaft/` layout
  is no longer read and no shim is kept; install and doctor print the migration
  notice until a reinstall replaces it. `distributions.json` keeps only
  `portable`; the pack's `profile.json` declares its own distribution.
- Breaking (CE-11): managed JDK, managed Maven, and the Maven Tools MCP runtime
  move from `hosts.py`/`install.py` into the java pack (`packs/java/`), bound
  into the controllers by `pack_binding.py`.
  `repair --component maven-tools-mcp` is unchanged.

## 10.3.20260930 - 2026-09-30

- Align the portable plugin version with SHAFT Engine release 10.3.20260930.

## 10.3.20260911 - 2026-09-11

- Breaking: the router `SKILL.md` becomes a core card; always-composed
  sections move to `references/router-contract.md` and role adapters load a
  2 KiB delegate card (#6176).
- Add the design-turn contract and store-citation read gate (#6091).
- Bundle `status_lease.py` in `bin/chaos-engine.pyz` so the `watch_pr_checks`
  MCP tool starts (#6202).

## 10.3.20260824 - 2026-08-24

- Breaking: rename the portable plugin, zipapp, and CLI from `act-as-mohab`
  to `chaos-engine`. Remove the discoverable compatibility-alias skill.
- Isolate delivery and fresh-base fixtures without weakening production guards.
- Add bounded OmniRoute delegate continuity with capability enforcement, private
  per-candidate invocation selection, immutable deadlines, and redacted state.

## 10.3.20260820 - 2026-08-20

- Add immutable installer generations, repair, rollback, managed dependency
  provisioning, and portable lifecycle kernel support across five hosts.
- Replace incremental harness enforcement with fast changed-surface PR checks
  and scheduled/manual exhaustive acceptance.

## 10.3.20260817 - 2026-08-17

- Align the bundled package version with the canonical SHAFT engine release.
- Restore the last fully validated portable harness contract after the newer
  harness revision failed the repository's cross-platform acceptance gate.

## 10.3.20260809 - 2026-08-09

- Breaking: expose only `act-as-mohab` as a discoverable skill; consultation
  and retrieval are now internal lifecycle references of that entrypoint.
- Add the bundled mandatory executable-planning contract and repository-safe
  plan-artifact routing to those internal lifecycle references.
- Add a deterministic stdlib Python runtime with repository-context,
  repository-aware PR watching, checkpoint-status, and MCP commands.
- Default repository operations to the caller's cwd and make bare numeric PR
  inference visible on stderr without contaminating machine-readable stdout.

## 1.0.0 - 2026-08-09

- First portable `act-as-mohab` Agent Plugin package.
