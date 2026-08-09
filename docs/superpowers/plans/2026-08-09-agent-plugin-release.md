# Agent Plugin Release Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:executing-plans task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Release the portable Agent Plugin packages safely now and from every future normal SHAFT release.

**Architecture:** A tracked manifest owns each package SemVer. Existing assemblers take a declared version, while a new standard-library release script builds validated ZIP archives and SHA-256 checksums. The current normal release pipeline produces and attaches those assets; the one-time interim distribution is only an annotated Git tag.

**Tech Stack:** Python 3 standard library, unittest, GitHub Actions, Agent Plugins v1.0.0.

## Global Constraints

- Package names: `act-as-mohab`, `shaft-skills`; versions are independent stable SemVer.
- Package roots contain license and changelog, but no secrets, user-local state, host trust, or absolute paths.
- `agent-plugins-v1.0.0` is created only after the PR merges and never becomes a GitHub Release.
- Future assets are attached only after existing Maven and installer release gates pass.

### Task 1: Release metadata and versioned package assembly

**Files:**
- Create: `agent-plugins/release.json`, `agent-plugins/CHANGELOG.md`
- Modify: `scripts/ci/assemble_act_as_mohab_plugin.py`, `scripts/ci/assemble_shaft_skills_plugin.py`
- Modify: `tests/scripts/test_assemble_act_as_mohab_plugin.py`, `tests/scripts/test_assemble_shaft_skills_plugin.py`

- [ ] Write tests that call `assemble(ROOT, output, "1.2.3")` and assert the root, Claude, and Codex manifests all use `1.2.3` and the package includes `LICENSE` and `CHANGELOG.md`.
- [ ] Run RED: `py -3 -m unittest tests.scripts.test_assemble_act_as_mohab_plugin tests.scripts.test_assemble_shaft_skills_plugin -v`.
- [ ] Add a strict manifest requiring exactly both package names and stable SemVer; make the assemblers consume the declared version and copy only tracked release files.
- [ ] Run GREEN: `py -3 -m unittest tests.scripts.test_assemble_act_as_mohab_plugin tests.scripts.test_assemble_shaft_skills_plugin tests.scripts.test_validate_agent_plugins -v`.
- [ ] Commit: `feat(ci): version portable plugin assemblies (#4576)`.

### Task 2: Deterministic release artifacts

**Files:**
- Create: `scripts/ci/agent_plugin_release.py`, `tests/scripts/test_agent_plugin_release.py`

- [ ] Write a failing test for `build_release_artifacts(ROOT, output)`: it must emit exactly two versioned ZIPs and two matching `.sha256` files; both output sets must have byte-identical names and content.
- [ ] Run RED: `py -3 -m unittest tests.scripts.test_agent_plugin_release.AgentPluginReleaseTest.test_build_release_artifacts_is_deterministic -v`.
- [ ] Load release metadata, call both assemblers, validate each package, write sorted fixed-timestamp ZIP entries, and emit `<sha256>  <archive-name>` checksum files.
- [ ] Run GREEN: `py -3 -m unittest tests.scripts.test_agent_plugin_release tests.scripts.test_assemble_act_as_mohab_plugin tests.scripts.test_assemble_shaft_skills_plugin -v`.
- [ ] Commit: `feat(ci): build portable plugin release assets (#4576)`.

### Task 3: Normal-release workflow integration

**Files:**
- Modify: `.github/workflows/mavenCentral_cd.yml`, `.github/workflows/pr-gate.yml`
- Modify: `tests/scripts/test_agent_plugin_release.py`

- [ ] Write failing static assertions that the normal release workflow marks `agent-plugins/**` and the release scripts as release inputs, builds `agent-plugin-release-assets`, uploads it, downloads it in `announce_release`, and attaches ZIP/checksum assets through the existing release action.
- [ ] Run RED: `py -3 -m unittest tests.scripts.test_agent_plugin_release.AgentPluginReleaseTest.test_normal_release_attaches_plugin_assets -v`.
- [ ] Add only these steps to the existing release path. Never add a new `release:` event producer or an independent deployment workflow.
- [ ] Run GREEN: `py -3 -m unittest tests.scripts.test_agent_plugin_release tests.scripts.test_validate_workflow_timeouts -v`.
- [ ] Commit: `ci: release portable agent plugin assets (#4576)`.

### Task 4: Compatibility, interim tag, and final delivery

**Files:**
- Create: `agent-plugins/COMPATIBILITY.md`
- Modify: `README.md`, `tests/scripts/test_agent_plugin_release.py`

- [ ] Write failing documentation tests requiring the immutable `agent-plugins-v1.0.0` pin, explicit validation/discovery/install/real-load columns, and a clearly labeled Claude waiver rather than a false successful load.
- [ ] Run RED: `py -3 -m unittest tests.scripts.test_agent_plugin_release.AgentPluginReleaseTest.test_compatibility_contract_marks_waived_runtime_evidence -v`.
- [ ] Add install, checksum, upgrade, rollback, and compatibility guidance. Run GREEN: `py -3 -m unittest tests.scripts.test_agent_plugin_release -v; py -3 scripts/ci/validate_agent_setup.py --skip-external`.
- [ ] Merge the PR, then run `git tag -a agent-plugins-v1.0.0 <merged-sha> -m "Agent Plugins 1.0.0"` and push it. Verify `gh release view agent-plugins-v1.0.0 --repo ShaftHQ/SHAFT_ENGINE` reports no release.
- [ ] Update #4576 and open the companion user-guide PR after the next normal SHAFT release has attached package assets.
