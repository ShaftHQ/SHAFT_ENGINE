# Release And Dependency Guard

Use only for release preparation, release review, release notes, or
dependency-currency work.
Never run deployment or publication commands locally.

## Workflow

1. Inspect recent merged release PRs and executable workflows before relying on
   prose. Compute versions as `{major}.{quarter}.{YYYYMMDD}`.
2. Keep the root project version, every reactor child parent, and SHAFT
   inter-module dependency aligned. Run:

   ```bash
   python3 scripts/ci/validate_reactor_versions.py
   ```

3. Update `shaft-engine/src/main/java/com/shaft/properties/internal/Internal.java`
   `shaftEngineVersion`. Verify `allure3Version` against the stable npm package
   and `nodeLtsVersion` against the current LTS patch using primary sources.
   Verify every other version default in `Internal.java` against its source
   registry when it changes. Keep non-Maven static versions in `Internal.java`;
   move stray version literals there.
4. Update all example and consumer-fixture `<shaft.version>` values found by:

   ```bash
   rg -l "<shaft.version>" -g "pom.xml"
   ```

5. Check Maven dependency/plugin/property updates. Accept stable ones after
   compatibility review; do not treat alpha, beta, RC,
   milestone, or snapshot releases as upgrades.
6. Validate release metadata and publication rules with existing scripts,
   including Maven Central POM/JAR/classifier/signature, the IntelliJ IDEA
   plugin release candidate, and canonical, combined-module, and
   legacy-relocation consumer checks.
7. Run one compile/package pass after metadata is aligned. Do not rerun broad
   product tests for metadata-only changes unless source/test behavior changed.
8. Verify the release version is newer than any immutable Maven Central
   coordinate it replaces. Merging to `main` triggers publication; local
   deploy, signing, and `scm-publish` are prohibited.

## Release notes (#6232)

Release bodies come from `scripts/ci/render_release_notes.py` (run by
`announce_release`) filling `.github/RELEASE_BODY_TEMPLATE.md`; do not switch
back to raw GitHub-generated notes or hand-write bodies.

- Format: version, a 1-3 line summary, the Maven/Gradle snippet, then only
  non-empty sections in this order: Breaking changes and upgrade notes, New
  features, Fixes, Performance, Deprecations, notable Dependency upgrades.
  One cleaned `- Title (#PR)` line per change; no authors or PR URLs. All
  internal work goes in one `<details>` block; the full changelog link is last.
- Internal: `[CE]`; `ci`, `test`, `docs`, `chore`, `build`, or `refactor`
  prefixes; ChaosEngine/installer/harness scopes; the labels
  `subsystem:agent-harness`, `subsystem:repository-tooling`, `documentation`,
  `tests`, `github-actions`, or `maintenance`; or no files under shipped
  module sources. Bumps collapse to "N dependency updates" unless labeled
  `security` or they are Selenium/Appium/Playwright majors.
- Labels: exactly one of `breaking-change`, `enhancement`, `bug`,
  `skip-release-notes`; optional `performance`, `deprecation`, `regression`.
  PR titles become release lines, so write them for SHAFT users.
- Keep the renderer, template, and `scripts/ci/validate_release_notes.py`
  label sets in sync; `Release-note governance` tests that parity.
- Slack and reconcile announcements reuse that summary line plus a link via
  `slack_payload` (#6241); never promise sections the notes do not have.
- Never edit a published release body. Preview with
  `python3 scripts/ci/render_release_notes.py --version <tag> --previous-tag <prev> --head <tag> --output <file>`.

## Output

List blockers first, then version alignment, stable updates considered,
commands and results, source checks for volatile versions, and remaining
publication risk.
