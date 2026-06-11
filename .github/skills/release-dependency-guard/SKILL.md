---
name: release-dependency-guard
description: Prepare or review a SHAFT release by checking reactor versions, metadata, examples, consumer fixtures, stable dependency updates, and Maven Central publication safeguards.
---

# Release And Dependency Guard

Use only for release preparation, release review, or dependency-currency work.
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
4. Update all example and consumer-fixture `<shaft.version>` values found by:

   ```bash
   rg -l "<shaft.version>" -g "pom.xml"
   ```

5. Run the repository's dependency/plugin/property update checks. Accept stable
   updates only after compatibility review; do not treat alpha, beta, RC,
   milestone, or snapshot releases as upgrades.
6. Validate release metadata and publication rules with existing scripts,
   including Maven Central POM/JAR/classifier/signature and canonical,
   combined-module, and legacy-relocation consumer checks.
7. Run one compile/package pass after metadata is aligned. Do not rerun broad
   product tests for metadata-only changes unless source/test behavior changed.
8. Verify the release version is newer than any immutable Maven Central
   coordinate it replaces. Merging to `main` triggers publication; local
   deploy, signing, and `scm-publish` are prohibited.

## Output

List blockers first, then version alignment, stable updates considered,
commands and results, source checks for volatile versions, and remaining
publication risk.
