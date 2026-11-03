# SHAFT IntelliJ Marketplace ZIP → cold IDEA e2e

- Date: 2026-11-03 (Africa/Cairo)
- IDE: IntelliJ IDEA Community 2024.3.7.1 (ideaIC-2024.3.7.1)
- Plugin ZIP: shaft-intellij-10.4.20261102.zip (built via `./gradlew buildPlugin`)
- Install method: unpacked ZIP into `idea.plugins.path` (Marketplace-equivalent sideload)
- Launch: `xvfb-run` headless display 1280x800

## Results
1. **Install + load:** IDEA log shows `Loaded custom plugins: SHAFT (10.4.20261102)` (see e2e-plugin-loaded.log).
2. **Core functionality (from installed Marketplace JAR):** target matrix VM args, BOM detect/bump, failure-noise collapse, page-object generation — `CORE_SMOKE_OK` (see core-smoke.log).
3. **Registered surface:** tool window `SHAFT` + 11 actions (see registered-actions.txt).

## Artifacts
- shaft-intellij-marketplace.zip
- e2e-plugin-loaded.log / core-smoke.log / registered-actions.txt / E2E-REPORT.md
