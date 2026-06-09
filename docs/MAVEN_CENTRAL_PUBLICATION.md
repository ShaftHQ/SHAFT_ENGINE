# Maven Central publication contract

SHAFT publishes the complete reactor as one Maven Central deployment. Publication is intentionally driven from the root `shaft-parent` project so the Central Portal either receives the coherent artifact set below or the release workflow stops before creating any downstream release signal.

## Public artifact set

| Coordinate | Packaging | Additional required artifacts |
| --- | --- | --- |
| `io.github.shafthq:shaft-parent` | POM | signature |
| `io.github.shafthq:shaft-engine` | JAR | sources, JavaDocs, signatures |
| `io.github.shafthq:shaft-browserstack` | JAR | sources, JavaDocs, signatures |
| `io.github.shafthq:shaft-video` | JAR | sources, JavaDocs, signatures |
| `io.github.shafthq:shaft-visual` | JAR | sources, JavaDocs, signatures |
| `io.github.shafthq:shaft-bom` | POM | signature |
| `io.github.shafthq:SHAFT_ENGINE` | relocation POM | signature |

`report-aggregate` is build support only. It participates in the reactor to produce aggregate JaCoCo output but sets Maven deployment and GPG signing skip properties so it cannot enter a Central deployment.

The BOM may manage only artifacts in the public set. `scripts/ci/validate_maven_publication.py` rejects unpublished BOM references, missing publication plugins, an unsafe release-workflow order, or an incomplete build output set.

## Disposable dry run

A normal unsigned local validation does not contact Maven Central:

```bash
python3 scripts/ci/validate_reactor_versions.py
python3 scripts/ci/validate_maven_publication.py
mvn --batch-mode clean install -DskipTests -Dgpg.skip
python3 scripts/ci/validate_maven_publication.py --check-build-outputs
mvn --batch-mode -f tools/modularization/consumer-fixtures/combined-modules/pom.xml \
  verify -Dshaft.version="$(mvn -f pom.xml help:evaluate -Dexpression=project.version -q -DforceStdout)"
python3 scripts/ci/validate_maven_publication.py \
  --create-bundle target/publication-dry-run/central-bundle.zip
```

The combined consumer imports `shaft-bom`, resolves all four JAR modules, enforces dependency convergence, detects duplicate classes outside the BrowserStack SDK's documented shaded JAR, and writes a CycloneDX SBOM.

For a signed staging rehearsal, import a disposable GPG key into an isolated `GNUPGHOME`, run the reactor without `-Dgpg.skip`, and add `--require-signatures` to both validator commands. Never use release credentials for a local rehearsal and never run `deploy` as part of the dry run.

## Atomic release order

`.github/workflows/mavenCentral_cd.yml` performs these irreversible actions in order:

1. Read the version from the root parent POM and reject reactor version drift.
2. Validate configuration, build all publication artifacts, and run the combined consumer checks.
3. Sign and deploy the complete reactor through the Central Publishing Maven Plugin, waiting for publication success.
4. Create the GitHub tag and release.
5. Dispatch the guide update and announce the release on Slack.

This ordering prevents a GitHub release, guide update, or Slack announcement from claiming availability when Central publication failed.

## Failure and rollback behavior

- **Before Central succeeds:** fix the validation, signing, metadata, or Central error and rerun. No GitHub release or announcement has been created.
- **After Central succeeds but before GitHub release creation:** Maven Central releases are immutable. Do not overwrite or delete the published version. A maintainer may rerun the workflow to create the missing GitHub release if the tag does not exist, or create it manually using the exact published version.
- **After GitHub release creation but before dispatch/Slack:** rerun or manually perform only the missing notification. Do not republish the Maven version.
- **Incorrect published content:** publish a new release version with the correction. Document the affected version; Maven Central does not support replacing immutable artifacts.
- **Partial local output:** delete generated `target/` directories and repeat the unsigned dry run. Generated bundles and build outputs must never be committed.
