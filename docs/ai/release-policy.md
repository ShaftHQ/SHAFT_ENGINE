# Release Policy

## Current Release Mechanism
SHAFT publishes a signed Maven artifact and GitHub release. `.github/workflows/mavenCentral_cd.yml` runs on pushes to `main`, reads the version from `pom.xml`, creates a same-version GitHub release, notifies the user-guide repository, and runs Maven deploy with repository and GPG credentials. JavaDocs and sample-version synchronization have separate workflows.

Normal AI tasks must not deploy, publish JavaDocs, create releases, rotate credentials, or change release versions unless explicitly requested.

## Risk Levels
- **Low:** documentation-only changes, comments, or non-executable examples with no public contract impact.
- **Medium:** isolated internal implementation/test/property change with no public signature or integration impact.
- **High:** public API/facade changes, driver lifecycle, reporting/listeners, concurrency/global properties, security/encryption, API session handling, JDBC/CLI behavior, embedded dashboard protocol, CI/deployment configuration, or supported-platform changes.
- **Critical:** breaking API change, release/version/signing change, credential handling, dependency vulnerability response, data-loss risk, or changes affecting all consumers.

## Release Ownership and Blocking Gate
- Mohab Mohie (`@MohabMohie`) is the release owner and final release decision-maker.
- Only two validations block release:
  1. A simple clean build: `mvn clean install -DskipTests -Dgpg.skip`.
  2. One browser smoke test that opens `https://shafthq.github.io/` and confirms the SHAFT User Guide loaded successfully.
- The smoke test must run with normal diagnostics; do not enable `maximumPerformanceMode`.
- All broader browser, mobile, API, database, cloud, Cucumber, reporting, quality, and compatibility matrices are scheduled and non-blocking. Failures are triaged after publication unless the release owner pauses the release.

## Blocking Release Checklist
- [ ] The simple clean build passes.
- [ ] The single user-guide browser smoke test passes with populated Allure results.

No other automated check blocks release unless the release owner explicitly pauses publication.

## Non-Blocking Release Governance
Complete or record these items, but do not treat them as automated release gates:
- [ ] Scope and risk level are documented.
- [ ] Public API remains source- and binary-compatible with the project-supported latest LTS Java version (currently JDK 25), and any removal completed at least three calendar months of deprecation.
- [ ] `pom.xml` version matches `SHAFT.Properties.internal.shaftEngineVersion`; `allure3Version` and `nodeLtsVersion` are reviewed and updated together when release tooling changes.
- [ ] Public JavaDocs generation/publication is scheduled or complete.
- [ ] No secrets, generated artifacts, local reports, binaries, or environment-specific values are committed.
- [ ] Scheduled quality and broad test matrices are recorded for follow-up.
- [ ] Release notes and rollback notes are complete.

## Migration and Configuration Checklist
- Not found in current codebase scan: application database migrations or an owned schema.
- If a consumer-facing property is added/changed, document key, default, precedence, thread-local behavior, compatibility, and sample usage.
- Defaults must be safe and preserve existing behavior unless a breaking change is approved.
- For resource/config format changes, document how downstream projects upgrade and roll back.
- For any future schema-related work, require forward migration, rollback/recovery procedure, data compatibility analysis, and maintainer approval.

## Shipment Completion
A release is complete once the approved version has shipped: the GitHub release/tag exists and Maven Central publication has completed. No post-release observation window is required.

Before declaring shipment complete, the release owner verifies:
- The release workflow completed without exposing secrets.
- The GitHub release/tag matches the `pom.xml` version.
- Maven Central contains the expected POM, JAR, sources, JavaDocs, signatures, and checksums.
- User-guide/sample synchronization and JavaDocs publication workflows were triggered or scheduled as non-blocking follow-up.

Mohab Mohie (`@MohabMohie`) is the release owner and provides final approval.

## Rollback Notes Expectations
Every production-impacting change must state:
- Last known good version/commit.
- Whether rollback is a code revert, release downgrade, property toggle, or integration disablement.
- Compatibility effects on downstream test projects and generated reports.
- Data/artifact cleanup required, especially for decrypted files, reports, drivers, or partial releases.
- Why rollback is safe and what validation follows it.

Published Maven versions are immutable; rollback normally means releasing a corrected version and advising consumers to pin/downgrade. Do not overwrite an existing release.

## Release Notes Expectations
Include:
- User-visible behavior and affected module.
- New/changed/deprecated public APIs and properties.
- Compatibility or migration steps.
- Fixed defects and security impact without exploit-sensitive detail.
- Validation performed and known environment gaps.
- Rollback/downgrade guidance.

## After Shipment
Once shipped, the release is done. Any later defect, documentation correction, synchronization failure, or integration problem is handled as new follow-up work and does not keep the completed release open.
