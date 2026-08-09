# Agent Plugin Release Design

## Goal

Ship the already-built `act-as-mohab` and `shaft-skills` packages through a
safe, repeatable release contract. Use one explicit tag-only release for the
current packages, then attach verified package artifacts to future normal
SHAFT releases.

## Scope

- Keep the two packages independently versioned with stable SemVer.
- Assemble, validate, archive, checksum, and document both packages in the
  existing normal release workflow.
- Publish package artifacts only as assets of the normal SHAFT GitHub Release.
- Create the one-time `agent-plugins-v1.0.0` Git tag only after the validated
  implementation merges; do not create a GitHub Release for that tag.
- Add deterministic tests for the release metadata and artifact contract.
- Record client compatibility evidence. The unavailable Claude live-load check
  is an explicitly user-approved exception, recorded as **unverified** rather
  than counted as a successful client load; its proof remains tracked in #4636.

## Non-goals

- Do not add an MCP server, credentials, user-local state, or host trust
  configuration to either portable package.
- Do not invoke the existing GitHub Release event for the one-time tag.
- Do not move the harness into a subproject or separate repository before a
  second consumer proves that need.

## Architecture

One tracked release manifest is the source of truth for the independent
package versions and compatibility evidence. The existing package assemblers
read that manifest, retain their self-contained package boundaries, and emit
versioned ZIP archives plus SHA-256 checksums through a small release-artifact
script. Both existing normal-release producers—the normal Maven release and
the Maven Central reconciliation recovery—run that script from the exact
release revision and attach its assets to the GitHub Release.

The release workflow remains the only source of GitHub Release events. The
current one-time distribution is an annotated Git tag pointing at the merged
implementation commit, which consumers can pin without starting MCP, IntelliJ,
or deployment workflows.

## Data and Interfaces

`agent-plugins/release.json` will contain one object per package with its name
and SemVer. A package-root `COMPATIBILITY.md` records discovery, validation,
installation, and real-load evidence per client. Package assembly receives the
declared version rather than hard-coding `1.0.0`. The release-artifact script
accepts an empty output directory, assembles both packages, runs the existing
validator, creates one ZIP and one `.sha256` file per package, and prints their
paths.

Both normal-release paths supply those files as release assets. They do not
publish them for ordinary `main` commits; assets are attached only after Maven,
installer, and release checks have succeeded. A recovery path repairs an
existing GitHub Release that is missing the assets without creating a duplicate
release.

## Invariants

- Root `plugin.json` remains Agent Plugins v1.0.0 conformant.
- Package files remain contained, tracked-source-derived, and secret-free.
- A package release version never derives from the Maven release version.
- If package payload inputs change since the most recent tagged release
  manifest, the affected package version must increase; a SemVer version never
  denotes two payloads.
- No current tag creates a GitHub Release or triggers downstream deployment.
- Future package changes are a release-relevant input and are verified before
  the normal release is announced.

## Verification

Focused Python tests first demonstrate that an absent or invalid release
manifest fails, then verify version propagation, immutable-version rejection,
deterministic ZIP/checksum contents, complete package metadata, and attachment
through both normal-release paths. Existing package assembly and agent guidance
gates remain green. A final release-readiness check verifies the one-time tag
points exactly at the merged commit without a corresponding GitHub Release.
