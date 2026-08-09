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
- Record client compatibility evidence, including the approved waiver for the
  unavailable Claude live-load check.

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
script. The normal SHAFT release workflow runs that script after source
validation and supplies the archives to its existing GitHub Release action.

The release workflow remains the only source of GitHub Release events. The
current one-time distribution is an annotated Git tag pointing at the merged
implementation commit, which consumers can pin without starting MCP, IntelliJ,
or deployment workflows.

## Data and Interfaces

`agent-plugins/release.json` will contain one object per package with its name,
SemVer, and supported-client evidence. Package assembly receives the declared
version rather than hard-coding `1.0.0`. The release-artifact script accepts an
empty output directory, assembles both packages, runs the existing validator,
creates one ZIP and one `.sha256` file per package, and prints their paths.

The normal release workflow supplies those four files as release assets. It
does not publish them for ordinary `main` commits; they are attached only by
the existing `announce_release` job after Maven, installer, and release checks
have succeeded.

## Invariants

- Root `plugin.json` remains Agent Plugins v1.0.0 conformant.
- Package files remain contained, tracked-source-derived, and secret-free.
- A package release version never derives from the Maven release version.
- No current tag creates a GitHub Release or triggers downstream deployment.
- Future package changes are a release-relevant input and are verified before
  the normal release is announced.

## Verification

Focused Python tests first demonstrate that an absent or invalid release
manifest fails, then verify version propagation, deterministic ZIP/checksum
contents, and release-workflow attachment. Existing package assembly and agent
guidance gates remain green. A final release-readiness check verifies the
one-time tag points exactly at the merged commit without a corresponding
GitHub Release.
