# SHAFT Pilot release

This runbook prepares and verifies the first monorepo SHAFT Pilot release.
Publication and repository archival are ordered so no external channel points
to an unverified artifact.

## Release-candidate matrix

| Area | Required release evidence |
| --- | --- |
| Runtime | JDK 25 and Maven 3.9 or newer. |
| Modules | `shaft-pilot-core`, `shaft-capture`, `shaft-doctor`, `shaft-ai`, and `SHAFT_MCP` tests plus full reactor package. |
| No-AI path | `pilot.ai.enabled=false`, provider `none`, Capture generation/replay, Doctor diagnosis, and MCP tools pass without credentials. |
| Browsers | Headless Chrome and Edge record the representative local fixture; generated Chrome replay compiles, passes, and populates Allure. |
| Doctor | Product, test, locator, data, timing, environment, infrastructure, unknown, empty, and retry evidence fixtures pass. |
| Providers | OpenAI, Anthropic, Gemini, and Ollama use local mock endpoints for success, timeout, rate-limit, authentication, malformed-response, schema, redaction, and fallback conformance. |
| MCP transports | Packaged stdio and Streamable HTTP `/mcp` initialization and tool discovery pass. |
| Containers | The release Docker image starts in HTTP mode and completes MCP initialization. |
| External agents | ChatGPT, Codex, Claude, Gemini, and GitHub Copilot setup is documented with current client limitations and credential ownership. |
| Publication | Maven Central artifacts, sources, JavaDocs, signatures, BOM, executable JAR, aggregate SBOM, GHCR image, MCP registry metadata, and GitHub release use one version. |
| Migration | The preserved `io.github.shafthq:SHAFT_MCP` coordinate and `shaft-mcp` server ID resolve. `ghcr.io/shafthq/shaft-mcp:10.2.20260612` remains a tested compatibility image, while `ghcr.io/shafthq/shaft-engine-mcp` is published by the monorepo for current and future releases. |

Live paid-provider smoke tests are optional. Run them only with explicit
approval and credentials in the provider-native environment variable. Their
absence cannot fail the normal release gate.

## Deterministic gate

Run from a clean checkout:

```bash
python3 scripts/ci/validate_reactor_versions.py
python3 scripts/ci/validate_modular_documentation.py
python3 scripts/ci/validate_shaft_pilot_release.py
mvn -pl shaft-engine,shaft-pilot-core,shaft-capture,shaft-doctor,shaft-ai,shaft-mcp \
  -am install -DskipTests -Dgpg.skip
mvn -pl shaft-pilot-core,shaft-capture,shaft-doctor,shaft-ai,shaft-mcp test -Dgpg.skip
python3 scripts/ci/validate_shaft_pilot_release.py --check-test-results
mvn -pl shaft-capture test \
  -DincludeCaptureBrowserE2E \
  -Dtest=ManagedCaptureRecorderBrowserTest,CaptureGeneratedReplayBrowserTest \
  -Dgpg.skip
mvn clean install -DskipTests -Dgpg.skip
python3 scripts/ci/validate_maven_publication.py --check-build-outputs
python3 scripts/ci/validate_shaft_pilot_release.py --check-build-outputs
python3 scripts/ci/validate_shaft_mcp_transports.py
```

On PowerShell, single-quote every Maven `-D` argument. Verify that each Pilot
module has populated `allure-results/*-result.json`; Surefire summaries alone
are not sufficient.

The Maven Central workflow repeats this evidence before deployment, then
validates clean consumer fixtures for provider-neutral core, combined modules,
and the executable MCP coordinate.

## Distribution verification

After Maven Central succeeds:

1. Run `scripts/ci/verify_maven_central_release.py <version>` from isolated
   local repositories.
2. Pull `ghcr.io/shafthq/shaft-engine-mcp:<version>` and initialize `/mcp` in
   a clean container. For the first monorepo release, also verify the
   `ghcr.io/shafthq/shaft-mcp:10.2.20260612` compatibility image.
3. Confirm the MCP registry entry `io.github.ShaftHQ/shaft-mcp` references the
   same package version.
4. Confirm the GitHub release, JavaDocs, aggregate CycloneDX SBOM, sources,
   signatures, BOM, and legacy relocation coordinate use the same version.
5. Exercise one credential-free stdio client configuration and the packaged
   Streamable HTTP transport. Exercise a hosted HTTPS endpoint only when its
   external deployment account is configured; otherwise record it as a known
   limitation instead of presenting the endpoint as live.

Record exact versions, immutable URLs, checksums, workflow runs, and known
limitations in the release or pull request before migration finalization.

For release `10.2.20260612`, the Render and Fly repository secrets were not
configured, the Fly hostname did not resolve, Render returned `404` at `/mcp`,
and the historical Smithery listing was unavailable. These optional hosted
services are not release installation paths. GHCR, MCP Registry, the executable
JAR, stdio, and packaged/container Streamable HTTP were verified.

## Standalone repository migration

Do not archive `ShaftHQ/SHAFT_MCP` until every distribution check above passes.
Then:

1. Replace its README introduction with a prominent notice that development,
   releases, documentation, issues, and source moved to
   `ShaftHQ/SHAFT_ENGINE`.
2. Link the verified monorepo release, [SHAFT Pilot](SHAFT_PILOT.md), and
   [SHAFT MCP](SHAFT_MCP.md).
3. Update the repository description and homepage to the canonical repository.
4. Disable write-oriented workflows and direct users to the canonical issue
   tracker while preserving existing history, issues, tags, and releases.
5. Verify the notice on the default branch.
6. Archive the repository with administrator approval.
7. Recheck the README, release links, Maven coordinate, image, and registry
   entry from a logged-out browser.

Archival is a post-release operation. A failed or incomplete distribution
check leaves the old repository unarchived.

## Rollback

- Before Maven Central succeeds, fix the gate and rerun with the same available
  version.
- After Maven Central succeeds, never overwrite or delete the immutable
  release. Publish a newer version for corrections.
- If GHCR, registry, hosted MCP, JavaDocs, or release creation fails, repair
  only that downstream channel and keep the verified Maven version.
- If migration verification fails, leave `ShaftHQ/SHAFT_MCP` writable and
  restore its normal automation until the canonical path is confirmed.
- If a secret canary appears in a recording, report, generated test, image,
  log, example, or packaged artifact, stop publication, remove the output,
  rotate any real credential, and repeat the full gate.
