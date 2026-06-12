# SHAFT MCP cloud deployment

Hosted deployments use Streamable HTTP at `/mcp` with
`SPRING_PROFILES_ACTIVE=http`. The server defaults to port `8081`; Fly.io sets
`PORT=8080`.

These files are deployment templates, not a promise that a SHAFT-hosted
endpoint is active. Maven Central, GHCR, and the MCP Registry are the supported
public distribution channels. A hosted service becomes available only after
its external account, repository integration, and required secret are
configured and its `/mcp` endpoint passes initialization.

All container builds use the SHAFT_ENGINE repository root as their Docker build
context so `shaft-mcp` is compiled with the same reactor version and canonical
`shaft-engine` dependency as the Maven Central release.

## Local container check

```bash
docker build -f shaft-mcp/Dockerfile.smithery.build -t shaft-mcp-http .
docker run --rm -p 8081:8081 shaft-mcp-http
```

Connect an MCP client to `http://localhost:8081/mcp`.

## Smithery

The root `smithery.yaml` selects
`shaft-mcp/Dockerfile.smithery.build`. Connect the `ShaftHQ/SHAFT_ENGINE`
repository in Smithery. Smithery rebuilds from the GitHub integration after a
release; no model-provider key is passed to the SHAFT image.

Release `10.2.20260612` has no active Smithery listing. Treat the workflow step
as a handoff reminder until the canonical repository integration is configured
and verified.

## Render

The root `render.yaml` defines the `shaft-mcp` web service and selects
`shaft-mcp/Dockerfile.render`. The post-release workflow calls
`RENDER_DEPLOY_HOOK_URL` when that repository secret is configured.

Release `10.2.20260612` did not configure that secret. No Render endpoint is
advertised for this release.

## Fly.io

Deploy from the repository root:

```bash
flyctl deploy --remote-only --config shaft-mcp/fly.toml
```

The release workflow uses `FLY_API_TOKEN` when configured. Release
`10.2.20260612` did not configure that secret, and
`https://shaft-mcp.fly.dev/mcp` is not an active endpoint.

## Release flow

1. The root Maven Central workflow publishes `io.github.shafthq:SHAFT_MCP`
   with the complete SHAFT reactor.
2. `publish-shaft-mcp.yml` builds and pushes
   `ghcr.io/shafthq/shaft-engine-mcp:<version>` and `latest`.
3. The same workflow renders `shaft-mcp/server.json` from the root version,
   validates it against the MCP registry schema, and publishes it with GitHub
   OIDC.
4. `deploy-shaft-mcp.yml` triggers configured Render and Fly.io deployments
   and records the Smithery handoff. Missing external configuration is reported
   as an explicit no-op.

The historical `ShaftHQ/SHAFT_MCP` repository is read-only. Its
`ghcr.io/shafthq/shaft-mcp:10.2.20260612` compatibility image was built from
the monorepo and remains available for migration. Use
`ghcr.io/shafthq/shaft-engine-mcp` for current and future container releases.
