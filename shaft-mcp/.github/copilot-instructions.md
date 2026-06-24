# shaft-mcp Copilot Instructions

Follow the repository-root `AGENTS.md` and `.github/copilot-instructions.md`.

- Treat `shaft-mcp` as a reactor module; keep its SHAFT version aligned with
  the root `pom.xml`.
- Preserve stdio protocol purity: JSON-RPC belongs on stdout and diagnostics
  belong on stderr.
- Keep HTTP transport behavior at `/mcp` compatible with existing clients.
- Do not add model-provider credentials to shaft-mcp configuration or tests.
- Store executable client examples under
  `src/test/resources/fixtures/shaft-pilot/`; public guidance belongs on the
  [Docusaurus MCP page](https://shafthq.github.io/docs/agentic/mcp).
- Validate focused changes with the affected `shaft-mcp` tests and the
  repository release-contract scripts.
