---
name: mcp-transport-contract-auditor
description: Use when auditing shaft-mcp tool manifests, client fixtures, installers, containers, workflows, or local transport contracts.
---

# MCP Transport Contract Auditor

Use for `shaft-mcp` contract changes across tool schemas, fixtures, startup, installers, containers, and transports.

## Workflow

1. Pair Java/service edits with `shaft-mcp/src/test/resources/fixtures/mcp-tool-manifest.json` and `shaft-mcp/src/test/resources/fixtures/shaft-pilot/mcp/**` when the public tool contract changes.
2. Run `python3 scripts/ci/validate_shaft_mcp_configuration.py`. Also run `python3 scripts/ci/validate_modular_documentation.py` for client fixture or public docs-link changes.
3. For Java behavior, run the narrow affected `shaft-mcp` tests with `-DheadlessExecution=true` and `-Dgpg.skip`.
4. Before claiming transport safety, package `shaft-mcp`, copy runtime dependencies to `shaft-mcp/target/dependency`, then run `python3 scripts/ci/validate_shaft_mcp_transports.py`.
5. For installer or release-facing changes, run `python3 scripts/ci/verify_shaft_mcp_installer_release.py` or state why it is not applicable.

## Output

List contract files touched, transport/client impact, checks run, and remaining fixture or installer risk.
