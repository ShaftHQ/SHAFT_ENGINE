# Changelog

## 10.3.20260817 - 2026-08-17

- Align the bundled package version with the canonical SHAFT engine release.
- Restore the last fully validated portable harness contract after the newer
  harness revision failed the repository's cross-platform acceptance gate.

## 10.3.20260809 - 2026-08-09

- Breaking: expose only `act-as-mohab` as a discoverable skill; consultation
  and retrieval are now internal lifecycle references of that entrypoint.
- Add the bundled mandatory executable-planning contract and repository-safe
  plan-artifact routing to those internal lifecycle references.
- Add a deterministic stdlib Python runtime with repository-context,
  repository-aware PR watching, checkpoint-status, and MCP commands.
- Default repository operations to the caller's cwd and make bare numeric PR
  inference visible on stderr without contaminating machine-readable stdout.

## 1.0.0 - 2026-08-09

- First portable `act-as-mohab` Agent Plugin package.
