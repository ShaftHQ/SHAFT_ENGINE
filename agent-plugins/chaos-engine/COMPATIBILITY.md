# Compatibility

Evidence applies to `chaos-engine` 10.3.20260824 as of 2026-08-24. This release exposes
one discoverable `chaos-engine` skill; its consultation and retrieval stages
remain available as internal references, including the mandatory executable
planning contract and repository-safe plan-artifact routing. Its bundled
`bin/chaos-engine.pyz` requires the `python` command to resolve CPython 3.10+
and uses `git` plus authenticated
`gh` for repository and pull-request operations. All repository roots are
caller-relative or explicit; no SHAFT checkout path is embedded.

| Client | Discovery and validation | Clean live load |
| --- | --- | --- |
| Codex | Native `.codex-plugin` adapter and package validation passed. | Most recent clean native load passed for 10.3.20260820 in the scheduled/manual acceptance workflow. The earlier evidence is recorded in [#4576](https://github.com/ShaftHQ/SHAFT_ENGINE/issues/4576). |
| Claude Code | Native `.claude-plugin` adapter; `claude plugin validate --strict` passed. | Most recent clean native load passed for 10.3.20260820 in the scheduled/manual acceptance workflow. |

The Claude live-load proof remains tracked in [#4636](https://github.com/ShaftHQ/SHAFT_ENGINE/issues/4636). Consumers should validate the package in their own client before relying on it.
