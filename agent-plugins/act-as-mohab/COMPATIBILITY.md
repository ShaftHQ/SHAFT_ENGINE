# Compatibility

Evidence applies to `act-as-mohab` 10.3.20260809 as of 2026-08-09. This release exposes
one discoverable `act-as-mohab` skill; its consultation and retrieval stages
remain available as internal references, including the mandatory executable
planning contract and repository-safe plan-artifact routing.

| Client | Discovery and validation | Clean live load |
| --- | --- | --- |
| Codex | Native `.codex-plugin` adapter and package validation passed. | Passed for 1.0.0; 10.3.20260809 preserves the adapter and adds the discovery and bundled planning-contract changes. The earlier evidence covers adapter loading, not the new contract behavior. Evidence is recorded in [#4576](https://github.com/ShaftHQ/SHAFT_ENGINE/issues/4576). |
| Claude Code | Native `.claude-plugin` adapter; `claude plugin validate --strict` passed. | **Unverified.** The organization blocked Claude Code subscription access before the package could load. The maintainer-approved 1.0.0 exception carries no live-load support claim for 10.3.20260809. |

The Claude live-load proof remains tracked in [#4636](https://github.com/ShaftHQ/SHAFT_ENGINE/issues/4636). Consumers should validate the package in their own client before relying on it.
