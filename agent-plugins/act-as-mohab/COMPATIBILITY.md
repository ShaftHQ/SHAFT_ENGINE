# Compatibility

Evidence applies to `act-as-mohab` 10.3.20260817 as of 2026-08-17. This release exposes
one discoverable `act-as-mohab` skill; its consultation and retrieval stages
remain available as internal references, including the mandatory executable
planning contract and repository-safe plan-artifact routing. Its bundled
`bin/act-as-mohab.pyz` requires the `python` command to resolve CPython 3.10+
and uses `git` plus authenticated
`gh` for repository and pull-request operations. All repository roots are
caller-relative or explicit; no SHAFT checkout path is embedded.

| Client | Discovery and validation | Clean live load |
| --- | --- | --- |
| Codex | Native `.codex-plugin` adapter and package validation passed. | Passed for 1.0.0; 10.3.20260817 preserves the validated adapter and portable harness contract. The earlier evidence is recorded in [#4576](https://github.com/ShaftHQ/SHAFT_ENGINE/issues/4576); this version has no new clean-live-load claim. |
| Claude Code | Native `.claude-plugin` adapter; `claude plugin validate --strict` passed. | **Unverified.** The organization blocked Claude Code subscription access before the package could load. The maintainer-approved 1.0.0 exception carries no live-load support claim for 10.3.20260817. |

The Claude live-load proof remains tracked in [#4636](https://github.com/ShaftHQ/SHAFT_ENGINE/issues/4636). Consumers should validate the package in their own client before relying on it.
