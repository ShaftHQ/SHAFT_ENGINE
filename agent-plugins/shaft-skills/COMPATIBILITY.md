# Compatibility

Evidence applies to `shaft-skills` 1.0.0 as of 2026-08-09.

| Client | Discovery and validation | Clean live load |
| --- | --- | --- |
| Codex | Native `.codex-plugin` adapter and package validation passed. | Passed; evidence is recorded in [#4576](https://github.com/ShaftHQ/SHAFT_ENGINE/issues/4576). |
| Claude Code | Native `.claude-plugin` adapter; `claude plugin validate --strict` passed. | **Unverified.** The organization blocked Claude Code subscription access before the package could load. The maintainer explicitly approved skipping this check for 1.0.0; this is not a support claim. |

The Claude live-load proof remains tracked in [#4636](https://github.com/ShaftHQ/SHAFT_ENGINE/issues/4636). Consumers should validate the package in their own client before relying on it.
