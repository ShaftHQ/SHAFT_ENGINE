"""Frozen ChaosGauge runtime pins.

Codex is the npm dist-tag `latest` at experiment freeze, never a beta/alpha tag
and never a floating unpinned install during a run. Bump this constant when
freezing a new experiment.
"""

CODEX_NPM_PACKAGE = "@openai/codex"
CODEX_VERSION = "0.152.0"
CODEX_CLI_BANNER = f"codex-cli {CODEX_VERSION}"
