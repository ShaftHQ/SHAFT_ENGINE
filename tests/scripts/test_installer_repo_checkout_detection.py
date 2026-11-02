#!/usr/bin/env python3
"""Regression coverage for the installer wrappers' repo-checkout detection.

install-shaft-mcp.sh/.ps1 can reuse a co-located install_shaft_mcp.py instead of downloading a
fresh copy, but that shortcut must only fire for a genuine SHAFT_ENGINE checkout (verified via the
repo root pom.xml two directories up). Otherwise a stale install_shaft_mcp.py left behind in a
scratch/temp directory by an earlier "copy command" run would be reused forever instead of always
fetching the latest installer -- reintroducing whatever bug that stale copy carried (see #3374).
"""

from __future__ import annotations

import json
import os
import platform
import shutil
import subprocess  # nosec B404 - tests exercise installer detection with controlled shell scripts.
import tempfile
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
SHAFT_PARENT_POM = "<project><artifactId>shaft-parent</artifactId></project>"
SHELL = next(
    (candidate for candidate in (
        shutil.which("sh"),
        "C:/Program Files/Git/bin/sh.exe",
        "C:/Program Files/Git/usr/bin/sh.exe",
    ) if candidate and Path(candidate).is_file()),
    None,
)


def _write_scenarios(root: Path) -> tuple[Path, Path]:
    """Creates a genuine-repo-checkout directory and an unrelated scratch directory.

    Both get a co-located install_shaft_mcp.py sibling; only the repo-checkout one should be
    considered eligible for local reuse.
    """
    repo_checkout = root / "repoA" / "scripts" / "mcp"
    repo_checkout.mkdir(parents=True)
    (root / "repoA" / "pom.xml").write_text(SHAFT_PARENT_POM, encoding="utf-8")
    (repo_checkout / "install_shaft_mcp.py").write_text("print('local sibling used')", encoding="utf-8")

    scratch = root / "scratchB" / "deep" / "enough"
    scratch.mkdir(parents=True)
    (scratch / "install_shaft_mcp.py").write_text("print('stale file used')", encoding="utf-8")

    return repo_checkout, scratch


class ShellInstallerRepoCheckoutDetectionTest(unittest.TestCase):
    def test_shim_delegates_to_the_agentic_tools_installer(self) -> None:
        # is_shaft_engine_repo_checkout lived in install-shaft-mcp.sh. That file
        # is now a shim for install-shaft-agentic-tools.sh and no longer detects
        # a repo checkout or a stale sibling installer.
        script = (ROOT / "scripts" / "mcp" / "install-shaft-mcp.sh").read_text(encoding="utf-8")
        self.assertIn("install-shaft-agentic-tools.sh", script)
        self.assertNotIn("is_shaft_engine_repo_checkout", script)
        self.assertIn("deprecated", script)

class PowerShellInstallerRepoCheckoutDetectionTest(unittest.TestCase):
    def test_shim_delegates_to_the_agentic_tools_installer(self) -> None:
        # Test-ShaftEngineRepoCheckout lived in install-shaft-mcp.ps1. That file
        # is now a shim for install-shaft-agentic-tools.ps1.
        script = (ROOT / "scripts" / "mcp" / "install-shaft-mcp.ps1").read_text(encoding="utf-8")
        self.assertIn("install-shaft-agentic-tools.ps1", script)
        self.assertNotIn("function Test-ShaftEngineRepoCheckout", script)
        self.assertIn("deprecated", script)


if __name__ == "__main__":
    unittest.main()
