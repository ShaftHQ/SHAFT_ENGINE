#!/usr/bin/env python3
"""Regression coverage for the installer wrappers' repo-checkout detection.

install-shaft-mcp.sh/.ps1 can reuse a co-located install_shaft_mcp.py instead of downloading a
fresh copy, but that shortcut must only fire for a genuine SHAFT_ENGINE checkout (verified via the
repo root pom.xml two directories up). Otherwise a stale install_shaft_mcp.py left behind in a
scratch/temp directory by an earlier "copy command" run would be reused forever instead of always
fetching the latest installer -- reintroducing whatever bug that stale copy carried (see #3374).
"""

from __future__ import annotations

import platform
import shutil
import subprocess
import tempfile
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
SHAFT_PARENT_POM = "<project><artifactId>shaft-parent</artifactId></project>"


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


@unittest.skipUnless(shutil.which("sh"), "sh is required to test install-shaft-mcp.sh")
class ShellInstallerRepoCheckoutDetectionTest(unittest.TestCase):
    def setUp(self) -> None:
        script = (ROOT / "scripts" / "mcp" / "install-shaft-mcp.sh").read_text(encoding="utf-8")
        marker = "\nbanner\n"
        boundary = script.index(marker)
        # Only the function/helper definitions above the top-level execution are needed; sourcing
        # the whole file would immediately run the real installer.
        self.functions_only = script[:boundary]

    def _detect(self, directory: Path) -> bool:
        # The candidate directory is passed as a real argv entry ($1), never interpolated into
        # the script text, so this cannot be mistaken for a shell-injection-shaped invocation.
        script = self.functions_only + (
            '\nis_shaft_engine_repo_checkout "$1" && echo REPO_CHECKOUT || echo NOT_REPO_CHECKOUT\n'
        )
        with tempfile.TemporaryDirectory() as script_dir:
            script_path = Path(script_dir) / "detect.sh"
            script_path.write_text(script, encoding="utf-8")
            result = subprocess.run(
                ["sh", str(script_path), str(directory)],
                capture_output=True,
                text=True,
                check=True,
            )
        return "REPO_CHECKOUT" == result.stdout.strip()

    def test_genuine_repo_checkout_is_detected(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            repo_checkout, _ = _write_scenarios(Path(tmp))
            self.assertTrue(self._detect(repo_checkout))

    def test_scratch_directory_with_stale_sibling_is_rejected(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            _, scratch = _write_scenarios(Path(tmp))
            self.assertFalse(self._detect(scratch))


@unittest.skipUnless(platform.system() == "Windows", "PowerShell installer only ships for Windows")
class PowerShellInstallerRepoCheckoutDetectionTest(unittest.TestCase):
    def setUp(self) -> None:
        script = (ROOT / "scripts" / "mcp" / "install-shaft-mcp.ps1").read_text(encoding="utf-8")
        start = script.index("function Test-ShaftEngineRepoCheckout")
        end = script.index("\n    function Resolve-PythonInstallerScript")
        self.function_only = script[start:end]

    def _detect(self, directory: Path) -> bool:
        # The candidate directory is bound through a real -Directory CLI parameter, never
        # interpolated into the script text, so this cannot be mistaken for a command-injection-
        # shaped invocation.
        script = (
            "param([string] $Directory)\n"
            + self.function_only
            + '\nif (Test-ShaftEngineRepoCheckout $Directory) '
            + '{ Write-Output "REPO_CHECKOUT" } else { Write-Output "NOT_REPO_CHECKOUT" }\n'
        )
        with tempfile.TemporaryDirectory() as script_dir:
            script_path = Path(script_dir) / "detect.ps1"
            script_path.write_text(script, encoding="utf-8")
            result = subprocess.run(
                ["powershell", "-NoProfile", "-ExecutionPolicy", "Bypass",
                 "-File", str(script_path), "-Directory", str(directory)],
                capture_output=True,
                text=True,
                check=True,
            )
        return "REPO_CHECKOUT" == result.stdout.strip()

    def test_genuine_repo_checkout_is_detected(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            repo_checkout, _ = _write_scenarios(Path(tmp))
            self.assertTrue(self._detect(repo_checkout))

    def test_scratch_directory_with_stale_sibling_is_rejected(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            _, scratch = _write_scenarios(Path(tmp))
            self.assertFalse(self._detect(scratch))


if __name__ == "__main__":
    unittest.main()
