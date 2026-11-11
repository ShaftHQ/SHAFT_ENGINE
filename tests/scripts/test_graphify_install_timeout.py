"""A graphifyy install that times out while unpacking is not a network failure (#7925)."""

from __future__ import annotations

import importlib.util
import subprocess
import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
BOOTSTRAP = ROOT / "chaos-engine" / "bootstrap.py"


def _bootstrap():
    spec = importlib.util.spec_from_file_location("ce_bootstrap_graphify_timeout", BOOTSTRAP)
    if spec is None or spec.loader is None:
        raise RuntimeError("cannot load bootstrap.py")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


class GraphifyInstallTimeoutTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        cls.boot = _bootstrap()

    def test_unpacking_graphify_timeout_is_not_a_network_hint(self) -> None:
        error = subprocess.TimeoutExpired(
            cmd=["uv", "tool", "install", "graphifyy"],
            timeout=900,
            output=b"Downloading graphifyy\nUnpacking wheel\n",
        )
        hint = self.boot.next_fix_hint("CE-INSTALL-FAILED", error, "python3 .chaos-engine/install.py")
        folded = hint.casefold()
        self.assertNotIn("restore network", folded)
        self.assertIn("budget", folded)
        self.assertIn("still in progress", folded)

    def test_prepared_installed_graphify_timeout_is_not_a_network_hint(self) -> None:
        error = subprocess.TimeoutExpired(
            cmd=["uv", "tool", "install", "graphifyy==0.9.106"],
            timeout=900,
            output=b"Downloading graphifyy\nPrepared 12 packages\nInstalled 12 packages\n",
        )
        hint = self.boot.next_fix_hint("CE-INSTALL-FAILED", error, "python3 .chaos-engine/install.py")
        folded = hint.casefold()
        self.assertNotIn("restore network", folded)
        self.assertIn("budget", folded)
        self.assertIn("still in progress", folded)
        transcript = error.output.decode().casefold()
        self.assertIn("prepared", transcript)
        self.assertIn("installed", transcript)
        self.assertNotIn("unpack", transcript)

    def test_empty_graphify_uv_install_timeout_is_budget_not_network(self) -> None:
        error = subprocess.TimeoutExpired(
            cmd=["uv", "tool", "install", "graphifyy==0.9.106"],
            timeout=900,
        )
        hint = self.boot.next_fix_hint("CE-INSTALL-FAILED", error, None)
        folded = hint.casefold()
        self.assertNotIn("restore network", folded)
        self.assertIn("budget", folded)
        self.assertIn("still in progress", folded)

    def test_empty_timeout_keeps_network_hint(self) -> None:
        error = subprocess.TimeoutExpired(cmd=["curl", "https://example.invalid"], timeout=5)
        hint = self.boot.next_fix_hint("CE-INSTALL-FAILED", error, None)
        self.assertIn("restore network", hint.casefold())


if __name__ == "__main__":
    unittest.main()
