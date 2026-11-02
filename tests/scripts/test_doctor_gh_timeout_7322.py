"""#7322: a hung `gh auth status` must not hang doctor or maintain."""
from __future__ import annotations

import importlib.util
import os
import stat
import subprocess  # nosec B404 - runs the probe against a fake gh only
import tempfile
import time
import unittest
import unittest.mock
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]


def load():
    spec = importlib.util.spec_from_file_location("mcp_policy_7322", ROOT / "chaos-engine" / "mcp_policy.py")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


class GhProbeTimeoutTest(unittest.TestCase):
    def setUp(self):
        self.policy = load()

    def test_probe_passes_a_timeout_and_treats_expiry_as_unconfigured(self):
        seen = {}

        def hung(cmd, **kwargs):
            seen.update(kwargs)
            raise subprocess.TimeoutExpired(cmd, kwargs.get("timeout"))

        self.assertFalse(self.policy.gh_is_configured(runner=hung))
        self.assertEqual(seen["timeout"], self.policy.GH_PROBE_TIMEOUT_SECONDS)

    def test_a_real_hanging_gh_returns_within_the_timeout(self):
        with tempfile.TemporaryDirectory() as temp:
            fake = Path(temp) / "gh"
            fake.write_text("#!/bin/sh\nsleep 30\n", encoding="utf-8")
            fake.chmod(fake.stat().st_mode | stat.S_IXUSR)
            env = dict(os.environ, PATH=f"{temp}{os.pathsep}{os.environ.get('PATH', '')}")
            with unittest.mock.patch.dict(os.environ, env), unittest.mock.patch.object(self.policy, "GH_PROBE_TIMEOUT_SECONDS", 1):
                started = time.monotonic()
                self.assertFalse(self.policy.gh_is_configured())
                self.assertLess(time.monotonic() - started, 10)

    def test_repair_leaves_mcp_unchanged_when_gh_times_out(self):
        result = self.policy.repair_user_github_mcp(home=Path(tempfile.gettempdir()) / "ce-7322-none",
                                                    gh_probe=lambda: self.policy.gh_is_configured(
                                                        runner=lambda cmd, **k: (_ for _ in ()).throw(
                                                            subprocess.TimeoutExpired(cmd, 1))))
        self.assertFalse(result["ghHealthy"])


if __name__ == "__main__":
    unittest.main()
