"""`entry --full` and SessionStart load the personality files (identity, core card, companions)."""

from __future__ import annotations

import importlib.util
import os
import subprocess  # nosec B404 - runs the repository's own tool.py with fixed argv
import sys
import unittest
from pathlib import Path

CE = Path(__file__).resolve().parents[2] / "chaos-engine"


class PersonalityLoadTest(unittest.TestCase):
    def test_entry_full_prints_every_personality_file(self):
        result = subprocess.run([sys.executable, str(CE / "tool.py"), "entry", "--full"],  # nosec B603
                                capture_output=True, text=True, timeout=120, check=True, cwd=CE.parent)
        out = result.stdout
        for marker in ("<!-- identity.md -->", "# ChaosEngine identity",
                       "# ChaosEngine\n",
                       "<!-- companions/caveman-ultra.md -->", "# Caveman ultra card",
                       "<!-- companions/ponytail-ultra.md -->", "# Ponytail ultra card"):
            self.assertIn(marker, out)
        # The push-back policy is reachable from the loaded identity (#7094 lesson).
        self.assertIn("push back", out)
        self.assertIn("references/identity-push-back.md", out)
        self.assertTrue((CE / "references/identity-push-back.md").is_file())

    def test_session_start_points_at_identity_and_both_companions(self):
        spec = importlib.util.spec_from_file_location("ce_lifecycle_personality", CE / "hooks/lifecycle.py")
        module = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(module)
        text = module.session_start_context(None, "on", session_id=f"personality-{os.getpid()}")
        self.assertIn("identity.md", text)
        self.assertIn("caveman-ultra.md", text)
        self.assertIn("ponytail-ultra.md", text)


if __name__ == "__main__":
    unittest.main()
