"""Load-stamp gate: checkpoint/resume refuse on a missing, stale, or mismatched entry stamp."""

import importlib.util
import shutil
import subprocess  # nosec B404 - runs the repository's own tool.py.
import sys
import tempfile
import time
import unittest
from pathlib import Path

CE = Path(__file__).resolve().parents[2] / "chaos-engine"
FILES = ("tool.py", "identity.md", "skills/chaos-engine/SKILL.md",
         "companions/caveman-ultra.md", "companions/ponytail-ultra.md", "task_ledger.py")


def _load_tool():
    spec = importlib.util.spec_from_file_location("ce_tool_load_stamp", CE / "tool.py")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


class LoadStampTest(unittest.TestCase):
    tool = _load_tool()

    def setUp(self):
        self._tmp = tempfile.TemporaryDirectory()
        self.addCleanup(self._tmp.cleanup)
        self.root = Path(self._tmp.name) / ".chaos-engine"
        for relative in FILES:
            target = self.root / relative
            target.parent.mkdir(parents=True, exist_ok=True)
            shutil.copy2(CE / relative, target)
        self.stamp = self.root / "runtime" / "entry-stamp"

    def run_tool(self, *args):
        return subprocess.run([sys.executable, str(self.root / "tool.py"), *args],  # nosec B603
                              capture_output=True, text=True, check=False, cwd=self.root.parent)

    def test_missing_stamp_refuses(self):
        self.assertFalse(self.tool.load_stamp_fresh(self.root))

    def test_stale_stamp_refuses(self):
        self.tool.entry_output(self.root, full=True)
        self.assertFalse(self.tool.load_stamp_fresh(self.root, now=time.time() + 41 * 60))

    def test_card_change_refuses(self):
        self.tool.entry_output(self.root, full=True)
        card = self.root / "companions/caveman-ultra.md"
        card.write_text(card.read_text(encoding="utf-8") + "\nedited\n", encoding="utf-8")
        self.assertFalse(self.tool.load_stamp_fresh(self.root))

    def test_fresh_stamp_passes(self):
        self.tool.entry_output(self.root, full=True)
        self.assertTrue(self.tool.load_stamp_fresh(self.root))

    def test_legacy_digest_only_stamp_refuses(self):
        self.stamp.parent.mkdir(parents=True, exist_ok=True)
        self.stamp.write_text("abcdef123456\n", encoding="utf-8")
        self.assertFalse(self.tool.load_stamp_fresh(self.root))

    def test_source_checkout_is_never_gated(self):
        self.assertTrue(self.tool.load_stamp_fresh(CE))

    def test_real_block_then_real_pass(self):
        blocked = self.run_tool("resume", "t")
        self.assertEqual(2, blocked.returncode)
        self.assertEqual("run entry --full\n", blocked.stderr)
        self.assertEqual("", blocked.stdout)
        self.assertEqual(0, self.run_tool("entry", "--full").returncode)
        spec = Path(self._tmp.name) / "spec-t.md"
        spec.write_text("# Spec\n", encoding="utf-8")
        passed = self.run_tool("checkpoint", "t", "implement", "--next", "push", "--spec", str(spec))
        self.assertEqual(0, passed.returncode, passed.stderr)
        self.assertNotIn("entry --full", passed.stdout + passed.stderr)


if __name__ == "__main__":
    unittest.main()
