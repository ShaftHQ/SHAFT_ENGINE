"""Lesson cards L1 delete-check discipline and L2 content-scan preflight (2026-10-29)."""

from __future__ import annotations

import subprocess  # nosec B404 - stdlib only; argv is fixed CE helper path
import sys
import tempfile
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
CE = ROOT / "chaos-engine"
def _text(path: Path) -> str:
    return " ".join(path.read_text(encoding="utf-8").split())


# Import helper without package layout
import importlib.util

_spec = importlib.util.spec_from_file_location(
    "content_scan_preflight", CE / "content_scan_preflight.py"
)
_mod = importlib.util.module_from_spec(_spec)
assert _spec.loader is not None
_spec.loader.exec_module(_mod)


class DeleteCheckDisciplineLessonTest(unittest.TestCase):
    def test_discipline_card_has_rule_why_check_and_follow_up(self) -> None:
        path = CE / "references/delete-check-discipline.md"
        raw = path.read_text(encoding="utf-8")
        text = _text(path)
        self.assertTrue(raw.startswith("# Delete-check discipline"))
        for section in ("## Rule", "## Why", "## Check", "## Follow-up"):
            self.assertIn(section, raw)
        for needle in (
            "tool.py delete-check",
            "doctor",
            "delete-guard: pass",
            "Stop on refusal",
            "Owner override",
            "--request-code",
            "agent-relayed code is not approval",
            "delete_guard.py",
            "references/delete-guard.md",
            "test_chaos_engine_delete_guard_7251.py",
        ):
            self.assertIn(needle, text)

    def test_bot_entry_links_discipline_not_mobile_retro(self) -> None:
        bot = (CE / "references/bot-entry.md").read_text(encoding="utf-8")
        self.assertIn("delete-check-discipline.md", bot)
        self.assertNotIn("mobile-retro", bot)


class ContentScanPreflightLessonTest(unittest.TestCase):
    def test_reference_requires_content_scan_and_planted_self_test(self) -> None:
        path = CE / "references/content-scan-preflight.md"
        raw = path.read_text(encoding="utf-8")
        text = _text(path)
        self.assertTrue(raw.startswith("# Content-scan preflight"))
        for section in ("## Rule", "## Why", "## Check", "## Helper"):
            self.assertIn(section, raw)
        for needle in (
            "Content-scan",
            "site-packages",
            "custom_nodes",
            "--self-test",
            "plants",
            "Path/filename denylists",
            "not enough",
            "content_scan_preflight.py",
        ):
            self.assertIn(needle, text)

    def test_tip_churn_and_harness_map_link_on_demand(self) -> None:
        tip = (CE / "references/tip-churn-preflight.md").read_text(encoding="utf-8")
        hmap = (CE / "references/harness-map.md").read_text(encoding="utf-8")
        self.assertIn("content-scan-preflight.md", tip)
        self.assertIn("delete-check-discipline.md", tip)
        self.assertIn("delete-check-discipline.md", hmap)
        self.assertIn("content-scan-preflight.md", hmap)
        self.assertIn("content_scan_preflight.py", hmap)

    def test_planted_self_test_detects_banned_token(self) -> None:
        self.assertEqual(0, _mod.planted_self_test())

    def test_content_scan_finds_token_inside_file_not_name(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            safe_name = root / "innocent_module.py"
            safe_name.write_text("import planted_banned_marker_xyz\n", encoding="utf-8")
            findings = _mod.scan_roots([root], ["planted_banned_marker_xyz"])
            self.assertTrue(findings)
            # Basename alone is clean; content carries the token.
            self.assertNotIn("planted_banned_marker_xyz", safe_name.name)

    def test_cli_self_test_exits_zero(self) -> None:
        proc = subprocess.run(  # nosec B603
            [sys.executable, str(CE / "content_scan_preflight.py"), "--self-test"],
            cwd=ROOT,
            capture_output=True,
            text=True,
            check=False,
        )
        self.assertEqual(0, proc.returncode, proc.stderr)


if __name__ == "__main__":
    unittest.main()
