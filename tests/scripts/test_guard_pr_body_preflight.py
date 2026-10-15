"""#6986: gh pr create/edit bodies are checked for the riskiest-assumption section."""

from __future__ import annotations

import importlib.util
import tempfile
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]


def _load_guard():
    spec = importlib.util.spec_from_file_location("ce_guard_6986", ROOT / "chaos-engine/hooks/guard.py")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


class PrBodyPreflightTest(unittest.TestCase):
    def setUp(self) -> None:
        self.guard = _load_guard()

    def test_body_file_without_targeted_test_is_blocked(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            body = Path(temporary, "body.md")
            body.write_text("Fixes things\n\n## Riskiest assumption\nThat it works.\n", encoding="utf-8")
            reason = self.guard._pr_body_block((f"gh pr create --title t --body-file {body}",))
        self.assertIn("PR body would fail the CI PR Body guard", reason)
        self.assertIn("targeted test", reason)

    def test_missing_section_on_inline_body_is_blocked(self) -> None:
        reason = self.guard._pr_body_block(("gh pr edit 5 --body 'just text'",))
        self.assertIn("missing the Riskiest assumption", reason)

    def test_valid_bodies_and_other_commands_pass(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            body = Path(temporary, "good.md")
            body.write_text("x\n\n## Riskiest assumption\nThat y. Targeted test: `test_y`.\n", encoding="utf-8")
            self.assertEqual("", self.guard._pr_body_block((f"gh pr create --body-file {body}",)))
        self.assertEqual("", self.guard._pr_body_block(("gh pr create --body 'x\n\n## Riskiest assumption\nN/A'",)))
        self.assertEqual("", self.guard._pr_body_block(("gh pr view 5",)))
        self.assertEqual("", self.guard._pr_body_block(("gh pr create --fill",)))
        self.assertEqual("", self.guard._pr_body_block(("gh pr create --body-file /no/such/file",)))


if __name__ == "__main__":
    unittest.main()
