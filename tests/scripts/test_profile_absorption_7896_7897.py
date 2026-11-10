"""On-demand designer and coach profiles for #7896 and #7897."""

from __future__ import annotations

import re
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
DESIGNER = ROOT / "chaos-engine" / "references" / "profiles" / "designer.md"
COACH = ROOT / "chaos-engine" / "references" / "profiles" / "coach.md"
ROLES = ROOT / "chaos-engine" / "references" / "roles.md"
ROUTER = ROOT / "chaos-engine" / "skills" / "chaos-engine" / "SKILL.md"

EMAIL = re.compile(r"[A-Za-z0-9._%+\-]+@[A-Za-z0-9.\-]+\.[A-Za-z]{2,}")
PEM = re.compile(r"-----BEGIN (?:RSA |OPENSSH |EC |DSA )?PRIVATE KEY-----")


def _routes_block(text: str) -> str:
    start = text.index("<!-- HARNESS-ROUTES:START -->")
    end = text.index("<!-- HARNESS-ROUTES:END -->")
    return text[start:end]


class ProfileAbsorptionTest(unittest.TestCase):
    def test_profile_files_exist(self) -> None:
        self.assertTrue(DESIGNER.is_file(), str(DESIGNER))
        self.assertTrue(COACH.is_file(), str(COACH))

    def test_roles_points_at_both_profiles(self) -> None:
        text = ROLES.read_text(encoding="utf-8")
        self.assertIn("chaos-engine/references/profiles/designer.md", text)
        self.assertIn("chaos-engine/references/profiles/coach.md", text)

    def test_profiles_are_not_always_loaded_routes(self) -> None:
        block = _routes_block(ROUTER.read_text(encoding="utf-8"))
        for needle in ("profiles/designer.md", "profiles/coach.md"):
            self.assertNotIn(needle, block)

    def test_profiles_have_no_email_or_private_key(self) -> None:
        for path in (DESIGNER, COACH):
            text = path.read_text(encoding="utf-8")
            self.assertIsNone(EMAIL.search(text), path.name)
            self.assertIsNone(PEM.search(text), path.name)

    def test_coach_reuses_coach_loop(self) -> None:
        text = COACH.read_text(encoding="utf-8")
        self.assertIn("coach-loop.md", text)
        self.assertIn("reused orchestration loop", text)

    def test_designer_names_design_addon(self) -> None:
        text = DESIGNER.read_text(encoding="utf-8")
        self.assertIn("chaos-engine/addons/design-skills/SKILL.md", text)


if __name__ == "__main__":
    unittest.main()
