"""Public-landing route: generalized lessons for calling cards and product pages.

Each assertion pins one lesson. A later edit that names a specific product
page, or that drops a rule, fails here.
"""

from __future__ import annotations

import json
import re
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
SOURCE = ROOT / "chaos-engine"
PAGE = SOURCE / "references/public-landing.md"
ROUTER = SOURCE / "skills/chaos-engine/SKILL.md"
LEVEL1 = SOURCE / "references/level-1-catalog.md"
INDEX = SOURCE / "harness-index.json"
PORTABLE_ROUTING = SOURCE / "profiles/portable/references/routing.md"
DESIGN = SOURCE / "addons/design-skills/SKILL.md"
PAGE_MAX_BYTES = 4096
FORBIDDEN = ("Mohab", "SHAFT_ENGINE", "shaft-engine", "github.com/MohabMohie")


def compact(path: Path) -> str:
    return re.sub(r"\s+", " ", path.read_text(encoding="utf-8"))


class PublicLandingRoutingTests(unittest.TestCase):
    def test_router_routes_landing_pages_to_the_reference(self):
        rows = [line for line in ROUTER.read_text(encoding="utf-8").splitlines() if line.startswith("| Public landing |")]
        self.assertEqual(1, len(rows))
        self.assertIn("calling card or product page", rows[0])
        self.assertIn("(../../references/public-landing.md)", rows[0])
        self.assertTrue(PAGE.is_file())
        self.assertLessEqual(len(PAGE.read_bytes()), PAGE_MAX_BYTES)

    def test_catalogs_and_design_addon_reach_the_reference(self):
        self.assertIn("[`public-landing.md`](public-landing.md)", LEVEL1.read_text(encoding="utf-8"))
        entries = {entry["name"]: entry for entry in json.loads(INDEX.read_text(encoding="utf-8"))["entries"]}
        self.assertEqual("route", entries["public-landing"]["kind"])
        self.assertEqual("references/public-landing.md", entries["public-landing"]["path"])
        self.assertIn("(../../../references/public-landing.md)", PORTABLE_ROUTING.read_text(encoding="utf-8"))
        self.assertIn("(../../references/public-landing.md)", DESIGN.read_text(encoding="utf-8"))

    def test_lessons_stay_abstract(self):
        text = PAGE.read_text(encoding="utf-8")
        for banned in FORBIDDEN:
            self.assertNotIn(banned, text)
            self.assertNotIn(banned, DESIGN.read_text(encoding="utf-8"))
        folded = compact(PAGE)
        for lesson in (
            "Never invent one",
            "newest primary source wins",
            "phone numbers, national IDs, street addresses, and compensation",
            "behind a disclosure",
            "Do not embed a host",
            "light and dark",
            "not a second policy",
            "similarly named repository is a different product",
            "Do not stuff keywords",
        ):
            self.assertIn(lesson, folded)
