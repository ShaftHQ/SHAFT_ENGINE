"""Managed Selenium Grid images must speak a CDP major the client supports.

The compose file and SeleniumGridSetupPlanner must share one valid
selenium/docker-selenium release tag so network interception never drifts back
to an incompatible browser major (such as 4.48.0-20260905 / Edge 152).
"""

import re
import unittest
from pathlib import Path

REPO = Path(__file__).resolve().parents[2]
TAG_PATTERN = re.compile(r"^\d+\.\d+\.\d+-\d{8}$")
PLANNER_TAG_PATTERN = re.compile(r'static final String IMAGE_TAG = "([^"]+)";')


class GridCdpContractTest(unittest.TestCase):
    def test_compose_and_planner_share_one_grid_tag(self):
        compose = (
            REPO / "shaft-engine/src/main/resources/docker-compose/selenium4.yml"
        ).read_text(encoding="utf-8")
        tags = set(
            re.findall(
                r"selenium/(?:hub|node-chrome|node-edge|node-firefox):(\S+)",
                compose,
            )
        )
        self.assertEqual(1, len(tags), f"Expected one shared Selenium Grid tag, found {tags}")
        grid_tag = next(iter(tags))
        self.assertRegex(grid_tag, TAG_PATTERN)
        self.assertNotEqual("4.48.0-20260905", grid_tag)
        planner = (
            REPO
            / "shaft-infrastructure/src/main/java/com/shaft/infrastructure/SeleniumGridSetupPlanner.java"
        ).read_text(encoding="utf-8")
        match = PLANNER_TAG_PATTERN.search(planner)
        self.assertIsNotNone(match, "SeleniumGridSetupPlanner.IMAGE_TAG declaration not found")
        self.assertEqual(grid_tag, match.group(1))
        self.assertNotIn("4.48.0-20260905", compose)

    def test_weekly_updater_rewrites_the_planner_alongside_compose(self):
        workflow = (
            REPO / ".github/workflows/update-selenium-grid-versions.yml"
        ).read_text(encoding="utf-8")
        self.assertIn("SeleniumGridSetupPlanner.java", workflow)
        self.assertIn('IMAGE_TAG = \\"', workflow)


if __name__ == "__main__":
    unittest.main()
