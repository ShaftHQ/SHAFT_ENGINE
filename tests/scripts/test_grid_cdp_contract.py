"""Managed Selenium Grid images must speak a CDP major the client supports.

Selenium 4.52's bundled DevTools majors are 154, 155, and 156. A grid image
whose Edge is 152 makes createSession a no-op, so network interception fails
before the local image assertion. The compose file and the planner must share
one tag. This tag ships Edge 156. The same image's Chrome is 157. The
interception test itself stays on the local fixture; this contract does not
swallow DevToolsException.
"""

import re
import unittest
from pathlib import Path

REPO = Path(__file__).resolve().parents[2]
GRID_TAG = "4.52.0-20261101"


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
        self.assertEqual({GRID_TAG}, tags)
        planner = (
            REPO
            / "shaft-infrastructure/src/main/java/com/shaft/infrastructure/SeleniumGridSetupPlanner.java"
        ).read_text(encoding="utf-8")
        self.assertIn(f'IMAGE_TAG = "{GRID_TAG}"', planner)
        self.assertNotIn("4.48.0-20260905", compose)

    def test_weekly_updater_rewrites_the_planner_and_the_contract(self):
        workflow = (
            REPO / ".github/workflows/update-selenium-grid-versions.yml"
        ).read_text(encoding="utf-8")
        self.assertIn("SeleniumGridSetupPlanner.java", workflow)
        self.assertIn("test_grid_cdp_contract.py", workflow)
        self.assertIn('IMAGE_TAG = \\"', workflow)
        self.assertIn('GRID_TAG = \\"', workflow)


if __name__ == "__main__":
    unittest.main()
