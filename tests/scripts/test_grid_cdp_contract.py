"""Managed Selenium Grid images must speak a CDP major the client supports.

Selenium 4.51's bundled DevTools majors are 153, 154, and 155. A grid image
whose Edge is 152 makes createSession a no-op, so network interception fails
before the local image assertion. The compose file and the planner must share
one tag that ships Edge 153. The interception test itself stays on the local
fixture; this contract does not swallow DevToolsException.
"""

import re
import unittest
from pathlib import Path

REPO = Path(__file__).resolve().parents[2]
GRID_TAG = "4.49.0-20260909"


class GridCdpContractTest(unittest.TestCase):
    def test_compose_and_planner_share_the_edge_153_grid_tag(self):
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


if __name__ == "__main__":
    unittest.main()
