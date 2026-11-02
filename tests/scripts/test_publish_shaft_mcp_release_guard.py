"""publish-shaft-mcp.yml runs for SHAFT version releases, not media releases (#7095)."""

from __future__ import annotations

import re
import unittest
from pathlib import Path

WORKFLOW = Path(__file__).resolve().parents[2] / ".github/workflows/publish-shaft-mcp.yml"
GUARD = "!contains(github.event.release.tag_name, '-')"


def publishes(tag: str) -> bool:
    """Mirror of the job condition for a `release: published` event."""
    return "-" not in tag


class PublishShaftMcpReleaseGuardTest(unittest.TestCase):
    def test_job_condition_skips_dashed_tags(self):
        self.assertIn(GUARD, WORKFLOW.read_text(encoding="utf-8"))

    def test_version_tags_publish_and_media_tags_do_not(self):
        for tag in ("10.4.20261013", "10.3.20260930", "11.0.20270101"):
            self.assertTrue(re.fullmatch(r"\d+\.\d+\.\d+", tag))
            self.assertTrue(publishes(tag), tag)
        for tag in ("shaft-hero-video-20261017", "shaft-feature-video-v2-20261009", "chaosengine-install-video-20261004"):
            self.assertFalse(publishes(tag), tag)


if __name__ == "__main__":
    unittest.main()
