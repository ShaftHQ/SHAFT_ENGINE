"""Tests for scripts/ci/spotbugs_trend.py and the SpotBugs nightly (#6999)."""
from __future__ import annotations

import importlib.util
import tempfile
import unittest
from pathlib import Path

import yaml

ROOT = Path(__file__).resolve().parents[2]
SPEC = importlib.util.spec_from_file_location("spotbugs_trend", ROOT / "scripts/ci/spotbugs_trend.py")
trend = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(trend)


class SpotBugsTrendTest(unittest.TestCase):
    def test_counts_bug_instances_per_module(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            target = Path(tmp, "shaft-ai", "target")
            target.mkdir(parents=True)
            (target / "spotbugsXml.xml").write_text(
                "<BugCollection><BugInstance/><BugInstance/></BugCollection>", encoding="utf-8")
            self.assertEqual({"shaft-ai": 2}, trend.module_counts(Path(tmp)))

    def test_only_increases_regress(self) -> None:
        self.assertEqual(["a: 3 > baseline 2"], trend.regressions({"a": 3, "b": 1}, {"a": 2, "b": 5}))

    def test_nightly_is_off_the_pr_path_and_tracks(self) -> None:
        workflow = yaml.safe_load((ROOT / ".github/workflows/spotbugs-nightly.yml").read_text(encoding="utf-8"))
        triggers = workflow[True] if True in workflow else workflow["on"]
        self.assertNotIn("pull_request", triggers)
        self.assertIn("schedule", triggers)
        steps = str(workflow["jobs"]["spotbugs"]["steps"])
        self.assertIn("spotbugs_trend.py", steps)
        self.assertIn("ci-spotbugs", steps)
        self.assertIn("upload-sarif", steps)


if __name__ == "__main__":
    unittest.main()
