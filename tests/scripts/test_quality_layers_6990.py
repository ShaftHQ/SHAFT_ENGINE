"""Quality layers for #6990: blocking diff-scoped analysis, weekly trend."""

import importlib.util
import unittest
from pathlib import Path

import yaml

ROOT = Path(__file__).resolve().parents[2]
spec = importlib.util.spec_from_file_location("qt", ROOT / "scripts/ci/quality_trend.py")
qt = importlib.util.module_from_spec(spec)
spec.loader.exec_module(qt)


class QualityLayersTest(unittest.TestCase):
    def test_static_analysis_blocks_through_the_summary(self) -> None:
        jobs = yaml.safe_load((ROOT / ".github/workflows/pr-gate.yml").read_text(encoding="utf-8"))["jobs"]
        self.assertIn("static-analysis", jobs["summary"]["needs"])
        run = " ".join(step.get("run", "") for step in jobs["static-analysis"]["steps"])
        self.assertIn("code_quality_preflight.py --base", run)

    def test_codacy_failure_or_coverage_drop_is_a_regression(self) -> None:
        rows = qt.codacy_rows([{"conclusion": "failure", "output": {"title": "2 new issues (0 max.)"}}])
        self.assertEqual(2, rows[0]["new_issues"])
        self.assertTrue(qt.summarize(rows, 70.0, 61.0)["regression"])
        self.assertTrue(qt.summarize([], 60.0, 61.0)["regression"])
        self.assertFalse(qt.summarize([], 62.0, 61.0)["regression"])
        # #7001: a Scorecard score below its floor is a regression too.
        self.assertTrue(qt.summarize([], 62.0, 61.0, 5.5, 6.0)["regression"])
        self.assertFalse(qt.summarize([], 62.0, 61.0, None, 6.0)["regression"])

    def test_weekly_workflow_tracks_quality_regressions(self) -> None:
        text = (ROOT / ".github/workflows/pr-time-to-merge.yml").read_text(encoding="utf-8")
        self.assertIn("quality_trend.py", text)
        self.assertIn("ci-quality", text)


    def test_coverage_statuses_block_and_carry_forward(self) -> None:
        config = yaml.safe_load((ROOT / "codecov.yml").read_text(encoding="utf-8"))
        status = config["coverage"]["status"]
        self.assertEqual("80%", status["patch"]["default"]["target"])
        self.assertFalse(status["patch"]["default"]["informational"])
        self.assertEqual("0.5%", status["project"]["default"]["threshold"])
        self.assertFalse(status["project"]["default"]["informational"])
        self.assertTrue(config["flag_management"]["default_rules"]["carryforward"])
        action = (ROOT / ".github/actions/upload-jacoco-coverage/action.yml").read_text(encoding="utf-8")
        self.assertIn('"$GITHUB_EVENT_NAME" = "pull_request"', action)
        self.assertIn("flags: ${{ inputs.flags }}", action)

    def test_main_push_uploads_every_coverage_flag(self) -> None:
        workflow = yaml.safe_load((ROOT / ".github/workflows/pr-gate.yml").read_text(encoding="utf-8"))
        self.assertIn("refs/heads/main", workflow["jobs"]["changes"]["outputs"]["coverage_all"])
        for job in ("intellij-build", "cli", "capture-e2e", "unit-tests", "template-coupling"):
            self.assertIn("needs.changes.outputs.coverage_all == 'true'", workflow["jobs"][job]["if"], job)

    def test_pr_uploads_carry_forward_from_the_pr_base(self) -> None:
        action = (ROOT / ".github/actions/upload-jacoco-coverage/action.yml").read_text(encoding="utf-8")
        self.assertIn("commit_parent: ${{ github.event.pull_request.base.sha }}", action)
        config = yaml.safe_load((ROOT / "codecov.yml").read_text(encoding="utf-8"))
        flags = {f["name"] for f in config["flag_management"]["individual_flags"] if f["carryforward"]}
        workflow = (ROOT / ".github/workflows/pr-gate.yml").read_text(encoding="utf-8")
        for name in ("shaft-cli", "shaft-intellij", "templates", "shaft-capture-e2e"):
            self.assertIn(name, flags)
            self.assertIn(f"flags: {name}", workflow)

    def test_codecov_yml_uses_only_schema_keys(self) -> None:
        # An unknown key makes Codecov reject the WHOLE file and fall back to
        # defaults (no carryforward, no 80% patch target): `parsers.xml` did.
        # Validate changes with `curl --data-binary @codecov.yml https://codecov.io/validate`.
        config = yaml.safe_load((ROOT / "codecov.yml").read_text(encoding="utf-8"))
        top = {"codecov", "coverage", "flag_management", "component_management", "parsers",
               "comment", "ignore", "fixes", "flags", "github_checks", "profiling", "slack_app", "beta_groups"}
        self.assertEqual(set(), set(config) - top)
        parsers = {"gcov", "go", "javascript", "jacoco", "cobertura", "v1"}
        self.assertEqual(set(), set(config.get("parsers") or {}) - parsers)

    def test_scorecard_workflow_publishes_and_uploads(self) -> None:
        workflow = yaml.safe_load((ROOT / ".github/workflows/scorecard.yml").read_text(encoding="utf-8"))
        triggers = workflow[True] if True in workflow else workflow["on"]
        self.assertNotIn("pull_request", triggers)
        steps = str(workflow["jobs"]["analysis"]["steps"])
        self.assertIn("ossf/scorecard-action@", steps)
        self.assertIn("'publish_results': True", steps)
        self.assertIn("upload-sarif", steps)
        self.assertIn("ossf-scorecard", (ROOT / "README.md").read_text(encoding="utf-8"))


if __name__ == "__main__":
    unittest.main()


class ScorecardFloorRatchetTest(unittest.TestCase):
    def test_floor_is_ratcheted_above_zero(self) -> None:
        import importlib.util
        spec = importlib.util.spec_from_file_location("qt", ROOT / "scripts/ci/quality_trend.py")
        mod = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(mod)
        self.assertGreaterEqual(mod.SCORECARD_FLOOR, 4.0)
