"""#6544: insight extraction from success/failure pairs (ExpeL + ReasoningBank)."""

from __future__ import annotations

import importlib.util
import pathlib
import tempfile
import time
import unittest

ROOT = pathlib.Path(__file__).resolve().parents[2]


def load(path: pathlib.Path, name: str):
    spec = importlib.util.spec_from_file_location(name, path)
    if spec is None or spec.loader is None:
        raise RuntimeError(f"cannot load module from {path}")
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


class InsightExtractTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.insights = load(
            ROOT / "chaos-engine/insight_extract.py", "ce_insight_6544"
        )
        cls.heuristics = load(
            ROOT / "chaos-engine/heuristics.py", "ce_heuristics_6544"
        )

    def _project(self, tmp: str) -> pathlib.Path:
        project = pathlib.Path(tmp)
        (project / ".chaos-engine").mkdir()
        (project / ".chaos-engine" / "install.py").write_text("# probe\n", encoding="utf-8")
        return project

    def test_reference_and_permanent_rule(self):
        refs = ROOT / "chaos-engine" / "references"
        text = (refs / "insight-extract.md").read_text(encoding="utf-8")
        self.assertIn("ExpeL", text)
        self.assertIn("ReasoningBank", text)
        self.assertIn("importance", text)
        permanent = (refs / "permanent-rules.md").read_text(encoding="utf-8")
        self.assertIn("insight-extract.md", permanent)

    def test_pair_requires_both_outcomes(self):
        with tempfile.TemporaryDirectory() as tmp:
            project = self._project(tmp)
            self.insights.record_experience(
                "gate-reg", "failure", "Forgot to register new test module", project=project
            )
            with self.assertRaises(ValueError):
                self.insights.record_pair_insight(
                    "gate-reg",
                    "Register new harness tests with the PR gate",
                    project=project,
                )
            self.insights.record_experience(
                "gate-reg", "success", "Registered test then pushed", project=project
            )
            item = self.insights.record_pair_insight(
                "gate-reg",
                "Register new harness tests with the PR gate",
                project=project,
            )
            self.assertEqual(item["importance"], 2)
            self.assertEqual(item["kind"], "pair")

    def test_operators_lifecycle(self):
        with tempfile.TemporaryDirectory() as tmp:
            project = self._project(tmp)
            added = self.insights.apply_operator(
                "ADD", text="Prefer event wake over CI poll", project=project
            )
            self.assertEqual(added["importance"], 2)
            up = self.insights.apply_operator(
                "UPVOTE", insight_id=added["id"], project=project
            )
            self.assertEqual(up["importance"], 3)
            edited = self.insights.apply_operator(
                "EDIT",
                insight_id=added["id"],
                text="Prefer event wake; never LLM-poll CI",
                project=project,
            )
            self.assertEqual(edited["importance"], 4)
            self.assertIn("never LLM-poll", edited["text"])
            # Downvote down to removal.
            for expected in (3, 2, 1):
                left = self.insights.apply_operator(
                    "DOWNVOTE", insight_id=added["id"], project=project
                )
                self.assertEqual(left["importance"], expected)
            gone = self.insights.apply_operator(
                "DOWNVOTE", insight_id=added["id"], project=project
            )
            self.assertIsNone(gone)
            with self.assertRaises(ValueError):
                self.insights.apply_operator(
                    "UPVOTE", insight_id=added["id"], project=project
                )

    def test_failure_distill_and_retrieve_rank(self):
        with tempfile.TemporaryDirectory() as tmp:
            project = self._project(tmp)
            self.insights.record_experience(
                "install-slow",
                "failure",
                "Healthy rerun still redownloaded deps",
                project=project,
            )
            low = self.insights.distill_failure(
                "install-slow",
                "Skip Provision when account deps already healthy",
                project=project,
            )
            time.sleep(0.01)
            high = self.insights.apply_operator(
                "ADD", text="Reuse healthy tooling when registry is down", project=project
            )
            self.insights.apply_operator("UPVOTE", insight_id=high["id"], project=project)
            self.insights.apply_operator("UPVOTE", insight_id=high["id"], project=project)
            top = self.insights.retrieve_top(2, project)
            self.assertEqual(top[0]["id"], high["id"])
            self.assertGreaterEqual(top[0]["importance"], low["importance"])

    def test_promote_to_playbook(self):
        with tempfile.TemporaryDirectory() as tmp:
            project = self._project(tmp)
            insight = self.insights.apply_operator(
                "ADD",
                text="Claim issues before starting SHAFT work",
                project=project,
            )
            heuristic = self.insights.promote_to_playbook(insight["id"], project=project)
            self.assertEqual(heuristic["text"], insight["text"])
            self.assertEqual(heuristic["source"], "insight-extract")
            stored = self.heuristics.retrieve_top(1, project)
            self.assertEqual(stored[0]["id"], heuristic["id"])

    def test_privacy_gate(self):
        with tempfile.TemporaryDirectory() as tmp:
            project = self._project(tmp)
            with self.assertRaises(ValueError):
                self.insights.apply_operator(
                    "ADD",
                    text="Never paste token ghp_abcdefghijklmnopqrstuvwxyz01",
                    project=project,
                )

    def test_empty_store_summary(self):
        with tempfile.TemporaryDirectory() as tmp:
            project = self._project(tmp)
            summary = self.insights.doctor_insights_summary(project)
            self.assertEqual(summary["status"], "absent")
            self.assertEqual(summary["insightCount"], 0)


if __name__ == "__main__":
    unittest.main()
