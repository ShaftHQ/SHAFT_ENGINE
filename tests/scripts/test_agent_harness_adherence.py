"""Regression coverage for the deterministic agent-harness adherence corpus."""

from __future__ import annotations

import importlib
import json
import tempfile
import unittest
from copy import deepcopy
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
FIXTURES = ROOT / "tests" / "fixtures" / "agent_harness_adherence"

try:
    adherence = importlib.import_module("scripts.ci.agent_harness_adherence")
except ModuleNotFoundError:
    adherence = None


class AgentHarnessAdherenceTest(unittest.TestCase):
    def load_fixture(self, name: str) -> dict:
        return json.loads((FIXTURES / name).read_text(encoding="utf-8"))

    def test_validate_corpus_accepts_the_reviewed_fixture(self) -> None:
        validator = getattr(adherence, "validate_corpus", None)
        self.assertTrue(callable(validator), "the corpus validator must be available")
        corpus = json.loads((FIXTURES / "corpus.json").read_text(encoding="utf-8"))
        self.assertEqual([], validator(corpus))

    def test_rejects_workspace_paths_that_escape_the_episode_root(self) -> None:
        validator = getattr(adherence, "validate_corpus", None)
        self.assertTrue(callable(validator), "the corpus validator must be available")
        corpus = json.loads((FIXTURES / "corpus.json").read_text(encoding="utf-8"))
        corpus = deepcopy(corpus)
        corpus["episodes"][0]["workspace"]["files"] = {
            "../outside": "not allowed",
            "C:/outside": "not allowed",
        }

        errors = validator(corpus)

        self.assertTrue(
            any("escape workspace path" in error for error in errors),
            errors,
        )

    def test_materializes_workspace_files_inside_the_given_directory(self) -> None:
        materialize = getattr(adherence, "materialize_workspace", None)
        self.assertTrue(callable(materialize), "the workspace materializer must be available")
        corpus = json.loads((FIXTURES / "corpus.json").read_text(encoding="utf-8"))

        with tempfile.TemporaryDirectory() as temporary_directory:
            directory = Path(temporary_directory)
            workspace = materialize(corpus["episodes"][0], directory)

            self.assertIsInstance(workspace, Path)
            self.assertTrue(workspace.is_relative_to(directory))
            self.assertEqual(
                {Path("source/task.md")},
                {path.relative_to(workspace) for path in workspace.rglob("*") if path.is_file()},
            )
            self.assertEqual("Inspect the task.", (workspace / "source/task.md").read_text(encoding="utf-8"))

    def test_materializer_refuses_an_escape_before_writing_outside_its_root(self) -> None:
        materialize = getattr(adherence, "materialize_workspace", None)
        self.assertTrue(callable(materialize), "the workspace materializer must be available")

        with tempfile.TemporaryDirectory() as temporary_directory:
            directory = Path(temporary_directory) / "root"
            directory.mkdir()
            outside = Path(temporary_directory) / "outside.txt"
            episode = {"workspace": {"files": {"../outside.txt": "must not write"}}}

            with self.assertRaises(ValueError):
                materialize(episode, directory)

            self.assertFalse(outside.exists())

    def test_rejects_windows_ambiguous_workspace_file_names(self) -> None:
        validator = getattr(adherence, "validate_corpus", None)
        self.assertTrue(callable(validator), "the corpus validator must be available")
        corpus = json.loads((FIXTURES / "corpus.json").read_text(encoding="utf-8"))
        corpus = deepcopy(corpus)
        corpus["episodes"][0]["workspace"]["files"] = {
            "C:drive-relative": "not allowed",
            "link:stream": "not allowed",
            "NUL": "not allowed",
            "name. ": "not allowed",
        }

        errors = validator(corpus)

        self.assertTrue(
            any("escape workspace path" in error for error in errors),
            errors,
        )

    def test_evaluates_required_prohibited_and_guard_expectations(self) -> None:
        evaluator = getattr(adherence, "evaluate", None)
        self.assertTrue(callable(evaluator), "the evidence evaluator must be available")

        report = evaluator(self.load_fixture("corpus.json"), self.load_fixture("baseline.json"))

        self.assertTrue(report["episodes"]["short-required-entrypoint"]["strict_episode_pass"])
        self.assertTrue(report["episodes"]["long-prohibited-heredoc"]["strict_episode_pass"])
        self.assertTrue(report["episodes"]["medium-guard-remedy"]["strict_episode_pass"])
        self.assertEqual(
            {"passed": 1, "total": 1},
            report["rules"]["entrypoint"]["required_action_adherence"],
        )
        self.assertEqual(
            {"passed": 1, "total": 1},
            report["rules"]["r23"]["prohibited_action_adherence"],
        )
        self.assertEqual([], report["unmeasured_rule_ids"])

    def test_marks_incomplete_action_evidence_as_unknown(self) -> None:
        evaluator = getattr(adherence, "evaluate", None)
        self.assertTrue(callable(evaluator), "the evidence evaluator must be available")
        evidence = self.load_fixture("baseline.json")
        evidence["long-prohibited-heredoc"] = {}

        report = evaluator(self.load_fixture("corpus.json"), evidence)

        episode = report["episodes"]["long-prohibited-heredoc"]
        self.assertIsNone(episode["strict_episode_pass"])
        self.assertEqual(
            {"passed": 0, "total": 0},
            report["rules"]["r23"]["prohibited_action_adherence"],
        )
        self.assertEqual(["r23"], report["unmeasured_rule_ids"])

    def test_guard_metrics_require_a_remedy_and_count_each_block_once(self) -> None:
        evaluator = getattr(adherence, "evaluate", None)
        self.assertTrue(callable(evaluator), "the evidence evaluator must be available")
        corpus = self.load_fixture("corpus.json")
        evidence = self.load_fixture("baseline.json")
        corpus = deepcopy(corpus)
        corpus["episodes"][3]["expectations"].append(
            {"kind": "guard", "outcome": "silent", "remedy": "none"}
        )
        evidence["medium-guard-remedy"] = {
            "actions": [],
            "guard_outcomes": [{"outcome": "reports"}],
        }
        evidence["medium-false-block"] = {
            "actions": [],
            "guard_outcomes": [{"outcome": "blocks", "remedy": "inspect worktree"}],
        }

        try:
            report = evaluator(corpus, evidence)
        except TypeError as error:
            self.fail(f"incomplete guard evidence must not crash evaluation: {error}")

        self.assertFalse(report["episodes"]["medium-guard-remedy"]["strict_episode_pass"])
        self.assertEqual(1, report["guard_metrics"]["false_block_count"])
        self.assertEqual(1, report["guard_metrics"]["actionable_remedy_count"])

    def test_marks_incomplete_guard_evidence_unknown_and_rejects_missing_remedies(self) -> None:
        evaluator = getattr(adherence, "evaluate", None)
        self.assertTrue(callable(evaluator), "the evidence evaluator must be available")
        corpus = self.load_fixture("corpus.json")
        evidence = self.load_fixture("baseline.json")
        evidence["medium-guard-remedy"] = {"actions": [], "guard_outcomes": None}
        evidence["medium-false-block"] = {
            "actions": [],
            "guard_outcomes": [{"outcome": "silent"}],
        }

        try:
            report = evaluator(corpus, evidence)
        except TypeError as error:
            self.fail(f"incomplete guard evidence must not crash evaluation: {error}")

        self.assertIsNone(report["episodes"]["medium-guard-remedy"]["strict_episode_pass"])
        self.assertFalse(report["episodes"]["medium-false-block"]["strict_episode_pass"])
        self.assertEqual(["r24"], report["unmeasured_rule_ids"])


if __name__ == "__main__":
    unittest.main()
