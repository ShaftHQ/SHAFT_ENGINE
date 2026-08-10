"""Regression coverage for the deterministic agent-harness adherence corpus."""

from __future__ import annotations

import importlib
import json
import subprocess
import sys
import tempfile
import unittest
from copy import deepcopy
from pathlib import Path
from unittest.mock import patch


ROOT = Path(__file__).resolve().parents[2]
FIXTURES = ROOT / "tests" / "fixtures" / "agent_harness_adherence"
SCRIPT = ROOT / "scripts" / "ci" / "agent_harness_adherence.py"

try:
    adherence = importlib.import_module("scripts.ci.agent_harness_adherence")
except ModuleNotFoundError:
    adherence = None


class AgentHarnessAdherenceTest(unittest.TestCase):
    def load_fixture(self, name: str) -> dict:
        return json.loads((FIXTURES / name).read_text(encoding="utf-8"))

    def run_cli(self, *arguments: Path | str) -> subprocess.CompletedProcess[str]:
        return subprocess.run(  # nosec B603 B607 - fixed interpreter and repository script.
            [sys.executable, str(SCRIPT), *(str(argument) for argument in arguments)],
            check=False,
            capture_output=True,
            text=True,
        )

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

    def test_materializer_preserves_the_callers_workspace_path_spelling(self) -> None:
        corpus = self.load_fixture("corpus.json")
        episode = corpus["episodes"][0]
        with tempfile.TemporaryDirectory() as temporary_directory:
            directory = Path(temporary_directory)
            actual_workspace = directory / "workspace"
            actual_workspace.mkdir()
            caller_spelling = actual_workspace / ".." / "workspace"
            with patch.object(adherence.tempfile, "mkdtemp", return_value=str(caller_spelling)):
                workspace = adherence.materialize_workspace(episode, directory)

            self.assertEqual(Path(caller_spelling), workspace)
            self.assertTrue(workspace.is_relative_to(directory))

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
        self.assertTrue(
            report["episodes"]["long-research-before-implementation"]["strict_episode_pass"]
        )
        self.assertEqual(
            {"passed": 1, "total": 1},
            report["rules"]["entrypoint"]["required_action_adherence"],
        )
        self.assertEqual(
            {"passed": 1, "total": 1},
            report["rules"]["r23"]["prohibited_action_adherence"],
        )
        self.assertEqual([], report["unmeasured_rule_ids"])

    def test_research_sequence_rejects_each_missing_or_late_preflight_action(self) -> None:
        evaluator = getattr(adherence, "evaluate", None)
        self.assertTrue(callable(evaluator), "the evidence evaluator must be available")
        corpus = self.load_fixture("corpus.json")
        baseline = self.load_fixture("baseline.json")
        identifier = "long-research-before-implementation"
        ordered = next(
            episode["expectations"][0]["ordered"]
            for episode in corpus["episodes"]
            if episode["id"] == identifier
        )

        for missing in ordered:
            with self.subTest(missing=missing):
                evidence = deepcopy(baseline)
                evidence[identifier]["actions"].remove(missing)
                self.assertFalse(evaluator(corpus, evidence)["episodes"][identifier]["strict_episode_pass"])

        evidence = deepcopy(baseline)
        actions = evidence[identifier]["actions"]
        actions.remove("authoritative-online-research")
        actions.append("authoritative-online-research")
        self.assertFalse(evaluator(corpus, evidence)["episodes"][identifier]["strict_episode_pass"])

    def test_research_sequence_allows_analysis_without_an_implementation_mutation(self) -> None:
        corpus = self.load_fixture("corpus.json")
        evidence = self.load_fixture("baseline.json")
        identifier = "long-research-before-implementation"
        evidence[identifier]["actions"].remove("implementation-mutation")

        self.assertTrue(adherence.evaluate(corpus, evidence)["episodes"][identifier]["strict_episode_pass"])

    def test_caveman_evidence_rejects_changed_negation_number_unit_or_command(self) -> None:
        corpus = self.load_fixture("corpus.json")
        baseline = self.load_fixture("baseline.json")
        identifier = "short-caveman-preserves-exact-facts"
        self.assertTrue(adherence.evaluate(corpus, baseline)["episodes"][identifier]["strict_episode_pass"])
        for response in (
            "Do retry; wait 15 ms; run mvn -q test.",
            "Retry now, not later; wait 15 ms; run mvn -q test.",
            "Do not retry; wait 16 ms; run mvn -q test.",
            "Do not retry; wait 15 seconds; run mvn -q test.",
            "Do not retry; wait 15 ms; run mvn test.",
        ):
            with self.subTest(response=response):
                evidence = deepcopy(baseline)
                evidence[identifier]["response"] = response
                self.assertFalse(adherence.evaluate(corpus, evidence)["episodes"][identifier]["strict_episode_pass"])

    def test_ponytail_evidence_rejects_skipping_the_first_viable_rung(self) -> None:
        corpus = self.load_fixture("corpus.json")
        baseline = self.load_fixture("baseline.json")
        identifier = "short-ponytail-first-viable-rung"
        self.assertTrue(adherence.evaluate(corpus, baseline)["episodes"][identifier]["strict_episode_pass"])
        for chosen in ("existing-owner", "native-platform", "installed-dependency", "minimum-new-code"):
            with self.subTest(chosen=chosen):
                evidence = deepcopy(baseline)
                evidence[identifier]["chosen_action"] = chosen
                self.assertFalse(adherence.evaluate(corpus, evidence)["episodes"][identifier]["strict_episode_pass"])

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

    def test_comparison_fails_the_release_gate_for_a_prohibition_regression(self) -> None:
        evaluator = getattr(adherence, "evaluate", None)
        comparator = getattr(adherence, "compare", None)
        self.assertTrue(callable(evaluator), "the evidence evaluator must be available")
        self.assertTrue(callable(comparator), "the report comparator must be available")
        corpus = self.load_fixture("corpus.json")
        baseline = evaluator(corpus, self.load_fixture("baseline.json"))
        candidate = evaluator(corpus, self.load_fixture("candidate_regression.json"))

        comparison = comparator(baseline, candidate)

        self.assertEqual(["long-prohibited-heredoc"], comparison["prohibition_regressions"])
        self.assertFalse(comparison["release_gate_passed"])

    def test_comparison_fails_closed_for_incompatible_reports(self) -> None:
        comparator = getattr(adherence, "compare", None)
        self.assertTrue(callable(comparator), "the report comparator must be available")
        baseline = {
            "episodes": {
                "episode": {
                    "expectations": [
                        {"kind": "requires", "passed": True},
                        {"kind": "forbids", "passed": True},
                    ]
                }
            }
        }
        candidate = {
            "episodes": {
                "episode": {
                    "expectations": [
                        {"kind": "forbids", "passed": False},
                        {"kind": "requires", "passed": True},
                    ]
                }
            }
        }

        comparison = comparator(baseline, candidate)

        self.assertFalse(comparison["release_gate_passed"])
        self.assertTrue(comparison.get("comparison_errors"), comparison)

    def test_comparison_fails_closed_for_malformed_matching_expectations(self) -> None:
        comparator = getattr(adherence, "compare", None)
        self.assertTrue(callable(comparator), "the report comparator must be available")

        comparison = comparator(
            {"episodes": {"episode": {"expectations": [None]}}},
            {"episodes": {"episode": {"expectations": [None]}}},
        )

        self.assertFalse(comparison["release_gate_passed"])
        self.assertTrue(comparison["comparison_errors"])

    def test_comparison_rejects_non_boolean_passed_values(self) -> None:
        comparator = getattr(adherence, "compare", None)
        self.assertTrue(callable(comparator), "the report comparator must be available")

        comparison = comparator(
            {"episodes": {"episode": {"expectations": [{"kind": "forbids", "passed": 1}]}}},
            {"episodes": {"episode": {"expectations": [{"kind": "forbids", "passed": 1}]}}},
        )

        self.assertFalse(comparison["release_gate_passed"])
        self.assertTrue(comparison["comparison_errors"])

    def test_cli_reports_a_prohibition_regression_with_exit_one(self) -> None:
        completed = self.run_cli(
            "--corpus",
            FIXTURES / "corpus.json",
            "--baseline",
            FIXTURES / "baseline.json",
            "--candidate",
            FIXTURES / "candidate_regression.json",
        )

        self.assertEqual(1, completed.returncode, completed.stderr)
        report = json.loads(completed.stdout)
        self.assertEqual(["long-prohibited-heredoc"], report["prohibition_regressions"])

    def test_comparison_fails_closed_when_candidate_prohibition_evidence_is_unknown(self) -> None:
        corpus = self.load_fixture("corpus.json")
        baseline = adherence.evaluate(corpus, self.load_fixture("baseline.json"))
        candidate_evidence = self.load_fixture("baseline.json")
        candidate_evidence["long-prohibited-heredoc"] = {}
        candidate = adherence.evaluate(corpus, candidate_evidence)

        comparison = adherence.compare(baseline, candidate)

        self.assertFalse(comparison["release_gate_passed"])
        self.assertEqual(["long-prohibited-heredoc"], comparison["prohibition_regressions"])
        self.assertEqual(["r23"], comparison["prohibition_regression_rule_ids"])

    def test_validator_rejects_boolean_schema_version(self) -> None:
        corpus = self.load_fixture("corpus.json")
        corpus["schema_version"] = True

        self.assertIn("schema_version must be 1", adherence.validate_corpus(corpus))

    def test_cli_requires_valid_evidence_and_accepts_json_flag(self) -> None:
        with tempfile.TemporaryDirectory() as temporary_directory:
            malformed = Path(temporary_directory) / "evidence.json"
            malformed.write_text('{"short-required-entrypoint": {"actions": "not-a-list"}}', encoding="utf-8")

            invalid = self.run_cli(
                "--corpus", FIXTURES / "corpus.json", "--evidence", malformed
            )
            comparison = self.run_cli(
                "--corpus", FIXTURES / "corpus.json", "--baseline", FIXTURES / "baseline.json",
                "--candidate", FIXTURES / "candidate_regression.json", "--json"
            )

        self.assertEqual(2, invalid.returncode, invalid.stderr)
        self.assertEqual(1, comparison.returncode, comparison.stderr)

    def test_cli_keeps_missing_episode_observations_unknown(self) -> None:
        with tempfile.TemporaryDirectory() as temporary_directory:
            incomplete = Path(temporary_directory) / "incomplete.json"
            incomplete.write_text("{}", encoding="utf-8")

            single = self.run_cli(
                "--corpus", FIXTURES / "corpus.json", "--evidence", incomplete
            )
            comparison = self.run_cli(
                "--corpus", FIXTURES / "corpus.json", "--baseline", FIXTURES / "baseline.json",
                "--candidate", incomplete
            )

        self.assertEqual(0, single.returncode, single.stderr)
        self.assertEqual(
            sorted({rule_id for episode in self.load_fixture("corpus.json")["episodes"] for rule_id in episode["rule_ids"]}),
            json.loads(single.stdout)["unmeasured_rule_ids"],
        )
        self.assertEqual(1, comparison.returncode, comparison.stderr)

    def test_comparison_fails_closed_for_unmeasured_baseline_or_candidate(self) -> None:
        corpus = self.load_fixture("corpus.json")
        unknown = adherence.evaluate(corpus, {})

        comparison = adherence.compare(unknown, unknown)

        self.assertFalse(comparison["release_gate_passed"])
        self.assertTrue(comparison["comparison_errors"])


if __name__ == "__main__":
    unittest.main()
