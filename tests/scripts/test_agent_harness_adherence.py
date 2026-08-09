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


if __name__ == "__main__":
    unittest.main()
