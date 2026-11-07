"""#7793: a retrospective close dispositions every enhancement."""

from __future__ import annotations

import importlib.util
import os
import sys
import tempfile
import unittest
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[2]))  # noqa: E402 - self-locating under python3 -I (#7197)

ROOT = Path(__file__).resolve().parents[2]
LABELS = "\n".join(
    f"{label}: recorded"
    for label in (
        "what happened",
        "why it happened",
        "continue doing",
        "stop doing",
        "start doing",
        "next action",
    )
)
BAD = """## Enhancements
- tracked: https://github.com/ShaftHQ/SHAFT_ENGINE/issues/7791
- retrieval is a separate decision
"""
GOOD = """## Enhancements
- tracked: https://github.com/ShaftHQ/SHAFT_ENGINE/issues/7791
- tracked: https://github.com/ShaftHQ/SHAFT_ENGINE/issues/7792
- rejected: hosted reranker
"""
SHIPPED = """## Enhancements
- shipped: https://github.com/ShaftHQ/SHAFT_ENGINE/pull/7790
- rejected: hosted reranker
nothing actionable
"""


def _load(name: str, path: Path):
    spec = importlib.util.spec_from_file_location(name, path)
    if spec is None or spec.loader is None:
        raise AssertionError(f"missing {path}")
    module = importlib.util.module_from_spec(spec)
    sys.modules[name] = module
    spec.loader.exec_module(module)
    return module


class EnhancementDispositionTest(unittest.TestCase):
    def setUp(self) -> None:
        self.checker = _load(
            "ce_enhancement_disposition_7793",
            ROOT / "chaos-engine/enhancement_disposition.py",
        )

    def test_night_cluster_names_the_unmarked_enhancement(self) -> None:
        errors = self.checker.enhancement_errors(BAD)
        self.assertTrue(any("retrieval is a separate decision" in item for item in errors), errors)

    def test_tracked_and_rejected_rows_pass(self) -> None:
        self.assertEqual([], self.checker.enhancement_errors(GOOD))

    def test_shipped_and_rejected_accept_nothing_actionable(self) -> None:
        self.assertEqual([], self.checker.enhancement_errors(SHIPPED))

    def test_checker_ignores_prose_outside_the_block(self) -> None:
        note = SHIPPED + "\n## Later\n- retrieval is a separate decision\n"
        self.assertEqual([], self.checker.enhancement_errors(note))

    def test_nothing_actionable_after_the_block_keeps_a_tracked_bullet(self) -> None:
        note = (
            "## Enhancements\n"
            "- tracked: https://github.com/ShaftHQ/SHAFT_ENGINE/issues/7792\n"
            "\n## Later\n"
            "nothing actionable\n"
        )
        self.assertEqual([], self.checker.enhancement_errors(note))

    def test_rejected_separate_decision_is_not_a_disposition(self) -> None:
        note = "## Enhancements\n- rejected: retrieval is a separate decision\n"
        errors = self.checker.enhancement_errors(note)
        self.assertTrue(errors, errors)


class StopAndFinalizeTest(unittest.TestCase):
    def _portable_reason(self, guard, temporary: str, session: str, note: str) -> str:
        os.environ["TMPDIR"] = temporary
        os.environ["TEMP"] = temporary
        guard.reflection.record_session_start(session, "2020-01-01T00:00:00+00:00")
        return guard._terminal_reflection_reason(
            {"session_id": session, "last_assistant_message": LABELS + "\n" + note},
            session,
        )

    def test_stop_hook_rejects_the_unmarked_note_and_accepts_the_disposition(self) -> None:
        guard = _load("ce_portable_guard_7793", ROOT / "chaos-engine/hooks/guard.py")
        with tempfile.TemporaryDirectory() as temporary:
            rejected = self._portable_reason(guard, temporary, "enh-7793-bad", BAD)
            accepted = self._portable_reason(guard, temporary, "enh-7793-good", GOOD)
            quiet = self._portable_reason(guard, temporary, "enh-7793-shipped", SHIPPED)
        self.assertIn("retrieval is a separate decision", rejected)
        self.assertEqual("", accepted)
        self.assertEqual("", quiet)

    def test_repository_stop_hook_uses_the_same_checker(self) -> None:
        import scripts.agents.guard as repo
        from unittest.mock import patch

        with (
            patch.object(repo._reflection, "session_elapsed_seconds", return_value=7200),
            patch.object(repo._reflection, "has_valid_terminal_receipt", return_value=False),
            patch.object(repo._reflection, "accept_terminal_reflection"),
        ):
            rejected = repo._terminal_reflection_reason(
                {"session_id": "enh-7793-repo", "last_assistant_message": LABELS + "\n" + BAD}
            )
            accepted = repo._terminal_reflection_reason(
                {"session_id": "enh-7793-ok", "last_assistant_message": LABELS + "\n" + GOOD}
            )
        self.assertIn("retrieval is a separate decision", rejected or "")
        self.assertIsNone(accepted)

    def test_finalize_no_durable_refuses_bad_shape_and_issues_first_records_urls(self) -> None:
        learning = _load("ce_learning_session_7793", ROOT / "chaos-engine/learning_session.py")
        previous = os.getcwd()
        with tempfile.TemporaryDirectory() as temporary:
            os.chdir(temporary)
            try:
                with self.assertRaises(ValueError) as caught:
                    learning.finalize(
                        "enh-7793",
                        disposition="no-durable",
                        extract_heuristics=False,
                        note=BAD,
                    )
                self.assertIn("retrieval is a separate decision", str(caught.exception))
                receipt = learning.finalize(
                    "enh-7793-good",
                    disposition="issues-first",
                    extract_heuristics=False,
                    note=GOOD,
                )
            finally:
                os.chdir(previous)
        tracked = receipt["trackingIssues"]
        self.assertIn("https://github.com/ShaftHQ/SHAFT_ENGINE/issues/7791", tracked)
        self.assertIn("https://github.com/ShaftHQ/SHAFT_ENGINE/issues/7792", tracked)

    def test_finalize_no_durable_accepts_shipped_and_rejected_only(self) -> None:
        learning = _load("ce_learning_session_7793b", ROOT / "chaos-engine/learning_session.py")
        previous = os.getcwd()
        with tempfile.TemporaryDirectory() as temporary:
            os.chdir(temporary)
            try:
                receipt = learning.finalize(
                    "enh-7793-quiet",
                    disposition="no-durable",
                    extract_heuristics=False,
                    note=SHIPPED,
                )
            finally:
                os.chdir(previous)
        self.assertEqual("no-durable", receipt["disposition"])
        self.assertNotIn("trackingIssues", receipt)


if __name__ == "__main__":
    unittest.main()
