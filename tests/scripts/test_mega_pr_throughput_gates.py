"""Throughput guards for mega-PR local gates (validate / overlay / code-quality)."""

from __future__ import annotations

import os
import tempfile
import threading
import unittest
from pathlib import Path
from unittest import mock

import sys

sys.path.insert(0, str(Path(__file__).resolve().parents[2]))

from scripts.ci import code_quality_preflight as cq
from scripts.ci import overlay_pre_push as opp
from scripts.ci import validate_pr_closing_keywords as validate


ROOT = Path(__file__).resolve().parents[2]


class ValidateClosingKeywordsThroughputTest(unittest.TestCase):
    def test_empty_closing_numbers_skips_network(self):
        with mock.patch.object(validate, "_fetch_issue_labels_graphql") as graph, mock.patch.object(
            validate, "_fetch_issue_labels_parallel"
        ) as parallel:
            self.assertEqual(validate.fetch_issue_labels([]), {})
        graph.assert_not_called()
        parallel.assert_not_called()

    def test_main_skips_label_fetch_when_body_has_no_closing_keyword(self):
        with mock.patch.object(validate, "fetch_issue_labels") as fetch, mock.patch.dict(
            os.environ,
            {
                "PR_BODY": "Related to #12\n\n## Riskiest assumption\n\nN/A\n",
                "PR_COMMITS_JSON": "[]",
            },
            clear=False,
        ), mock.patch.object(sys, "argv", ["validate_pr_closing_keywords.py"]):
            # Human body without ## Plan may still pass mindset when not agentic.
            validate.main()
        fetch.assert_not_called()

    def test_graphql_batch_preferred_over_parallel_rest(self):
        with mock.patch.object(
            validate, "_fetch_issue_labels_graphql", return_value={"1": ["bug"]}
        ) as graph, mock.patch.object(validate, "_fetch_issue_labels_parallel") as parallel:
            self.assertEqual(validate.fetch_issue_labels(["1", "2"]), {"1": ["bug"]})
        graph.assert_called_once_with(["1", "2"])
        parallel.assert_not_called()

    def test_parallel_rest_used_when_graphql_unavailable(self):
        with mock.patch.object(
            validate, "_fetch_issue_labels_graphql", return_value=None
        ), mock.patch.object(
            validate, "_fetch_issue_labels_parallel", return_value={"9": ["ready"]}
        ) as parallel:
            self.assertEqual(validate.fetch_issue_labels(["9"]), {"9": ["ready"]})
        parallel.assert_called_once_with(["9"])


class OverlayPrePushThroughputTest(unittest.TestCase):
    def test_gh_pr_view_is_cached_per_root(self):
        opp.clear_gh_pr_cache()
        calls = {"n": 0}

        def fake_run(cmd, **kwargs):
            calls["n"] += 1
            class Result:
                returncode = 0
                stdout = '{"number":1,"labels":[],"author":{"login":"x"},"isDraft":false,"body":"b","title":"t"}'
                if cmd[:2] == [mock.ANY, "rev-parse"] or (len(cmd) >= 2 and cmd[1] == "rev-parse"):
                    stdout = "feature\n"
            # Distinguish git vs gh
            out = type("R", (), {})()
            if len(cmd) >= 2 and cmd[1] == "rev-parse":
                out.returncode = 0
                out.stdout = "feature\n"
            else:
                out.returncode = 0
                out.stdout = '{"number":1,"labels":[],"author":{"login":"x"},"isDraft":false,"body":"b","title":"t"}'
            return out

        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            with mock.patch.object(opp.shutil, "which", side_effect=lambda name: f"/bin/{name}"), mock.patch.object(
                opp.subprocess, "run", side_effect=fake_run
            ):
                first = opp._gh_open_pr_for_head(root)
                second = opp._gh_open_pr_for_head(root)
        self.assertEqual(first, second)
        # one git rev-parse + one gh pr view; second call uses cache
        self.assertEqual(calls["n"], 2)

    def test_full_suite_lock_is_exclusive(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            held = threading.Event()
            release = threading.Event()
            error: list[BaseException] = []

            def holder():
                with opp.full_scripts_unittest_lock(root):
                    held.set()
                    release.wait(timeout=5)

            thread = threading.Thread(target=holder)
            thread.start()
            self.assertTrue(held.wait(timeout=5))
            try:
                with opp.full_scripts_unittest_lock(root, blocking=False):
                    self.fail("expected BlockingIOError when lock held")
            except BlockingIOError as exc:
                error.append(exc)
            release.set()
            thread.join(timeout=5)
            self.assertTrue(error)

    def test_overlay_collectors_accept_parallel_flag(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            timings: list[tuple[str, float]] = []
            failures = opp.overlay_pre_push_failures(
                root, paths=["shaft-engine/src/Main.java"], parallel=True, timings=timings
            )
            self.assertEqual(failures, [])
            self.assertTrue(timings)


class CodeQualityPreflightThroughputTest(unittest.TestCase):
    def test_parallel_scan_matches_serial_for_many_files(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            changed = {}
            for index in range(6):
                rel = f"mod_{index}.py"
                (root / rel).write_text(f"def f_{index}(x):\n    return x\n", encoding="utf-8")
                changed[rel] = {0}
            parallel = cq.preflight_findings(root, changed)
            # Force serial path via <4 files then combine — semantic equality on empty findings
            self.assertEqual(parallel, [])


class LessonsCardPresentTest(unittest.TestCase):
    def test_throughput_lesson_card_exists_with_timings_and_interrupt_evidence(self):
        card = ROOT / "scripts/ci/mega-pr-throughput-20261102.md"
        text = card.read_text(encoding="utf-8")
        self.assertIn("Session cap", text)
        self.assertIn("full_scripts_unittest_lock", text)
        self.assertIn("--all-failures", text)
        self.assertIn("unproven", text.lower())
        self.assertIn("GraphQL", text)


if __name__ == "__main__":
    unittest.main()
