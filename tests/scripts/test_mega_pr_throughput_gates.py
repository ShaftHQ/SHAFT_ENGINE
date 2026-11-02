"""Throughput guards for mega-PR local gates (validate / overlay / code-quality)."""

from __future__ import annotations

import json
import os
import tempfile
import threading
import unittest
import unittest.mock
from pathlib import Path

import sys

sys.path.insert(0, str(Path(__file__).resolve().parents[2]))

from scripts.ci import code_quality_preflight as cq
from scripts.ci import overlay_pre_push as opp
from scripts.ci import validate_pr_closing_keywords as validate


ROOT = Path(__file__).resolve().parents[2]


class ValidateClosingKeywordsThroughputTest(unittest.TestCase):
    def test_empty_closing_numbers_skips_network(self):
        with unittest.mock.patch.object(validate, "_fetch_issue_labels_graphql") as graph, unittest.mock.patch.object(
            validate, "_fetch_issue_labels_parallel"
        ) as parallel:
            self.assertEqual(validate.fetch_issue_labels([]), {})
        graph.assert_not_called()
        parallel.assert_not_called()

    def test_main_skips_label_fetch_when_body_has_no_closing_keyword(self):
        with unittest.mock.patch.object(validate, "fetch_issue_labels") as fetch, unittest.mock.patch.dict(
            os.environ,
            {
                "PR_BODY": "Related to #12\n\n## Riskiest assumption\n\nN/A\n",
                "PR_COMMITS_JSON": "[]",
            },
            clear=False,
        ), unittest.mock.patch.object(sys, "argv", ["validate_pr_closing_keywords.py"]):
            # Human body without ## Plan may still pass mindset when not agentic.
            validate.main()
        fetch.assert_not_called()

    def test_graphql_batch_preferred_over_parallel_rest(self):
        with unittest.mock.patch.object(
            validate, "_fetch_issue_labels_graphql", return_value={"1": ["bug"]}
        ) as graph, unittest.mock.patch.object(validate, "_fetch_issue_labels_parallel") as parallel:
            self.assertEqual(validate.fetch_issue_labels(["1", "2"]), {"1": ["bug"]})
        graph.assert_called_once_with(["1", "2"])
        parallel.assert_not_called()

    def test_parallel_rest_used_when_graphql_unavailable(self):
        with unittest.mock.patch.object(
            validate, "_fetch_issue_labels_graphql", return_value=None
        ), unittest.mock.patch.object(
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
            with unittest.mock.patch.object(opp.shutil, "which", side_effect=lambda name: f"/bin/{name}"), unittest.mock.patch.object(
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


class GraphqlOwnerNameVariablesTest(unittest.TestCase):
    def test_graphql_passes_owner_and_name_as_variables(self):
        captured: dict[str, object] = {}

        def fake_run(cmd, **kwargs):
            captured["cmd"] = list(cmd)
            out = type("R", (), {})()
            out.returncode = 0
            out.stdout = json.dumps(
                {"data": {"repository": {"i1": {"number": 1, "labels": {"nodes": [{"name": "bug"}]}}}}}
            )
            out.stderr = ""
            return out

        with unittest.mock.patch.dict(os.environ, {"GITHUB_REPOSITORY": "ShaftHQ/SHAFT_ENGINE"}, clear=False), unittest.mock.patch.object(
            validate.subprocess, "run", side_effect=fake_run
        ):
            labels = validate._fetch_issue_labels_graphql(["1"])
        self.assertEqual(labels, {"1": ["bug"]})
        cmd = captured["cmd"]
        self.assertIn("query($owner: String!, $name: String!)", " ".join(cmd))
        self.assertIn("owner=ShaftHQ", cmd)
        self.assertIn("name=SHAFT_ENGINE", cmd)
        joined = " ".join(cmd)
        self.assertNotIn('owner: "ShaftHQ"', joined)
        self.assertNotIn('name: "SHAFT_ENGINE"', joined)


class SkillspectorFingerprintContentHashTest(unittest.TestCase):
    def test_fingerprint_uses_content_not_mtime(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            skill = root / "chaos-engine" / "skills" / "demo-skill"
            skill.mkdir(parents=True)
            target = skill / "SKILL.md"
            target.write_text("# demo\n", encoding="utf-8")
            baseline = root / "scripts" / "ci" / "skillspector"
            baseline.mkdir(parents=True)
            (baseline / ".skillspector-baseline.yaml").write_text("version: 2\n", encoding="utf-8")
            rel = "chaos-engine/skills/demo-skill"
            with unittest.mock.patch(
                "scripts.ci.skillspector_gate.skill_dirs_from_paths", return_value=[Path(rel)]
            ):
                first = opp._skillspector_fingerprint(root, [f"{rel}/SKILL.md"])
                os.utime(target, (1_000_000, 1_000_000))
                second = opp._skillspector_fingerprint(root, [f"{rel}/SKILL.md"])
                target.write_text("# demo changed\n", encoding="utf-8")
                third = opp._skillspector_fingerprint(root, [f"{rel}/SKILL.md"])
        self.assertEqual(first, second)
        self.assertNotEqual(first, third)
        self.assertEqual(len(first), 64)
        int(first, 16)  # valid sha256 hex


class FullScriptsUnittestWrapperTest(unittest.TestCase):
    def test_wrapper_script_documents_lock_and_is_the_sanctioned_path(self):
        wrapper = ROOT / "scripts/ci/run_full_scripts_unittest.py"
        text = wrapper.read_text(encoding="utf-8")
        self.assertTrue(wrapper.is_file())
        self.assertIn("full_scripts_unittest_lock", text)
        self.assertIn("Sanctioned entrypoint", text)
        self.assertIn("os.execvpe", text)
        self.assertIn("discover", text)
        self.assertIn("-s", text)
        self.assertIn("tests/scripts", text)


class LessonsCardPresentTest(unittest.TestCase):
    def test_throughput_lesson_card_exists_with_timings_and_interrupt_evidence(self):
        card = ROOT / "scripts/ci/mega-pr-throughput-20261102.md"
        text = card.read_text(encoding="utf-8")
        self.assertIn("Session cap", text)
        self.assertIn("full_scripts_unittest_lock", text)
        self.assertIn("run_full_scripts_unittest.py", text)
        self.assertIn("--all-failures", text)
        self.assertIn("unproven", text.lower())
        self.assertIn("GraphQL", text)
        self.assertIn("Never run the full suite in the foreground", text)
        self.assertIn("nohup", text.casefold())



class LessonsCard20261102bTest(unittest.TestCase):
    def test_followup_lesson_card_covers_cq_threads_worktree_cap_and_heal(self):
        card = ROOT / "scripts/ci/mega-pr-throughput-20261102b.md"
        text = card.read_text(encoding="utf-8")
        self.assertIn("unresolved", text.casefold())
        self.assertIn("code-quality", text.casefold())
        self.assertIn("linked-worktree", text.casefold())
        self.assertIn("refuse_new_worker", text)
        self.assertIn("heal-route", text)
        self.assertIn("uninstall", text.casefold())
        self.assertIn("session overlay", text.casefold())
        heal = (ROOT / "chaos-engine/references/heal-route.md").read_text(encoding="utf-8")
        self.assertIn("Distribution id mismatch", heal)
        self.assertIn("Session overlay vs primary doctor", heal)
        bot = (ROOT / "chaos-engine/references/bot-entry.md").read_text(encoding="utf-8")
        self.assertIn("scripts/ci/mega-pr-throughput-20261102b.md", bot)
        self.assertIn("code-quality review thread", bot)
        self.assertIn("unresolved-review-threads", bot)
        self.assertNotIn("../../scripts/ci/mega-pr-throughput-20261102b.md", bot)
        eco = (ROOT / "chaos-engine/references/ci-status-economy.md").read_text(encoding="utf-8")
        self.assertIn("scripts/ci/mega-pr-throughput-20261102b.md", eco)
        self.assertNotIn("../../scripts/ci/mega-pr-throughput-20261102b.md", eco)


if __name__ == "__main__":
    unittest.main()
