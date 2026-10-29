"""Client-name scrub gate (synthetic placeholders only — no real client markers)."""

from __future__ import annotations

import ast
import sys
import tempfile
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT / "chaos-engine"))
sys.path.insert(0, str(ROOT))

import client_name_scrub  # noqa: E402
from scripts.ci import overlay_pre_push  # noqa: E402

# Synthetic tokens for tests only. Never use a real client engagement name here.
PLACEHOLDER = "AcmeClientX"
PLACEHOLDER_ALT = "ZephyrEngagement9"


class LoadDenylistTest(unittest.TestCase):
    def test_empty_when_unconfigured(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            self.assertEqual((), client_name_scrub.load_denylist(Path(tmp), environ={}))

    def test_env_tokens(self) -> None:
        env = {client_name_scrub.ENV_KEY: f"{PLACEHOLDER}, {PLACEHOLDER_ALT}"}
        tokens = client_name_scrub.load_denylist(Path("/"), environ=env)
        self.assertEqual(tokens, (PLACEHOLDER, PLACEHOLDER_ALT))

    def test_state_file_tokens(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            path = root / client_name_scrub.STATE_REL
            path.parent.mkdir(parents=True)
            path.write_text(f"# comment\n{PLACEHOLDER}\n\n", encoding="utf-8")
            self.assertEqual(
                (PLACEHOLDER,),
                client_name_scrub.load_denylist(root, environ={}),
            )


class ScrubSurfacesTest(unittest.TestCase):
    def test_unconfigured_is_noop(self) -> None:
        self.assertEqual(
            [],
            client_name_scrub.client_name_scrub_failures(
                ROOT,
                denylist=(),
                branch=f"ChaosEngine/{PLACEHOLDER}-leak",
                commit_messages=[f"feat: {PLACEHOLDER} lessons"],
                pr_body=f"Rename to {PLACEHOLDER}",
                diff_added_text=f"+ leak {PLACEHOLDER}\n",
            ),
        )

    def test_branch_match_fails_closed(self) -> None:
        failures = client_name_scrub.client_name_scrub_failures(
            ROOT,
            denylist=(PLACEHOLDER,),
            branch=f"ChaosEngine/{PLACEHOLDER.lower()}-harness",
            commit_messages=[],
            pr_body="",
            diff_added_text="",
        )
        self.assertTrue(failures)
        self.assertIn("branch name", failures[0])

    def test_commit_message_match_fails_closed(self) -> None:
        failures = client_name_scrub.client_name_scrub_failures(
            ROOT,
            denylist=(PLACEHOLDER,),
            branch="ChaosEngine/mobile-retro-harness",
            commit_messages=[f"feat(harness): {PLACEHOLDER} lessons"],
            pr_body="",
            diff_added_text="",
        )
        self.assertTrue(any("commit message" in f for f in failures))

    def test_pr_body_match_fails_closed(self) -> None:
        failures = client_name_scrub.client_name_scrub_failures(
            ROOT,
            denylist=(PLACEHOLDER,),
            branch="ChaosEngine/mobile-retro-harness",
            commit_messages=[],
            pr_body=f"Plan: rename `{PLACEHOLDER.lower()}_j11` → mobile_retro",
            diff_added_text="",
        )
        self.assertTrue(any("PR body" in f for f in failures))

    def test_clean_surfaces_pass(self) -> None:
        failures = client_name_scrub.client_name_scrub_failures(
            ROOT,
            denylist=(PLACEHOLDER, PLACEHOLDER_ALT),
            branch="ChaosEngine/mobile-retro-harness",
            commit_messages=["feat(harness): mobile-retro lessons — freeze, Blocked, NO_PUSH"],
            pr_body="Fixes #7497\nRefs #7493",
            diff_added_text='+ buildName("Acme Android")\n',
        )
        self.assertEqual([], failures)

    def test_diff_added_lines_match_fails_closed(self) -> None:
        diff = (
            "diff --git a/x.py b/x.py\n"
            "--- a/x.py\n"
            "+++ b/x.py\n"
            f"- old\n+ buildName(\"{PLACEHOLDER} Android\")\n"
        )
        failures = client_name_scrub.client_name_scrub_failures(
            ROOT,
            denylist=(PLACEHOLDER,),
            branch="ChaosEngine/scrub-main-client-literals",
            commit_messages=[],
            pr_body="",
            diff_added_text=client_name_scrub.added_lines_from_diff(diff),
        )
        self.assertTrue(any("PR diff added lines" in f for f in failures), failures)

    def test_diff_removal_only_does_not_fail(self) -> None:
        """Scrub PRs delete markers on '-' lines; those must not trip the gate."""
        diff = (
            "diff --git a/x.py b/x.py\n"
            "--- a/x.py\n"
            "+++ b/x.py\n"
            f"- buildName(\"{PLACEHOLDER} Android\")\n"
            "+ buildName(\"Acme Android\")\n"
        )
        failures = client_name_scrub.client_name_scrub_failures(
            ROOT,
            denylist=(PLACEHOLDER,),
            branch="ChaosEngine/scrub-main-client-literals",
            commit_messages=["chore(tests): replace legacy client literals"],
            pr_body="Fixes #0",
            diff_added_text=client_name_scrub.added_lines_from_diff(diff),
        )
        self.assertEqual([], failures)



class LessonAndWiringTest(unittest.TestCase):
    def test_lesson_and_map_exist(self) -> None:
        lesson = (ROOT / "chaos-engine/references/client-name-scrub.md").read_text(encoding="utf-8")
        self.assertIn("CHAOS_ENGINE_CLIENT_NAME_DENYLIST", lesson)
        self.assertTrue("fail closed" in lesson.casefold() or "fails closed" in lesson.casefold())
        self.assertIn("added lines", lesson.casefold())
        harness_map = (ROOT / "chaos-engine/references/harness-map.md").read_text(encoding="utf-8")
        self.assertIn("client-name-scrub.md", harness_map)
        agents = (ROOT / ".agents/skills/README.md").read_text(encoding="utf-8")
        self.assertIn("client-name-scrub.md", agents)

    def test_overlay_calls_client_name_scrub_failures(self) -> None:
        src = (ROOT / "scripts/ci/overlay_pre_push.py").read_text(encoding="utf-8")
        tree = ast.parse(src)
        calls: list[str] = []

        class Visitor(ast.NodeVisitor):
            def visit_Call(self, node: ast.Call) -> None:  # noqa: N802
                func = node.func
                if isinstance(func, ast.Name):
                    calls.append(func.id)
                self.generic_visit(node)

        Visitor().visit(tree)
        self.assertIn(
            "client_name_scrub_failures",
            calls,
            "removing client_name_scrub_failures wiring from overlay_pre_push must fail this test",
        )

    def test_overlay_helper_skips_when_unconfigured(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            # Sparse fixture: no denylist file, empty environ via explicit denylist
            self.assertEqual(
                [],
                overlay_pre_push.client_name_scrub_failures(root, environ={}),
            )


if __name__ == "__main__":
    unittest.main()
