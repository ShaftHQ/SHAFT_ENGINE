"""UI delivery route: lessons from a real UI-polish delivery bound to the portable overlay.

Each assertion pins one generalized lesson so a later edit that drops or
qualifies it fails here, on every host, instead of living in one agent's memory.
"""

from __future__ import annotations

import json
import re
import shutil
import sys
import tempfile
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
SOURCE = ROOT / "chaos-engine"
UI = SOURCE / "references/ui-delivery.md"
ROUTER = SOURCE / "skills/chaos-engine/SKILL.md"
LEVEL1 = SOURCE / "references/level-1-catalog.md"
INDEX = SOURCE / "harness-index.json"
PORTABLE_ROUTING = SOURCE / "profiles/portable/references/routing.md"
TASK_ISOLATION = SOURCE / "references/task-isolation.md"
DELEGATION = SOURCE / "references/delegation.md"
HEAL = SOURCE / "references/heal-route.md"
UI_MAX_BYTES = 4096
VIEWPORTS = ("390x844", "768x1024", "1440x900", "2560x1440", "3840x2160")


def compact(path: Path) -> str:
    return re.sub(r"\s+", " ", path.read_text(encoding="utf-8"))


def section(text: str, heading: str) -> str:
    match = re.search(rf"(?ms)^## {re.escape(heading)}\n(.*?)(?=^## |\Z)", text)
    if match is None:
        raise AssertionError(f"missing section: {heading}")
    return re.sub(r"\s+", " ", match.group(1))


class UiDeliveryRoutingTests(unittest.TestCase):
    def test_router_routes_ui_work_to_the_reference(self):
        rows = [line for line in ROUTER.read_text(encoding="utf-8").splitlines() if line.startswith("| UI delivery |")]
        self.assertEqual(1, len(rows))
        self.assertIn("user-visible UI", rows[0])
        self.assertIn("(../../references/ui-delivery.md)", rows[0])
        self.assertTrue(UI.is_file())
        self.assertLessEqual(len(ROUTER.read_bytes()), 7168)

    def test_level1_index_and_portable_profile_reach_the_reference(self):
        self.assertIn("[`ui-delivery.md`](ui-delivery.md)", LEVEL1.read_text(encoding="utf-8"))
        entries = {entry["name"]: entry for entry in json.loads(INDEX.read_text(encoding="utf-8"))["entries"]}
        self.assertEqual("route", entries["ui-delivery"]["kind"])
        self.assertEqual("references/ui-delivery.md", entries["ui-delivery"]["path"])
        self.assertIn("user-visible UI", compact(PORTABLE_ROUTING))
        self.assertIn("(../../../references/ui-delivery.md)", PORTABLE_ROUTING.read_text(encoding="utf-8"))

    def test_plugin_mirror_ships_the_reference(self):
        sys.path.insert(0, str(ROOT))
        from scripts.ci.assemble_chaos_engine_plugin import assemble

        temp = Path(tempfile.mkdtemp())
        self.addCleanup(shutil.rmtree, temp, True)
        assemble(ROOT, temp / "package")
        self.assertEqual(UI.read_bytes(), (temp / "package/references/ui-delivery.md").read_bytes())


class UiDeliveryLessonTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.raw = UI.read_text(encoding="utf-8")
        cls.text = compact(UI)

    def test_reference_stays_lean(self):
        self.assertLessEqual(len(UI.read_bytes()), UI_MAX_BYTES)

    def test_visual_matrix_covers_every_viewport_theme_and_scroll_sweep(self):
        matrix = section(self.raw, "Visual matrix")
        for viewport in VIEWPORTS:
            with self.subTest(viewport=viewport):
                self.assertIn(viewport, matrix)
        self.assertIn("every theme", matrix)
        self.assertIn("before and after", matrix)
        self.assertIn("from 320 to 3840 for horizontal scroll", matrix)
        self.assertIn("2560 and 3840 are required", matrix)
        for check in ("empty regions", "crowding", "nav readability"):
            self.assertIn(check, matrix)

    def test_tests_must_be_seen_red_against_the_base_build(self):
        red = section(self.raw, "Red, then green")
        self.assertIn("against a build of the base branch first", red)
        self.assertIn("must fail for the stated reason", red)
        self.assertIn("A test never seen red is not evidence", red)

    def test_empty_space_is_measured_not_eyeballed(self):
        measure = section(self.raw, "Measure, do not eyeball")
        for token in ("scrollWidth <= innerWidth", "getBoundingClientRect()", "empty-cell counts", "never replace an assertion"):
            self.assertIn(token, measure)

    def test_best_practice_research_is_cited_and_mapped(self):
        plan = section(self.raw, "Plan")
        for source in ("NN/g", "WCAG 2.2", "Baymard", "web.dev", "MDN"):
            self.assertIn(source, plan)
        self.assertIn("concrete change or to existing compliance", plan)
        self.assertIn("(deep-research.md)", self.raw)

    def test_test_contract_change_is_narrowed_and_proven(self):
        contract = section(self.raw, "Test-contract changes")
        self.assertIn("narrow it to the real invariant", contract)
        self.assertIn("Never delete it", contract)
        self.assertIn("still fails on the original bad case", contract)
        self.assertIn("`Test contract changes`", contract)

    def test_ci_coverage_gap_is_detected_and_disclosed(self):
        plan = section(self.raw, "Plan")
        report = section(self.raw, "PR body and report")
        self.assertIn('rg -n "pull_request|playwright|e2e" .github/workflows', plan)
        self.assertIn("CI coverage gap", plan)
        self.assertIn("PR CI green does not cover <surface>", report)
        self.assertIn("attach the local run", report)

    def test_adversarial_review_is_mandatory_and_cheap(self):
        review = section(self.raw, "Review")
        self.assertIn("is mandatory when a UI PR changes a test contract", review)
        self.assertIn("Time pressure is not a skip reason", review)
        self.assertIn("one reviewer, one round", review)
        # Qualifying the rule is a mutation too: no escape hatch may be appended.
        for hedge in ("unless", "optional", "if time", "when possible"):
            self.assertNotIn(hedge, review.casefold())
        delegation = compact(DELEGATION)
        self.assertIn("Review is mandatory, never skipped for time, when a PR changes an existing test contract", delegation)
        self.assertIn("(ui-delivery.md)", delegation)

    def test_manual_deploy_is_named_after_merge(self):
        report = section(self.raw, "PR body and report")
        self.assertIn("`Post-merge deploy`", report)
        self.assertIn("gh workflow run <file> --ref <default>", report)

    def test_consumer_repository_delivery_uses_a_separate_worktree(self):
        report = section(self.raw, "PR body and report")
        self.assertIn("(task-isolation.md)", report)
        isolation = compact(TASK_ISOLATION)
        self.assertIn("consumer repository", isolation)
        self.assertIn("uses a separate worktree by default", isolation)
        self.assertIn("git worktree add <path> -b <branch> origin/<default>", isolation)
        for path in (".chaos-engine/", ".claude/", ".grok/", ".mcp.json", "AGENTS.md", ".gitignore"):
            self.assertIn(path, isolation)
        self.assertIn("`git status --porcelain` lists only task files", isolation)

    def test_heal_route_names_the_rate_limit_fix(self):
        heal = compact(HEAL)
        self.assertIn("rate limit", heal)
        self.assertIn('GITHUB_TOKEN="$(gh auth token)"', heal)


if __name__ == "__main__":
    unittest.main()
