"""Spec Kit idea adoption (#8023): intent inventory, read-only analyze, indexed skill."""

from __future__ import annotations

import json
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
CE = ROOT / "chaos-engine"
sys.path.insert(0, str(CE))

import harness_index  # noqa: E402
import intent_converge as gate  # noqa: E402

SKILL = CE / "skills/learn-from-external-harnesses/SKILL.md"
ROUTER = CE / "skills/chaos-engine/SKILL.md"


class ConvergeTest(unittest.TestCase):
    def test_missing_obligation_is_reported_and_a_present_one_is_not(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            (root / "gate.py").write_text("keeps FR-001\n", encoding="utf-8")
            work = (
                "FR-001: keep the gate. Evidence: gate.py\n"
                "FR-002: keep the log. Evidence: log.py\n"
            )
            report = gate.converge(root, work)
            missing = [item["id"] for item in report["findings"] if item["kind"] == "missing"]
            self.assertEqual(missing, ["FR-002"])
            self.assertEqual(report["findings"][0]["source"], "FR-002: keep the log. Evidence: log.py")
            self.assertEqual(report["findings"][0]["evidence"], "log.py")
            self.assertEqual(report["work_item"], work)

    def test_a_checked_task_is_not_evidence(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            work = "FR-002: keep the log. Evidence: log.py\n- [x] FR-002 done\n"
            report = gate.converge(root, work)
            self.assertEqual(
                [item["id"] for item in report["findings"] if item["kind"] == "missing"],
                ["FR-002"],
            )

    def test_converged_leaves_the_work_item_unchanged(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            (root / "gate.py").write_text("FR-001\n", encoding="utf-8")
            work = "FR-001: keep the gate. Evidence: gate.py\n"
            report = gate.converge(root, work)
            self.assertEqual(report["status"], "Converged")
            self.assertEqual(report["findings"], [])
            self.assertEqual(report["work_item"], work)
            self.assertNotIn("Phase: Convergence", work)

    def test_unrequested_file_is_listed_and_kept(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            (root / "gate.py").write_text("FR-001\n", encoding="utf-8")
            extra = root / "extra.py"
            extra.write_text("spare\n", encoding="utf-8")
            work = "FR-001: keep the gate. Evidence: gate.py\n"
            report = gate.converge(root, work)
            self.assertEqual(report["status"], "Converged")
            self.assertEqual(
                [item["id"] for item in report["findings"] if item["kind"] == "unrequested"],
                ["extra.py"],
            )
            self.assertEqual(extra.read_text(encoding="utf-8"), "spare\n")
            banned = root / ("." + "specify")
            self.assertFalse(banned.exists())

    def test_cli_does_not_rewrite_the_work_item(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            (root / "gate.py").write_text("FR-001\n", encoding="utf-8")
            work_path = root / "item.md"
            work_path.write_text("FR-001: keep the gate. Evidence: gate.py\n", encoding="utf-8")
            before = work_path.read_bytes()
            completed = subprocess.run(
                [sys.executable, str(CE / "intent_converge.py"), "converge", str(work_path), str(root)],
                check=False,
                capture_output=True,
                text=True,
            )
            self.assertEqual(completed.returncode, 0, completed.stderr)
            self.assertEqual(work_path.read_bytes(), before)
            self.assertEqual(json.loads(completed.stdout)["status"], "Converged")


class AnalyzeTest(unittest.TestCase):
    def test_missing_task_and_a_contradiction_are_both_findings(self) -> None:
        work = (
            "FR-001: the gate records the owner.\n"
            "FR-002: the log MUST keep the token.\n"
            "FR-003: the log MUST NOT keep the token.\n"
        )
        tasks = "- [ ] FR-002 add the log\n- [ ] FR-003 drop the log\n"
        sentinel = CE / "intent_converge.py"
        before = sentinel.read_bytes()
        report = gate.analyze(work, tasks)
        self.assertEqual(sentinel.read_bytes(), before)
        self.assertEqual(
            [item["id"] for item in report["findings"] if item["category"] == "coverage"],
            ["FR-001"],
        )
        self.assertEqual(
            [item["category"] for item in report["findings"] if item["category"] == "inconsistency"],
            ["inconsistency"],
        )

    def test_a_clean_pair_reports_zero_findings_and_coverage(self) -> None:
        work = "FR-001: the gate records the owner.\n"
        tasks = "- [ ] FR-001 record the owner\n"
        report = gate.analyze(work, tasks)
        self.assertEqual(report["findings"], [])
        self.assertEqual(report["coverage"], {"obligations": 1, "tasks": 1, "covered": 1})


class ClarifyPlanBugIdeaTest(unittest.TestCase):
    def test_answers_are_written_into_the_work_item_and_zero_is_valid(self) -> None:
        ready = "FR-001: store the recording on disk.\n"
        self.assertEqual(gate.apply_clarification(ready, []), ready)
        updated = gate.apply_clarification("FR-001: NEEDS CLARIFICATION storage\n", ["store on disk"])
        self.assertNotIn("NEEDS CLARIFICATION", updated)
        self.assertIn("store on disk", updated)
        with self.assertRaises(ValueError):
            gate.apply_clarification("FR-001: NEEDS CLARIFICATION\n", ["a", "b", "c", "d", "e", "f"])

    def test_constitution_check_and_optional_table(self) -> None:
        self.assertEqual(
            gate.plan_gaps("## Constitution Check\nCite identity.md Principles. Pass.\n"),
            [],
        )
        self.assertEqual(gate.plan_gaps("research starts now\n"), ["missing Constitution Check"])
        violated = "## Constitution Check\nThis plan has an iron law violation: skip the test.\n"
        self.assertIn("iron-law violation without a Complexity Tracking row", gate.plan_gaps(violated))
        explained = (
            violated
            + "\n## Complexity Tracking\n"
            + "| Violation | Why Needed | Simpler Alternative Rejected Because |\n"
            + "| --- | --- | --- |\n"
            + "| skip the test | the host has no display | a headless fixture was rejected because it cannot boot the sample |\n"
        )
        self.assertEqual(gate.plan_gaps(explained), [])

    def test_verified_requires_the_original_reproduction(self) -> None:
        self.assertEqual(
            gate.bug_verdict_errors("verdict: verified\n"),
            ["verified without exercising the original reproduction"],
        )
        self.assertEqual(
            gate.bug_verdict_errors("verdict: verified\nexercised the original reproduction\n"),
            [],
        )

    def test_new_product_idea_has_one_decision(self) -> None:
        self.assertEqual(gate.idea_decision_errors("decision: kill\nreason: already shipped\n"), [])
        self.assertTrue(gate.idea_decision_errors("decision: go\ndecision: kill\n"))
        self.assertTrue(gate.idea_decision_errors("we should think about it\n"))

    def test_requirements_quality_flags_a_stamp_and_an_unmeasured_criterion(self) -> None:
        text = (
            "SC-001: fast\n"
            "SC-002: under 2 seconds\n"
            "Deliver the stated acceptance\n"
            "Deliver the stated acceptance\n"
        )
        self.assertEqual(
            gate.english_quality_findings(text),
            ["identical acceptance stamp", "SC-001 is not measurable"],
        )


class SkillRegistryTest(unittest.TestCase):
    def test_skill_is_indexed_on_demand_and_linked(self) -> None:
        names = [row[0] for row in harness_index.SKILLS]
        self.assertIn("learn-from-external-harnesses", names)
        self.assertIn("learn-from-external-harnesses", (CE / "harness-index.json").read_text(encoding="utf-8"))
        self.assertIn(
            "../skills/learn-from-external-harnesses/SKILL.md",
            (CE / "references/level-1-catalog.md").read_text(encoding="utf-8"),
        )
        self.assertIn(
            "learn-from-external-harnesses",
            (CE / "references/details/ce-study-2026-11-09/cadence.md").read_text(encoding="utf-8"),
        )
        routes = ROUTER.read_text(encoding="utf-8").split("HARNESS-ROUTES:START", 1)[1].split("HARNESS-ROUTES:END", 1)[0]
        self.assertNotIn("learn-from-external-harnesses", routes)
        budget = (CE / "always-loaded-budget.json").read_text(encoding="utf-8")
        self.assertNotIn("learn-from-external-harnesses", budget)
        body = SKILL.read_text(encoding="utf-8")
        self.assertLess(len(body.splitlines()), 150)
        self.assertIn("Inventory the local harness first", body)
        self.assertIn("already-have / improve-existing / new / reject", body)
        self.assertIn("not a silent edit to skills, rules, or the registry", body)
        self.assertIn("tracked ticket or draft-PR proposal", body)
        self.assertIn("Verify the source is what it claims", body)
        self.assertEqual(
            gate.registry_gaps(CE / "skills", {row[0] for row in harness_index.SKILLS}),
            [],
        )

    def test_an_unindexed_skill_file_is_a_gap(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            folder = root / "learn-from-external-harnesses"
            folder.mkdir()
            (folder / "SKILL.md").write_text("name: learn-from-external-harnesses\n", encoding="utf-8")
            self.assertEqual(gate.registry_gaps(root, set()), ["learn-from-external-harnesses"])
            self.assertEqual(gate.registry_gaps(root, {"learn-from-external-harnesses"}), [])


if __name__ == "__main__":
    unittest.main()
