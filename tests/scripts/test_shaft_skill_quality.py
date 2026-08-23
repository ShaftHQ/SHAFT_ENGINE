"""Generated SHAFT skill inventory and quality-contract tests (#4640)."""

import json
import tempfile
import unittest
from pathlib import Path
from unittest.mock import patch

try:
    from scripts.ci.shaft_skill_quality import (
        render_inventory,
        render_quality_report,
        validate_repository,
        write_generated,
    )
except ImportError:
    from scripts.ci.shaft_skill_quality import validate_repository

    render_inventory = None
    render_quality_report = None
    write_generated = None


class ShaftSkillQualityTest(unittest.TestCase):
    def setUp(self):
        self.temporary_directory = tempfile.TemporaryDirectory()
        self.root = Path(self.temporary_directory.name)
        self.write(
            "shaft-skills/example/SKILL.md",
            "---\n"
            "name: example\n"
            "description: Use when an example workflow needs a deterministic result.\n"
            "---\n\n"
            "# Example\n\nRead [the playbook](references/playbook.md).\n",
        )
        self.write(
            "shaft-skills/example/references/playbook.md",
            "# Playbook\n\nRun `python -m unittest` and assert the result.\n",
        )
        self.write(
            "shaft-skills/other/SKILL.md",
            "---\n"
            "name: other\n"
            "description: Use when another workflow needs a separate result.\n"
            "---\n\n"
            "# Other\n\nRead [the playbook](references/playbook.md).\n",
        )
        self.write(
            "shaft-skills/other/references/playbook.md",
            "# Playbook\n\nVerify the output with a focused check.\n",
        )
        self.review = {
            "package": "shaft-skills",
            "supported_clients": ["Claude Code", "Codex CLI"],
            "context_budget": {
                "max_description_chars": 180,
                "max_total_description_chars": 300,
                "max_skill_md_lines": 50,
            },
            "skills": {
                "example": self.skill_review("other"),
                "other": self.skill_review("example"),
            },
        }
        self.write_review()
        if callable(write_generated):
            write_generated(self.root)

    def tearDown(self):
        self.temporary_directory.cleanup()

    def write(self, relative_path: str, content: str):
        path = self.root / relative_path
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(content, encoding="utf-8")

    @staticmethod
    def skill_review(confusion_with: str) -> dict:
        return {
            "scores": {
                "trigger_precision": 5,
                "sibling_overlap": 4,
                "dependency_depth": 5,
                "reference_freshness": 4,
                "runnable_evidence": 4,
                "context_cost": 5,
            },
            "confusion_with": [confusion_with],
            "notes": "Distinct trigger and current reference evidence were reviewed.",
        }

    def write_review(self):
        self.write(
            "shaft-skills/quality-review.json",
            json.dumps(self.review, indent=2) + "\n",
        )

    def test_quality_validator_is_available(self):
        self.assertTrue(callable(validate_repository))

    def test_quality_renderers_are_available(self):
        self.assertTrue(callable(render_inventory))
        self.assertTrue(callable(render_quality_report))
        self.assertTrue(callable(write_generated))

    def test_valid_generated_contract_passes(self):
        self.assertEqual(validate_repository(self.root), [])

    def test_inventory_contains_every_required_field(self):
        inventory = render_inventory(self.root)

        self.assertIn("shaft-skills/example/SKILL.md", inventory)
        self.assertIn("skills/example/SKILL.md", inventory)
        self.assertIn("Use when an example workflow", inventory)
        self.assertIn("example/references/playbook.md", inventory)
        self.assertIn("shaft-skills", inventory)
        self.assertIn("Claude Code; Codex CLI", inventory)

    def test_inventory_uses_the_assembler_owned_package_mapping(self):
        with patch(
            "scripts.ci.shaft_skill_quality.package_path_for_source",
            return_value=Path("portable/example/SKILL.md"),
            create=True,
        ):
            inventory = render_inventory(self.root)

        self.assertIn("portable/example/SKILL.md", inventory)
        self.assertNotIn("| `skills/example/SKILL.md` |", inventory)

    def test_quality_report_contains_all_six_scores_and_live_context_cost(self):
        report = render_quality_report(self.root)

        for dimension in (
            "Trigger precision",
            "Sibling overlap",
            "Dependency depth",
            "Reference freshness",
            "Runnable evidence",
            "Context cost",
        ):
            self.assertIn(dimension, report)
        self.assertIn("Description characters", report)
        self.assertIn("example", report)

    def test_missing_review_row_is_reported(self):
        del self.review["skills"]["other"]
        self.write_review()

        self.assertDefect("review-skill-set")

    def test_unknown_review_row_is_reported(self):
        self.review["skills"]["ghost"] = self.skill_review("example")
        self.write_review()

        self.assertDefect("review-skill-set")

    def test_missing_quality_dimension_is_reported(self):
        del self.review["skills"]["example"]["scores"]["context_cost"]
        self.write_review()

        self.assertDefect("quality-dimensions")

    def test_invalid_quality_score_is_reported(self):
        self.review["skills"]["example"]["scores"]["trigger_precision"] = 6
        self.write_review()

        self.assertDefect("quality-score")

    def test_unknown_confusion_pair_is_reported(self):
        self.review["skills"]["example"]["confusion_with"] = ["ghost"]
        self.write_review()

        self.assertDefect("confusion-skill")

    def test_per_skill_description_budget_is_enforced(self):
        self.review["context_budget"]["max_description_chars"] = 10
        self.write_review()

        self.assertDefect("description-budget")

    def test_total_description_budget_is_enforced(self):
        self.review["context_budget"]["max_total_description_chars"] = 20
        self.write_review()

        self.assertDefect("listing-budget")

    def test_skill_body_line_budget_is_enforced(self):
        self.review["context_budget"]["max_skill_md_lines"] = 2
        self.write_review()

        self.assertDefect("progressive-disclosure-budget")

    def test_dead_linked_reference_is_reported(self):
        (self.root / "shaft-skills/example/references/playbook.md").unlink()

        self.assertDefect("linked-reference")

    def test_link_escape_is_reported(self):
        self.write("outside.md", "# Outside\n")
        path = self.root / "shaft-skills/example/SKILL.md"
        path.write_text(
            path.read_text(encoding="utf-8").replace(
                "references/playbook.md", "../../outside.md"
            ),
            encoding="utf-8",
        )

        self.assertDefect("linked-reference-containment")

    def test_skill_change_without_regeneration_is_reported(self):
        path = self.root / "shaft-skills/example/SKILL.md"
        path.write_text(
            path.read_text(encoding="utf-8").replace(
                "deterministic result", "reviewed deterministic result"
            ),
            encoding="utf-8",
        )

        defects = validate_repository(self.root)
        self.assertTrue(any("inventory-drift" in defect for defect in defects), defects)
        self.assertTrue(any("quality-report-drift" in defect for defect in defects), defects)

    def test_current_repository_generated_contract_is_valid(self):
        repository_root = Path(__file__).resolve().parents[2]
        self.assertEqual(validate_repository(repository_root), [])

    def test_pr_gate_runs_quality_tests_and_validator(self):
        repository_root = Path(__file__).resolve().parents[2]
        workflow = (repository_root / ".github/workflows/pr-gate.yml").read_text(
            encoding="utf-8"
        )
        from scripts.ci.harness_pr_gate import classify_paths

        self.assertIn("scripts/ci/shaft_skill_quality.py", workflow)
        self.assertIn("scripts/ci/harness_pr_gate.py", workflow)
        plan = classify_paths(["scripts/ci/shaft_skill_quality.py"])
        quality = next(check for check in plan.checks if check.id == "plugin-quality-contract")
        self.assertEqual(("tests.scripts.test_shaft_skill_quality",), quality.modules)

    def assertDefect(self, code: str):
        defects = validate_repository(self.root)
        self.assertTrue(any(code in defect for defect in defects), defects)


if __name__ == "__main__":
    unittest.main()
