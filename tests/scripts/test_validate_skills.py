import json
import tempfile
import unittest
from pathlib import Path

from scripts.ci.validate_skills import validate_repository


class ValidateSkillsTest(unittest.TestCase):
    def setUp(self):
        self.temporary_directory = tempfile.TemporaryDirectory()
        self.root = Path(self.temporary_directory.name)
        self.budget_path = self.root / "scripts/ci/agent_guidance_budget.json"
        self.budget = {"skill_budgets": {".claude/skills": {"max_skill_md_bytes": 400}}}
        self.write_budget()
        self.write(
            ".claude/skills/example/SKILL.md",
            "---\n"
            "name: example\n"
            "description: >-\n"
            "  Do the example thing well. Use when the task looks like an\n"
            "  example so future readers know when to reach for it.\n"
            "---\n\n"
            "# Example\n\nBody content that is non-empty.\n"
            "See `references/detail.md` for more.\n",
        )
        self.write(".claude/skills/example/references/detail.md", "# Detail\n")

    def tearDown(self):
        self.temporary_directory.cleanup()

    def write(self, relative_path, content):
        path = self.root / relative_path
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(content, encoding="utf-8")

    def write_budget(self):
        self.budget_path.parent.mkdir(parents=True, exist_ok=True)
        self.budget_path.write_text(json.dumps(self.budget), encoding="utf-8")

    def codes(self):
        return {error["code"] for error in validate_repository(self.root, self.budget_path)}

    def test_valid_skill_passes(self):
        self.assertEqual(validate_repository(self.root, self.budget_path), [])

    def test_folded_block_scalar_description_is_read_in_full(self):
        # Regression for the parse_frontmatter gap that used to truncate a
        # `description: >-` value to its two-character marker: a thin/no-op
        # description hiding behind a folded block scalar must still be
        # caught, not silently treated as "long enough".
        self.write(
            ".claude/skills/example/SKILL.md",
            "---\nname: example\ndescription: >-\n  x\n---\n\n# Example\n\nBody.\n",
        )
        self.assertIn("description-too-thin", self.codes())

    def test_rejects_missing_skill_md(self):
        (self.root / ".claude/skills/empty").mkdir(parents=True)
        self.assertIn("skill-missing", self.codes())

    def test_rejects_malformed_frontmatter(self):
        self.write(".claude/skills/example/SKILL.md", "# No frontmatter at all\n")
        self.assertIn("frontmatter-malformed", self.codes())

    def test_rejects_name_mismatch(self):
        self.write(
            ".claude/skills/example/SKILL.md",
            "---\nname: not-example\ndescription: Use when testing name mismatches.\n---\n\nBody.\n",
        )
        self.assertIn("frontmatter-name", self.codes())

    def test_rejects_missing_description(self):
        self.write(
            ".claude/skills/example/SKILL.md",
            "---\nname: example\n---\n\nBody.\n",
        )
        self.assertIn("frontmatter-description", self.codes())

    def test_rejects_description_missing_trigger(self):
        self.write(
            ".claude/skills/example/SKILL.md",
            "---\nname: example\ndescription: A skill that does example things nicely.\n---\n\nBody.\n",
        )
        self.assertIn("description-missing-trigger", self.codes())

    def test_rejects_empty_body(self):
        self.write(
            ".claude/skills/example/SKILL.md",
            "---\nname: example\ndescription: Use when the body is empty.\n---\n\n   \n",
        )
        self.assertIn("body-empty", self.codes())

    def test_rejects_dead_backtick_reference(self):
        self.write(
            ".claude/skills/example/SKILL.md",
            "---\nname: example\ndescription: Use when checking dead references.\n---\n\n"
            "Body. See `references/missing.md`.\n",
        )
        self.assertIn("dead-reference", self.codes())

    def test_rejects_dead_markdown_link_reference(self):
        self.write(
            ".claude/skills/example/SKILL.md",
            "---\nname: example\ndescription: Use when checking dead references.\n---\n\n"
            "Body. See [detail](references/missing.md) for more.\n",
        )
        self.assertIn("dead-reference", self.codes())

    def test_rejects_oversized_skill_md(self):
        self.budget["skill_budgets"][".claude/skills"]["max_skill_md_bytes"] = 10
        self.write_budget()
        self.assertIn("byte-budget", self.codes())

    def test_reports_missing_budget_config(self):
        self.budget_path.write_text(json.dumps({}), encoding="utf-8")
        self.assertIn("budget-config", self.codes())

    def test_current_repository_skills_are_valid(self):
        repository_root = Path(__file__).resolve().parents[2]
        self.assertEqual(validate_repository(repository_root), [])


if __name__ == "__main__":
    unittest.main()
