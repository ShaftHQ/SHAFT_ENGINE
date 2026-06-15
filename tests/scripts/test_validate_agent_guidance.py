import json
import tempfile
import unittest
from pathlib import Path

from scripts.ci.validate_agent_guidance import validate_repository


class ValidateAgentGuidanceTest(unittest.TestCase):
    def setUp(self):
        self.temporary_directory = tempfile.TemporaryDirectory()
        self.root = Path(self.temporary_directory.name)
        self.write("AGENTS.md", "# Agents\n\n[Local](docs/local.md)\n")
        self.write("CLAUDE.md", "# Claude\n\n@AGENTS.md\n")
        self.write(".github/copilot-instructions.md", "# Copilot\n")
        self.write(
            ".github/instructions/source.instructions.md",
            '---\napplyTo: "**/src/main/java/**/*.java"\n---\n\n# Source\n',
        )
        self.write(
            ".github/instructions/tests.instructions.md",
            '---\napplyTo: "**/src/test/java/**/*.java"\n---\n\n# Tests\n',
        )
        self.write("module/src/main/java/Example.java", "class Example {}\n")
        self.write("module/src/test/java/ExampleTest.java", "class ExampleTest {}\n")
        self.write("docs/local.md", "# Local\n")
        self.write(
            ".github/skills/example/SKILL.md",
            "---\nname: example\ndescription: Example task workflow.\n---\n\n# Example\n",
        )
        self.write(
            ".agents/skills/example-bridge/SKILL.md",
            "---\nname: example-bridge\ndescription: Load the example workflow.\n---\n\n# Bridge\n",
        )
        self.write(
            ".github/workflows/refresh-agent-instructions.yml",
            """name: Refresh
on:
  workflow_dispatch:
    inputs:
      reason:
        required: true
      force_ai:
        default: false
steps:
  - id: audit
    run: python3 scripts/ci/validate_agent_setup.py
  - if: steps.audit.outputs.needs_ai == 'true'
    uses: openai/codex-action@v1
""",
        )
        self.budget = {
            "file_budgets": {
                "AGENTS.md": {"max_bytes": 1000},
                "CLAUDE.md": {"max_bytes": 1000, "max_lines": 20},
                ".github/copilot-instructions.md": {"max_chars": 3999},
            },
            "host_contexts": {
                "codex": ["AGENTS.md"],
                "claude": ["AGENTS.md", "CLAUDE.md"],
                "copilot": ["AGENTS.md", ".github/copilot-instructions.md"],
            },
            "max_estimated_tokens_per_host": 10000,
            "active_guidance_globs": [
                "AGENTS.md",
                "CLAUDE.md",
                ".agents/skills/*/SKILL.md",
                ".github/copilot-instructions.md",
                ".github/instructions/*.instructions.md",
                ".github/skills/*/SKILL.md",
            ],
            "reference_scan_globs": [
                "AGENTS.md",
                "CLAUDE.md",
                ".agents/skills/*/SKILL.md",
                ".github/copilot-instructions.md",
                ".github/instructions/*.instructions.md",
                ".github/skills/*/SKILL.md",
            ],
            "scope_files": [
                ".github/instructions/source.instructions.md",
                ".github/instructions/tests.instructions.md",
            ],
            "skills_roots": [".agents/skills", ".github/skills"],
            "duplicate_paragraph_min_chars": 80,
            "forbidden_patterns": [
                {"pattern": "(?i)before every commit", "message": "per-commit ceremony"}
            ],
            "stale_references": ["retired.md"],
            "refresh_workflow": ".github/workflows/refresh-agent-instructions.yml",
        }
        self.budget_path = self.root / "scripts/ci/agent_guidance_budget.json"
        self.write_budget()

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

    def test_valid_repository_passes(self):
        self.assertEqual(validate_repository(self.root, self.budget_path), [])

    def test_rejects_oversized_file(self):
        self.budget["file_budgets"]["AGENTS.md"]["max_bytes"] = 10
        self.write_budget()
        self.assertIn("size-budget", self.codes())

    def test_enforces_copilot_4000_character_limit(self):
        self.write(".github/copilot-instructions.md", "x" * 4000)
        self.assertIn("character-budget", self.codes())

    def test_enforces_total_guidance_reduction(self):
        self.budget["total_guidance_globs"] = ["AGENTS.md", "CLAUDE.md"]
        self.budget["reduction_baseline_bytes"] = 100
        self.budget["minimum_reduction_percent"] = 60
        self.write_budget()
        self.write("AGENTS.md", "x" * 50)
        self.assertIn("total-reduction", self.codes())

    def test_counts_skill_metadata_in_host_budget(self):
        self.budget["host_contexts"] = {"codex": ["AGENTS.md"]}
        self.budget["host_skill_metadata_globs"] = {
            "codex": [".agents/skills/*/SKILL.md"]
        }
        self.budget["max_estimated_tokens_per_host"] = 10
        self.write_budget()
        errors = validate_repository(self.root, self.budget_path)
        self.assertTrue(
            any(
                error["code"] == "host-token-budget" and error["path"] == "codex"
                for error in errors
            )
        )

    def test_rejects_duplicate_long_paragraphs(self):
        paragraph = "This duplicated instruction is deliberately long enough for deterministic detection. " * 2
        self.write("AGENTS.md", f"# Agents\n\n{paragraph}\n")
        self.write("CLAUDE.md", f"# Claude\n\n{paragraph}\n")
        self.assertIn("duplicate-paragraph", self.codes())

    def test_rejects_broken_local_reference(self):
        self.write("AGENTS.md", "# Agents\n\n[Missing](docs/missing.md)\n")
        self.assertIn("broken-reference", self.codes())

    def test_rejects_local_reference_outside_repository(self):
        self.write("AGENTS.md", "# Agents\n\n[Outside](../outside.md)\n")
        self.assertIn("reference-outside-root", self.codes())

    def test_rejects_invalid_skill_frontmatter(self):
        self.write(".github/skills/example/SKILL.md", "---\nname: wrong\n---\n\n# Example\n")
        codes = self.codes()
        self.assertIn("skill-name", codes)
        self.assertIn("skill-description", codes)

    def test_rejects_invalid_codex_skill_frontmatter(self):
        self.write(
            ".agents/skills/example-bridge/SKILL.md",
            "---\nname: wrong\n---\n\n# Bridge\n",
        )
        codes = self.codes()
        self.assertIn("skill-name", codes)
        self.assertIn("skill-description", codes)

    def test_enforces_exact_skill_set(self):
        self.budget["expected_skill_names"] = {
            ".agents/skills": ["example-bridge"],
            ".github/skills": ["example"],
        }
        self.write_budget()
        self.write(
            ".agents/skills/unexpected/SKILL.md",
            "---\nname: unexpected\ndescription: Unexpected workflow.\n---\n\n# Unexpected\n",
        )
        self.assertIn("skill-set", self.codes())

    def test_requires_valid_codex_skill_metadata(self):
        self.budget["skill_budgets"] = {
            ".agents/skills": {
                "max_description_chars": 100,
                "max_body_chars": 100,
                "require_openai_yaml": True,
            }
        }
        self.write_budget()
        self.assertIn("skill-metadata", self.codes())

        self.write(
            ".agents/skills/example-bridge/agents/openai.yaml",
            """interface:
  display_name: "Example Bridge"
  short_description: "Load the canonical example workflow"
  default_prompt: "Use $example-bridge to handle this example task."
policy:
  allow_implicit_invocation: true
""",
        )
        self.assertNotIn("skill-metadata", self.codes())

    def test_rejects_unmatched_path_scope(self):
        self.write(
            ".github/instructions/source.instructions.md",
            '---\napplyTo: "**/src/missing/**/*.java"\n---\n\n# Source\n',
        )
        self.assertIn("unmatched-scope", self.codes())

    def test_rejects_costly_mandate(self):
        self.write("AGENTS.md", "# Agents\n\nRun the build before every commit.\n")
        self.assertIn("forbidden-mandate", self.codes())

    def test_rejects_scheduled_or_ungated_paid_refresh(self):
        self.write(
            ".github/workflows/refresh-agent-instructions.yml",
            """on:
  workflow_dispatch:
  schedule:
    - cron: '0 0 * * 1'
""",
        )
        self.assertIn("refresh-workflow", self.codes())

    def test_current_repository_configuration_is_valid(self):
        repository_root = Path(__file__).resolve().parents[2]
        self.assertEqual(validate_repository(repository_root), [])


if __name__ == "__main__":
    unittest.main()
