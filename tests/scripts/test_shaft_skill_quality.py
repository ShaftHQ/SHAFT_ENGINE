"""Generated SHAFT skill inventory and quality-contract tests (#4640)."""

import importlib.util
import json
import re
import tempfile
import unittest
from pathlib import Path
from unittest.mock import patch

import sys
sys.path.insert(0, str(Path(__file__).resolve().parents[2]))  # noqa: E402 - self-locating under python3 -I (#7197)
sys.path.insert(0, str(Path(__file__).resolve().parents[2] / "scripts" / "ci"))  # noqa: E402 - bare sibling imports in scripts/ci
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
        self.assertEqual(
            (
                "tests.scripts.test_shaft_skill_quality",
                "tests.scripts.test_execution_class_skills",
            ),
            quality.modules,
        )

    def assertDefect(self, code: str):
        defects = validate_repository(self.root)
        self.assertTrue(any(code in defect for defect in defects), defects)


MAP = (
    Path(__file__).resolve().parents[2]
    / "shaft-skills"
    / "shaft-test-case-design"
    / "references"
    / "journey-coverage-map.md"
)
MAP_POINTERS = (
    Path(__file__).resolve().parents[2] / "shaft-skills" / "shaft-test-case-design" / "SKILL.md",
    Path(__file__).resolve().parents[2]
    / "shaft-skills"
    / "shaft-requirements-analysis"
    / "references"
    / "playbook.md",
    Path(__file__).resolve().parents[2]
    / "shaft-skills"
    / "shaft-automated-test-authoring"
    / "references"
    / "playbook.md",
    Path(__file__).resolve().parents[2] / "shaft-skills" / "shaft-test-planning" / "references" / "playbook.md",
    Path(__file__).resolve().parents[2]
    / "shaft-skills"
    / "shaft-stakeholder-reporting"
    / "references"
    / "playbook.md",
)


class JourneyCoverageMapTest(unittest.TestCase):
    def test_shipped_map_traces_steps_and_tracks_status(self):
        text = MAP.read_text(encoding="utf-8")
        hierarchy = text.lower()
        self.assertLess(
            hierarchy.index("a journey, then a scenario, then a numbered step"),
            hierarchy.index("## coverage matrix"),
        )
        for field in (
            "Preconditions",
            "Test data",
            "Platforms",
            "API endpoints",
            "Source refs",
            "Dependencies",
        ):
            self.assertIn(field, text)
        self.assertIn("Every item traces to a source", text)
        self.assertIn("A gap stays unsourced", text)
        self.assertIn("## Coverage matrix", text)
        self.assertIn("in code, not in plan", text)
        self.assertIn("in plan, not in design", text)
        self.assertIn("## Reconciliation", text)
        self.assertIn("by screen state, not by control id or keyword", text)
        for side_effect in ("SAFE", "STOP-BEFORE", "DESIGN-ONLY"):
            self.assertIn(side_effect, text)
        self.assertIn("Exactly one status per row", text)
        for status in ("Done", "In progress", "Blocked", "Not started"):
            self.assertIn(status, text)
        self.assertIn("### Roll-up", text)
        self.assertIn("Totals per status", text)
        self.assertIn("by-design rows excluded", text)
        self.assertIn("### Who updates it and when", text)
        self.assertIn("same pull request", text)
        self.assertIn("one flowchart per journey", text)
        self.assertLess(text.index("mmdc"), text.index("--no-sandbox"))
        self.assertIn("@mermaid-js/mermaid-cli", text)

    def test_design_authoring_and_reporting_skills_point_at_the_map(self):
        for path in MAP_POINTERS:
            content = path.read_text(encoding="utf-8")
            self.assertIn("journey-coverage-map.md", content, path.name)


REPOSITORY = Path(__file__).resolve().parents[2]
TOOL_INDEX = (
    REPOSITORY
    / "shaft-mcp/src/main/resources/META-INF/shaft-mcp/tool-index.json"
)
CACHE_RULE = (
    "Compact cache rule: name, when to use, and essential arguments only. "
    "Do not embed a JSON schema. Load this file only when the shaft skills "
    "entrypoint selects this skill."
)
SKILL_NAME = re.compile(r"^shaft-[a-z0-9]+(?:-[a-z0-9]+)*$")
HELP_INSTRUCTION = re.compile(r"(?i)\b(do not|never|don't)\b")


def _inventory_names() -> list[str]:
    inventory = (REPOSITORY / "shaft-skills/INVENTORY.md").read_text(encoding="utf-8")
    return re.findall(r"(?m)^\| (shaft-[a-z0-9-]+) \|", inventory)


def _entrypoint_text() -> str:
    hub = REPOSITORY / "shaft-skills/shaft-developer/SKILL.md"
    text = hub.read_text(encoding="utf-8")
    routing = next(
        hub.parent / target
        for target in re.findall(r"\[[^\]]+\]\(([^)]+)\)", text)
        if target.split("#", 1)[0].endswith("routing.md")
    )
    return text + "\n" + routing.read_text(encoding="utf-8")


def _cache_files() -> list[tuple[str, Path]]:
    found = []
    for skill in sorted((REPOSITORY / "shaft-skills").glob("*/SKILL.md")):
        body = skill.read_text(encoding="utf-8")
        if "references/cache.md" not in body:
            continue
        cache = skill.parent / "references/cache.md"
        heading = ""
        if cache.is_file():
            heading = cache.read_text(encoding="utf-8").splitlines()[0]
        found.append((heading, cache))
    return found


class ShaftSkillsOptimization7858Test(unittest.TestCase):
    """Issue #7858: one entrypoint, two compact on-demand tool caches, one mindmap."""

    def test_entrypoint_names_every_generated_inventory_skill(self):
        names = _inventory_names()
        self.assertGreaterEqual(len(names), 30)
        blob = _entrypoint_text()
        missing = [name for name in names if name not in blob]
        self.assertEqual(missing, [])

    def test_skill_names_match_the_shaft_scheme(self):
        for name in _inventory_names():
            self.assertRegex(name, SKILL_NAME)

    def test_exactly_one_mcp_cache_and_one_cli_cache(self):
        kinds = []
        for heading, cache in _cache_files():
            self.assertTrue(cache.is_file(), cache)
            text = cache.read_text(encoding="utf-8")
            self.assertIn(CACHE_RULE, text)
            self.assertNotIn("inputSchema", text)
            self.assertNotIn('"properties"', text)
            self.assertNotIn('"type":', text)
            if heading == "# MCP cache":
                kinds.append("mcp")
            elif heading == "# CLI cache":
                kinds.append("cli")
            else:
                self.fail(f"unexpected cache heading {heading!r} in {cache}")
        self.assertEqual(kinds, ["cli", "mcp"])

    def test_mcp_cache_recovers_index_names_without_schemas(self):
        index = json.loads(TOOL_INDEX.read_text(encoding="utf-8"))
        names = [tool["name"] for tool in index["tools"]]
        self.assertGreaterEqual(len(names), 100)
        cache = (
            REPOSITORY / "shaft-skills/shaft-mcp/references/cache.md"
        ).read_text(encoding="utf-8")
        self.assertIn(names[0], cache)
        missing = [name for name in names if f"`{name}`" not in cache]
        extra = re.findall(r"(?m)^\| `([a-z][a-z0-9_]*)` \|", cache)
        self.assertEqual(missing, [])
        self.assertEqual(sorted(extra), sorted(names))
        self.assertLess(len(cache.encode("utf-8")), TOOL_INDEX.stat().st_size // 2)

    def test_cli_cache_recovers_a_catalog_command_without_help_discovery(self):
        reference = (
            REPOSITORY / "shaft-skills/references/shaft-cli-commands.md"
        ).read_text(encoding="utf-8")
        form = re.search(r"shaft-cli call <TOOL>[^\n`]*", reference)
        self.assertIsNotNone(form)
        cache_path = REPOSITORY / "shaft-skills/shaft-cli/references/cache.md"
        skill = (REPOSITORY / "shaft-skills/shaft-cli/SKILL.md").read_text(encoding="utf-8")
        cache = cache_path.read_text(encoding="utf-8")
        self.assertIn(form.group(0).strip(), cache)
        self.assertIn("references/cache.md", skill)
        for label, text in (("skill", skill), ("cache", cache)):
            instructed = [
                line
                for line in text.splitlines()
                if "--help" in line and not HELP_INSTRUCTION.search(line)
            ]
            self.assertEqual(instructed, [], label)

    def test_readme_mindmap_names_each_skill_and_the_relation(self):
        skills = REPOSITORY / "shaft-skills"
        readmes = [path.name for path in skills.iterdir() if path.name.casefold() == "readme.md"]
        self.assertEqual(readmes, ["README.md"])
        readme = (skills / "README.md").read_text(encoding="utf-8")
        self.assertIn("mindmap", readme.casefold())
        self.assertIn("selects exactly one", readme.casefold())
        for name in _inventory_names():
            self.assertIn(name, readme)
        self.assertIn("Allure failure analysis", readme)
        self.assertIn("Record and playback", readme)
        self.assertIn("Code generation", readme)

    def test_on_demand_caches_keep_the_full_when_text(self):
        generator = REPOSITORY / "scripts/mcp/generate_shaft_skill_tool_caches.py"
        spec = importlib.util.spec_from_file_location(
            "generate_shaft_skill_tool_caches_7858", generator
        )
        module = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(module)
        index = json.loads(TOOL_INDEX.read_text(encoding="utf-8"))
        caches = [
            path.read_text(encoding="utf-8")
            for path in (
                REPOSITORY / "shaft-skills/shaft-mcp/references/cache.md",
                REPOSITORY / "shaft-skills/shaft-cli/references/cache.md",
            )
        ]
        for tool in index["tools"]:
            description = str(tool.get("description") or "")
            when = module._when(description)
            self.assertEqual(
                when,
                description.replace("|", "/").replace("`", "'").replace("\n", " ").strip(),
            )
            row = f"| `{tool['name']}` | {when} |"
            for cache in caches:
                self.assertIn(row, cache)

    def test_description_listing_and_progressive_disclosure_budgets_hold(self):
        self.assertEqual(validate_repository(REPOSITORY), [])


if __name__ == "__main__":
    unittest.main()
