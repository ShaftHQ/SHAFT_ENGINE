"""Source-derived ChaosEngine README inventory contracts (#5307)."""

from __future__ import annotations

import shutil
import tempfile
import unittest
from pathlib import Path

from scripts.ci import validate_chaos_engine_readme as readme_owner
from scripts.ci.validate_chaos_engine_readme import inventory_sections, validate


ROOT = Path(__file__).resolve().parents[2]


class ChaosEngineReadmeInventoryTest(unittest.TestCase):
    def test_write_generated_refreshes_inventory_and_second_run_is_clean(self):
        write_generated = getattr(readme_owner, "write_generated", None)
        self.assertTrue(callable(write_generated))
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            shutil.copytree(ROOT / "chaos-engine", root / "chaos-engine")
            readme = root / "chaos-engine/README.md"
            original = readme.read_text(encoding="utf-8")
            readme.write_text(
                original.replace("| uv |", "| ux |", 1),
                encoding="utf-8",
            )
            self.assertTrue(any("managed-dependencies" in error for error in validate(root)))

            write_generated(root)
            self.assertEqual([], validate(root))
            after_first = readme.read_text(encoding="utf-8")
            write_generated(root)
            self.assertEqual(after_first, readme.read_text(encoding="utf-8"))

    def test_repository_readme_matches_every_source_derived_inventory(self):
        self.assertEqual([], validate(ROOT))
        sections = inventory_sections(ROOT)

        self.assertIn("uv==0.11.29", sections["managed-dependencies"])
        self.assertIn("tree-sitter-sql==0.3.11", sections["managed-dependencies"])
        for skill in ("chaos-engine", "caveman", "ponytail", "local-coding-delegate", "omniroute", "work-item"):
            self.assertIn(skill, sections["skills"])
        for host in ("codex", "claude", "gemini", "grok", "copilot"):
            self.assertIn(f"| {host} |", sections["hosts"])
        for event in ("SessionStart", "PreToolUse", "Stop", "SessionEnd"):
            self.assertIn(f"| {event} |", sections["lifecycle-events"])
        self.assertIn("probe_hardware.py", sections["python-libraries"])
        self.assertIn("| platform |", sections["python-libraries"])

    def test_unknown_skill_and_one_changed_inventory_byte_fail_closed(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            shutil.copytree(ROOT / "chaos-engine", root / "chaos-engine")
            extra = root / "chaos-engine/skills/untracked-capability/SKILL.md"
            extra.parent.mkdir(parents=True)
            extra.write_text(
                "---\nname: untracked-capability\ndescription: Newly declared skill.\n---\n",
                encoding="utf-8",
            )
            errors = validate(root)
            self.assertTrue(any("skills" in error for error in errors), errors)

            extra.unlink()
            extra.parent.rmdir()
            readme = root / "chaos-engine/README.md"
            content = readme.read_text(encoding="utf-8").replace("| uv |", "| ux |", 1)
            readme.write_text(content, encoding="utf-8")
            byte_errors = validate(root)
            self.assertTrue(
                any("managed-dependencies" in error for error in byte_errors),
                byte_errors,
            )

    def test_missing_mermaid_flow_is_reported(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            shutil.copytree(ROOT / "chaos-engine", root / "chaos-engine")
            readme = root / "chaos-engine/README.md"
            content = readme.read_text(encoding="utf-8").replace(
                "accTitle: Rollback flow", "accTitle: Removed rollback flow", 1
            )
            readme.write_text(content, encoding="utf-8")

            self.assertTrue(any("Rollback flow" in error for error in validate(root)))

    def test_new_packaged_python_source_cannot_disappear_from_inventory(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            shutil.copytree(ROOT / "chaos-engine", root / "chaos-engine")
            (root / "chaos-engine/new_runtime.py").write_text(
                "import fractions\n", encoding="utf-8"
            )

            errors = validate(root)

            self.assertTrue(any("python-libraries" in error for error in errors), errors)

    def test_malformed_mermaid_edge_is_rejected_by_the_parser(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            shutil.copytree(ROOT / "chaos-engine", root / "chaos-engine")
            readme = root / "chaos-engine/README.md"
            content = readme.read_text(encoding="utf-8").replace(
                "Intent[Write rollback intent] --> Previous[Authenticate prior core and hosts]",
                "Intent[[Write rollback intent] --> Previous[Authenticate prior core and hosts]",
                1,
            )
            readme.write_text(content, encoding="utf-8")

            self.assertTrue(any("Mermaid syntax" in error for error in validate(root)))


if __name__ == "__main__":
    unittest.main()
