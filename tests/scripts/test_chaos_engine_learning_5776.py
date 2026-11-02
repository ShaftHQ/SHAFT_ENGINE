"""Contract tests for ChaosEngine learning ticket #5776."""

from __future__ import annotations

import importlib.util
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
PLAYBOOK = ROOT / "chaos-engine/references/work-github-playbook.md"
GOTCHA = (
    ROOT
    / ".memory/memory/gotchas/"
    / "keep-generated-assets-inventory-in-sync-when-memory-gitignore-policy-changes.md"
)
VALIDATOR = ROOT / "scripts/ci/validate_chaos_engine_readme.py"
README = ROOT / "chaos-engine/README.md"


def _load_validator():
    spec = importlib.util.spec_from_file_location("vcr", VALIDATOR)
    module = importlib.util.module_from_spec(spec)
    assert spec.loader is not None
    spec.loader.exec_module(module)
    return module


class Learning5776Test(unittest.TestCase):
    def test_playbook_requires_inventory_sync_with_memory_gitignore_changes(self) -> None:
        text = PLAYBOOK.read_text(encoding="utf-8")
        self.assertIn("generated-assets", text)
        self.assertIn("validate_chaos_engine_readme.py", text)
        self.assertIn("Memory tracking policy", text)

    def test_gotcha_and_validator_anchor_issue_5776(self) -> None:
        self.assertTrue(GOTCHA.is_file(), GOTCHA)
        self.assertIn("#5776", GOTCHA.read_text(encoding="utf-8"))
        self.assertIn("#5776", VALIDATOR.read_text(encoding="utf-8"))

    def test_readme_generated_assets_matches_validator_inventory(self) -> None:
        module = _load_validator()
        sections = module.inventory_sections(ROOT)
        table = sections["generated-assets"]
        self.assertIn("durable Memory tracked", table)
        readme = README.read_text(encoding="utf-8")
        # README embeds the inventory markers; require the Memory row phrasing.
        self.assertIn("durable Memory tracked; derived indexes untracked", readme)
        start = "<!-- inventory:generated-assets:start -->"
        end = "<!-- inventory:generated-assets:end -->"
        self.assertIn(start, readme)
        self.assertIn(end, readme)
        begin = readme.index(start) + len(start)
        finish = readme.index(end, begin)
        embedded = readme[begin:finish].strip()
        self.assertEqual(table.strip(), embedded)


if __name__ == "__main__":
    unittest.main()
